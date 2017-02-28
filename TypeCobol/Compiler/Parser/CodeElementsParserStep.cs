﻿using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Linq;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// Incrementally parse CodeElements from a set of pre-processed tokens changes
    /// </summary>
    static class CodeElementsParserStep
    {
        /// <summary>
        /// Initial parsing of a complete document
        /// </summary>
        internal static void ParseDocument(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> documentLines, TypeCobolOptions compilerOptions)
        {
            ParseProcessedTokensLinesChanges(textSourceInfo, documentLines, null, null, compilerOptions);
        }

        /// <summary>
        /// Represents a section of code we need to re-parse in search of new code elements
        /// </summary>
        private class ParseSection
        {
            public ParseSection()
            { }

            public ParseSection(int startLineIndex, Token startToken, int stopLineIndex, Token stopToken, bool stopTokenIsLastTokenOfTheLine)
            {
                StartLineIndex = startLineIndex;
                StartToken = startToken;
                StopLineIndex = stopLineIndex;
                StopToken = stopToken;
                StopTokenIsFirstTokenOfTheLine = stopTokenIsLastTokenOfTheLine;
            }

            public int StartLineIndex { get; set; }
            public Token StartToken { get; set; }
            public int StopLineIndex { get; set; }
            public Token StopToken { get; set; }
            public bool StopTokenIsFirstTokenOfTheLine { get; set; }
        }

        /// <summary>
        /// Incremental parsing of a set of processed tokens lines changes
        /// </summary>
        internal static IList<DocumentChange<ICodeElementsLine>> ParseProcessedTokensLinesChanges(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> documentLines, IList<DocumentChange<IProcessedTokensLine>> processedTokensLinesChanges, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions)
        {
            // Collect all changes applied to the processed tokens lines during the incremental scan
            IList<DocumentChange<ICodeElementsLine>> codeElementsLinesChanges = new List<DocumentChange<ICodeElementsLine>>();

            // There are 2 reasons to re-parse a tokens line after a change :
            // 1. The tokens line changed : these lines were already reset during the previous steps
            // 2. If a tokens line that changed was involved in the parsing of a multiline code element, the whole group of lines must be parsed again

            // --- PREPARATION PHASE : identify all parse sections where code elements need to be refreshed ---

            IList<ParseSection> refreshParseSections = null;

            // Iterate over all processed tokens changes detected by the PreprocessorStep :
            // - refresh all the adjacent lines participating in a CodeElement
            // - register the start and stop token for all sections of the document which need to be parsed again
            if (processedTokensLinesChanges != null)
            {
                // If the document was cleared, everything must be parsed again
                if (processedTokensLinesChanges[0].Type != DocumentChangeType.DocumentCleared)
                {
                    refreshParseSections = new List<ParseSection>();

                    ParseSection lastParseSection = null;
                    foreach (DocumentChange<IProcessedTokensLine> tokensChange in processedTokensLinesChanges)
                    {
                        if (lastParseSection == null || tokensChange.LineIndex > lastParseSection.StopLineIndex)
                        {
                            lastParseSection = CheckIfAdjacentLinesNeedRefresh(tokensChange.Type, tokensChange.LineIndex, documentLines, prepareDocumentLineForUpdate, codeElementsLinesChanges, lastParseSection);
                            refreshParseSections.Add(lastParseSection);
                        }
                    }
                }
            }

            // --- INITIALIZE ANTLR CodeElements parser ---

            // Create a token iterator on top of pre-processed tokens lines
            ITokensLinesIterator tokensIterator = ProcessedTokensDocument.GetProcessedTokensIterator(textSourceInfo, documentLines);

            // Create an Antlr compatible token source on top of the token iterator
            TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                textSourceInfo.Name,
                tokensIterator);

            // Init parser
            TokensLinesTokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
            CodeElementsParser cobolParser = new CodeElementsParser(tokenStream);
            // REVERT TO STD PARSER ==> TracingCobolParser cobolParser = new TracingCobolParser(tokenStream);
            
            // Customize error recovery strategy
            IAntlrErrorStrategy cobolErrorStrategy = new CodeElementErrorStrategy();
            cobolParser.ErrorHandler = cobolErrorStrategy;

            // Register all parse errors in a list in memory
            ParserDiagnosticErrorListener errorListener = new ParserDiagnosticErrorListener();
            cobolParser.RemoveErrorListeners();
            cobolParser.AddErrorListener(errorListener);

            // Prepare to analyze the parse tree
            ParseTreeWalker walker = new ParseTreeWalker();
            CodeElementBuilder codeElementBuilder = new CodeElementBuilder();
            codeElementBuilder.Dispatcher = new CodeElementDispatcher();
            codeElementBuilder.Dispatcher.CreateListeners();

            // --- INCREMENTAL PARSING ---

            // In case of incremental parsing, parse only the code sections we need to refresh
            IEnumerator<ParseSection> parseSectionsEnumerator = null;
            ParseSection currentParseSection = null;
            if (refreshParseSections != null)
            {
                // Get the first code section we need to refresh
                parseSectionsEnumerator = refreshParseSections.GetEnumerator();
                parseSectionsEnumerator.MoveNext();
                currentParseSection = parseSectionsEnumerator.Current;

                // Seek just before the next code element starting token
                tokenStream.SeekToToken(currentParseSection.StartToken);
                tokenStream.StartLookingForStopToken(currentParseSection.StopToken);
            }

            // Parse a list of code elements for each parse section while advancing in the underlying token stream
            do
            {
                // Reset parsing error diagnostics
                cobolErrorStrategy.Reset(cobolParser);

                // Try to parse code elements :
                // - starting with the current parse section Start token
                // - ending with the current parse section Stop token
                CodeElementsParser.CobolCodeElementsContext codeElementsParseTree = null;
                try {
                    codeElementsParseTree = cobolParser.cobolCodeElements();
                } catch (Exception e) {
                    var currentToken = (Token)cobolParser.CurrentToken;
                    CodeElementsLine codeElementsLine = GetCodeElementsLineForToken(currentToken);
                    codeElementsLine.AddParserDiagnostic(new TokenDiagnostic(MessageCode.ImplementationError, currentToken, currentToken.Line, e));
                }

                if (codeElementsParseTree != null) {
                    // If the parse tree is not empty
                    if (codeElementsParseTree.codeElement() != null && codeElementsParseTree.codeElement().Length > 0)
                    {
                        // Analyze the parse tree for each code element
                        foreach (var codeElementParseTree in codeElementsParseTree.codeElement())
                        {
                            // Get the first line that was parsed     
                            var tokenStart = (Token)codeElementParseTree.Start;
                            CodeElementsLine codeElementsLine = GetCodeElementsLineForToken(tokenStart);

                            // Register that this line was updated
                            // COMMENTED FOR THE SAKE OF PERFORMANCE -- SEE ISSUE #160
                            //int updatedLineIndex = documentLines.IndexOf(codeElementsLine, codeElementsLine.InitialLineIndex);
                            //codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, updatedLineIndex, codeElementsLine));
                            codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, codeElementsLine.InitialLineIndex, codeElementsLine));

                            // Visit the parse tree to build a first class object representing the code elements
                            try { walker.Walk(codeElementBuilder, codeElementParseTree); }
                            catch (Exception ex)
                            {
                                var code = MessageCode.ImplementationError;
                                int line = 0; int start = 0; int stop = 0;
                                if (codeElementsLine.SourceTokens != null && codeElementsLine.SourceTokens.Count > 0)
                                {
                                    start = codeElementsLine.SourceTokens[0].StartIndex;
                                    stop = codeElementsLine.SourceTokens[codeElementsLine.SourceTokens.Count - 1].StopIndex;
                                }
                                codeElementsLine.AddParserDiagnostic(new ParserDiagnostic(ex.ToString(), start, stop, line, null, code));
                            }
                            CodeElement codeElement = codeElementBuilder.CodeElement;
                            if (codeElement != null)
                            {
                                // Attach consumed tokens and main document line numbers information to the code element
                                if (codeElement.ConsumedTokens.Count == 0)
                                {// ISSUE #204:
                                    if (tokenStream.Lt(1) != null)
                                    {// if not end of file,
                                        // add next token to ConsumedTokens to know where is the CodeElement in error
                                        codeElement.ConsumedTokens.Add((Token)tokenStream.Lt(1));
                                        // this alter CodeElements semantics: in addition to matched tokens,
                                        // it includes the first token in error if no token has been matched
                                    }
                                }

                                //TODO Issue #384 to discuss if this code should stay here:
                                //This should be in a Checker, but "codeElement.ConsumedTokens" is only set after all the checkers have been called
                                //Rule TCLIMITATION_NO_CE_ACROSS_SOURCES
                                if (codeElement.IsAcrossSourceFile())
                                {
                                    DiagnosticUtils.AddError(codeElement, "A Cobol statement cannot be across 2 sources files (eg. Main program and a COPY)", MessageCode.TypeCobolParserLimitation);
                                }
                            
                                // Add code element to the list                    
                                codeElementsLine.AddCodeElement(codeElement);
                                if (codeElement.Diagnostics != null)
                                {
                                    foreach (Diagnostic d in codeElement.Diagnostics)
                                    {
                                        codeElementsLine.AddParserDiagnostic(d);
                                    }
                                }
                            }
                        }
                    }
                    // If the parse tree contains errors
                    if (codeElementsParseTree.Diagnostics != null)
                    {
                        foreach (ParserDiagnostic d in codeElementsParseTree.Diagnostics)
                        {
                            if (d.OffendingSymbol != null)
                            {
                                CodeElementsLine codeElementsLine = GetCodeElementsLineForToken((Token)d.OffendingSymbol);
                                codeElementsLine.AddParserDiagnostic(d);
                            }
                        }
                    }
                }

                // In case of incremental parsing, directly jump to next parse section in the token stream
                // Else, simply start parsing the next CodeElement beginning with the next token
                if (currentParseSection != null)
                {
                    // Adavance to the next ParseSection
                    if (parseSectionsEnumerator.MoveNext())
                    {
                        currentParseSection = parseSectionsEnumerator.Current;
                        tokenStream.SeekToToken(currentParseSection.StartToken);
                        tokenStream.StartLookingForStopToken(currentParseSection.StopToken);
                    }
                    // No more section to parse
                    else
                    {
                        break;
                    }
                }
            }
            while (tokenStream.La(1) >= 0);

            return codeElementsLinesChanges; 
        }

        private static CodeElementsLine GetCodeElementsLineForToken(Token tokenStart)
        {
            CodeElementsLine codeElementsLine;
            var importedToken = tokenStart as ImportedToken;
            if (importedToken != null)
            {
                codeElementsLine = (CodeElementsLine)importedToken.CopyDirective.TextNameSymbol.TokensLine;
            }
            else
            {
                codeElementsLine = ((CodeElementsLine)tokenStart.TokensLine);
            }

            return codeElementsLine;
        }


        /// <summary>
        /// Illustration : lines with code elements continued from previous line or with a continuation on next line (before update)
        /// SW represents a code element starting word
        /// [  SW    x] 
        /// [        x]
        /// [SW      x]
        /// [   SW    ]
        /// [   x   SW]
        /// A DocumentChange intersects with a previously parsed multiline code element if : 
        /// * LineInserted :
        ///   - all previous lines until the last starting word 
        ///     (get previous lines until the previous code element is found)
        ///   - all the next lines until the next starting word
        ///     (get next lines until the next code element is found, 
        ///      do not reset the last line if the next code element starts at the beginning of the line)
        /// * LineUpdated / LineRemoved :
        ///   - all previous lines until the last starting word 
        ///     (get previous lines until the previous code element is found)
        ///   - all the next lines until the next starting word
        ///     (get next lines until the next code element is found, 
        ///      do not reset the last line if the next code element starts at the beginning of the line)
        /// When navigating to previous or next line searching for a code element, we can stop when a fresh insertion / update is encountered.
        /// When we reset a line which was not directly updated, and where the code element started in the middle of the line,
        /// we must "remember" at which token we must start parsing.
        /// In conclusion, the incremental parsing step for code elements is divided in two steps :
        /// 1. List all the starting and stop tokens of sections to parse with the rules above
        /// 2. Parse code elements beginning with starting token and until we reach stop token
        /// </summary>
        private static ParseSection CheckIfAdjacentLinesNeedRefresh(DocumentChangeType changeType, int lineIndex, ISearchableReadOnlyList<CodeElementsLine> documentLines, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, IList<DocumentChange<ICodeElementsLine>> codeElementsLinesChanges, ParseSection lastParseSection)
        {
            ParseSection currentParseSection = new ParseSection();

            // Navigate backwards to the start of the multiline code element
            if (lineIndex > 0)
            {
                int previousLineIndex = lineIndex;
                int lastLineIndexReset = lastParseSection != null ? lastParseSection.StopLineIndex  : - 1;
                IEnumerator<CodeElementsLine> reversedEnumerator = documentLines.GetEnumerator(previousLineIndex - 1, -1, true);
                bool previousLineHasCodeElements = false;
                while (reversedEnumerator.MoveNext() && (--previousLineIndex > lastLineIndexReset))
                {
                    // Get the previous line until the first code element is encountered
                    CodeElementsLine previousLine = reversedEnumerator.Current;

                    // The start of the parse section is delimited by the previous CodeElement
                    previousLineHasCodeElements = previousLine.HasCodeElements;
                    if (previousLineHasCodeElements)
                    {
                        currentParseSection.StartLineIndex = previousLineIndex;
                        currentParseSection.StartToken = previousLine.CodeElements[0].ConsumedTokens[0];
                    }
                    
                    // All lines contained in the parse section could be modified, and should be reset
                    previousLine = (CodeElementsLine)prepareDocumentLineForUpdate(previousLineIndex, previousLine, CompilationStep.CodeElementsParser);
                    previousLine.ResetCodeElements();
                    codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, previousLineIndex, previousLine));
                                        
                    // Stop iterating backwards as soon as the start of an old CodeElement is found                   
                    if (previousLineHasCodeElements)
                    {
                        break;
                    }
                }
                // If no CodeElement was found on the previous lines, current parse section starts at the beggining of the file
                // (because last parseSection could only stop at a CodeElement or at the end of the file)
                if (!previousLineHasCodeElements)
                {
                    currentParseSection.StartLineIndex = 0;
                    currentParseSection.StartToken = null;
                }
            }
            // If line 0 was updated, current parse section starts at the beggining of the file
            else
            {                
                currentParseSection.StartLineIndex = 0;
                currentParseSection.StartToken = null;
            }

            // Navigate forwards to the end of the multiline code element
            if (lineIndex < (documentLines.Count - 1))
            {
                int nextLineIndex = lineIndex;
                IEnumerator<CodeElementsLine> enumerator = documentLines.GetEnumerator(nextLineIndex + 1, -1, false);
                bool nextLineHasCodeElements = false;
                while (enumerator.MoveNext())
                {
                    // Get the next line until non continuation line is encountered
                    nextLineIndex++;
                    CodeElementsLine nextLine = enumerator.Current;

                    // Check if the next CodeElement found starts at the beginning of the line   
                    nextLineHasCodeElements = nextLine.HasCodeElements;
                    bool nextCodeElementStartsAtTheBeginningOfTheLine = false;
                    if (nextLineHasCodeElements)
                    {                        
                        Token startTokenForNextParseSection = nextLine.CodeElements[0].ConsumedTokens[0];
                        Token firstSourceTokenOfThisLine = nextLine.TokensWithCompilerDirectives.First(token => token.Channel == Token.CHANNEL_SourceTokens);
                        nextCodeElementStartsAtTheBeginningOfTheLine = startTokenForNextParseSection == firstSourceTokenOfThisLine;                        
                    }

                    // All lines contained in the parse section could be modified
                    if (!nextCodeElementStartsAtTheBeginningOfTheLine)
                    {
                        // TO DO : ERROR below, will not work if we have source tokens from previous CodeElement + one other CodeElement on the same line
                        // => the other CodeElement will be deleted by prepareDocumentLineForUpdate and not parsed again
                        nextLine = (CodeElementsLine)prepareDocumentLineForUpdate(nextLineIndex, nextLine, CompilationStep.CodeElementsParser);
                        codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, nextLineIndex, nextLine));
                    }

                    // Stop iterating forwards as soon as the start of an old CodeElement is found                   
                    if (nextLineHasCodeElements)
                    {
                        currentParseSection.StopLineIndex = nextLineIndex;
                        currentParseSection.StopToken = nextLine.CodeElements[0].ConsumedTokens[0];
                        currentParseSection.StopTokenIsFirstTokenOfTheLine = nextCodeElementStartsAtTheBeginningOfTheLine;
                        break;
                    }                   
                }
                // If no CodeElement was found on the next lines, current parse section current parse section ends at the end of the file
                if(!nextLineHasCodeElements)
                {
                    currentParseSection.StopLineIndex = nextLineIndex;
                    currentParseSection.StopToken = null;
                    currentParseSection.StopTokenIsFirstTokenOfTheLine = false;
                }
            }
            // If last line was updated, or if no CodeElement was found after the updated line, current parse section ends at the end of the file
            else
            {
               currentParseSection.StopLineIndex = documentLines.Count - 1;
               currentParseSection.StopToken = null;
               currentParseSection.StopTokenIsFirstTokenOfTheLine = false;
            }        

            return currentParseSection;
        }
    }
}
