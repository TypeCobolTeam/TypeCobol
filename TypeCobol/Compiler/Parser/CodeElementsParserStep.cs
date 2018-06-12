using Antlr4.Runtime;
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
        internal static void ParseDocument(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> documentLines, TypeCobolOptions compilerOptions, PerfStatsForParserInvocation perfStatsForParserInvocation)
        {
            ParseProcessedTokensLinesChanges(textSourceInfo, documentLines, null, null, compilerOptions, perfStatsForParserInvocation);
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

        // When not null, optionnaly used to gather Antlr performance profiling information
        public static AntlrPerformanceProfiler AntlrPerformanceProfiler;

        /// <summary>
        /// Incremental parsing of a set of processed tokens lines changes
        /// </summary>
        internal static IList<DocumentChange<ICodeElementsLine>> ParseProcessedTokensLinesChanges(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<CodeElementsLine> documentLines, IList<DocumentChange<IProcessedTokensLine>> processedTokensLinesChanges, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions, PerfStatsForParserInvocation perfStatsForParserInvocation)
        {
            // Collect all changes applied to the processed tokens lines during the incremental scan
            IList<DocumentChange<ICodeElementsLine>> codeElementsLinesChanges = new List<DocumentChange<ICodeElementsLine>>();

            // There are 2 reasons to re-parse a tokens line after a change :
            // 1. The tokens line changed : these lines were already reset during the previous steps
            // 2. If a tokens line that changed was involved in the parsing of a multiline code element, the whole group of lines must be parsed again

            // --- PREPARATION PHASE : identify all parse sections where code elements need to be refreshed ---

            IList<ParseSection> refreshParseSections = null;
            ParseSection largestRefreshParseSection = null;

            // Iterate over all processed tokens changes detected by the PreprocessorStep :
            // - refresh all the adjacent lines participating in a CodeElement
            // - register the start and stop token for all sections of the document which need to be parsed again
            if (processedTokensLinesChanges != null && processedTokensLinesChanges.Count > 0)
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
            if (refreshParseSections != null)
            { 
                //After getting all the parts refreshed, get the largest part that has been refreshed
                var minParseSection = refreshParseSections.OrderBy(p => p.StartLineIndex).First();
                var maxParseSection = refreshParseSections.OrderByDescending(p => p.StopLineIndex).First();
                largestRefreshParseSection = new ParseSection(minParseSection.StartLineIndex,
                    minParseSection.StartToken, maxParseSection.StopLineIndex, maxParseSection.StopToken,
                    maxParseSection.StopTokenIsFirstTokenOfTheLine);
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

            // Optionnaly activate Antlr Parser performance profiling
            // WARNING : use this in a single-treaded context only (uses static field)     
            if (AntlrPerformanceProfiler == null && perfStatsForParserInvocation.ActivateDetailedAntlrPofiling) AntlrPerformanceProfiler = new AntlrPerformanceProfiler(cobolParser);
            if (AntlrPerformanceProfiler != null)
            {
                // Replace the generated parser by a subclass which traces all rules invocations
                cobolParser = new CodeElementsTracingParser(tokenStream);

                var tokensCountIterator = ProcessedTokensDocument.GetProcessedTokensIterator(textSourceInfo, documentLines);
                AntlrPerformanceProfiler.BeginParsingFile(textSourceInfo, tokensCountIterator);
            }

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

            if (largestRefreshParseSection != null)
            {
                // Seek just before the next code element starting token
                tokenStream.SeekToToken(largestRefreshParseSection.StartToken);
                tokenStream.StartLookingForStopToken(largestRefreshParseSection.StopToken);

                //Remove all the code elements for the future line to parse.

                for (int i = largestRefreshParseSection.StartLineIndex;
                    i < (largestRefreshParseSection.StopLineIndex == documentLines.Count - 1 && largestRefreshParseSection.StopToken == null //If the last index is equals to number of line in document, make sure to also reset the last line, otherwise, reset lines normally. 
                        ? largestRefreshParseSection.StopLineIndex + 1
                        : largestRefreshParseSection.StopLineIndex);
                    i++)
                {
                    if (documentLines[i].CodeElements != null)
                        documentLines[i].ResetCodeElements();
                }
            }

           


            // Reset parsing error diagnostics
            cobolErrorStrategy.Reset(cobolParser);

            // Try to parse code elements :
            // - starting with the current parse section Start token
            // - ending with the current parse section Stop token
            CodeElementsParser.CobolCodeElementsContext codeElementsParseTree = null;
            try
            {
                perfStatsForParserInvocation.OnStartParsing();
                if (AntlrPerformanceProfiler != null) AntlrPerformanceProfiler.BeginParsingSection();
                codeElementsParseTree = cobolParser.cobolCodeElements();
                if (AntlrPerformanceProfiler != null)
                    AntlrPerformanceProfiler.EndParsingSection(codeElementsParseTree.ChildCount);
                perfStatsForParserInvocation.OnStopParsing(
                    AntlrPerformanceProfiler != null
                        ? (int) AntlrPerformanceProfiler.CurrentFileInfo.DecisionTimeMs
                        : 0,
                    AntlrPerformanceProfiler != null
                        ? AntlrPerformanceProfiler.CurrentFileInfo.RuleInvocations.Sum()
                        : 0);
            }
            catch (Exception e)
            {
                var currentToken = (Token) cobolParser.CurrentToken;
                CodeElementsLine codeElementsLine = GetCodeElementsLineForToken(currentToken);
                if (codeElementsLine != null)
                {
                    codeElementsLine.AddParserDiagnostic(new TokenDiagnostic(MessageCode.ImplementationError,
                        currentToken, currentToken.Line, e));
                }
            }

            if (codeElementsParseTree != null)
            {
                // If the parse tree is not empty
                if (codeElementsParseTree.codeElement() != null && codeElementsParseTree.codeElement().Length > 0)
                {
                    // Analyze the parse tree for each code element
                    foreach (var codeElementParseTree in codeElementsParseTree.codeElement())
                    {
                        // Get the first line that was parsed     
                        var tokenStart = (Token) codeElementParseTree.Start;
                        CodeElementsLine codeElementsLine = GetCodeElementsLineForToken(tokenStart);
                        if (codeElementsLine == null)
                        {
                            continue;
                        }

                        // Register that this line was updated
                        // COMMENTED FOR THE SAKE OF PERFORMANCE -- SEE ISSUE #160
                        //int updatedLineIndex = documentLines.IndexOf(codeElementsLine, codeElementsLine.LineIndex);
                        //codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, updatedLineIndex, codeElementsLine));
                        codeElementsLinesChanges.Add(
                            new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated,
                                codeElementsLine.LineIndex, codeElementsLine));

                        perfStatsForParserInvocation.OnStartTreeBuilding();
                        // Visit the parse tree to build a first class object representing the code elements
                        try
                        {
                            walker.Walk(codeElementBuilder, codeElementParseTree);
                        }
                        catch (Exception ex)
                        {
                            var code = MessageCode.ImplementationError;
                            int line = 0;
                            int start = 0;
                            int stop = 0;
                            if (codeElementsLine.SourceTokens != null && codeElementsLine.SourceTokens.Count > 0)
                            {
                                start = codeElementsLine.SourceTokens[0].StartIndex;
                                stop =
                                    codeElementsLine.SourceTokens[codeElementsLine.SourceTokens.Count - 1].StopIndex;
                            }
                            codeElementsLine.AddParserDiagnostic(new ParserDiagnostic(ex.ToString(), start, stop,
                                line, null, code, ex));
                        }
                        CodeElement codeElement = codeElementBuilder.CodeElement;
                        if (codeElement != null)
                        {
                            // Attach consumed tokens and main document line numbers information to the code element
                            if (codeElement.ConsumedTokens.Count == 0)
                            {
// ISSUE #204:
                                var tempToken = tokenStream.Lt(1);
                                if (tempToken != null && tempToken != Token.END_OF_FILE)
                                {
// if not end of file,
                                    // add next token to ConsumedTokens to know where is the CodeElement in error
                                    codeElement.ConsumedTokens.Add((Token) tempToken);
                                    // this alter CodeElements semantics: in addition to matched tokens,
                                    // it includes the first token in error if no token has been matched
                                }
                            }

                            //TODO Issue #384 to discuss if this code should stay here:
                            //This should be in a Checker, but "codeElement.ConsumedTokens" is only set after all the checkers have been called
                            //Rule TCLIMITATION_NO_CE_ACROSS_SOURCES
                            if (codeElement.IsAcrossSourceFile())
                            {
                                DiagnosticUtils.AddError(codeElement,
                                    "A Cobol statement cannot be across 2 sources files (eg. Main program and a COPY)",
                                    MessageCode.TypeCobolParserLimitation);
                            }

                            // Add code element to the list                    
                            codeElementsLine.AddCodeElement(codeElement);
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
                            CodeElementsLine codeElementsLine =
                                GetCodeElementsLineForToken((Token) d.OffendingSymbol);
                            if (codeElementsLine != null)
                                codeElementsLine.AddParserDiagnostic(d);
                        }
                    }
                }
                perfStatsForParserInvocation.OnStopTreeBuilding();
            }


            if (AntlrPerformanceProfiler != null)
                AntlrPerformanceProfiler.EndParsingFile(cobolParser.ParseInfo.DecisionInfo, (int)(cobolParser.ParseInfo.GetTotalTimeInPrediction() / 1000000));

            return codeElementsLinesChanges;
        }

        private static CodeElementsLine GetCodeElementsLineForToken(Token tokenStart)
        {
            CodeElementsLine codeElementsLine = null;
            var importedToken = tokenStart as ImportedToken;
            if (importedToken != null)
            {
                if (importedToken.CopyDirective.TextNameSymbol.TokensLine is CodeElementsLine)
                    codeElementsLine = (CodeElementsLine)importedToken.CopyDirective.TextNameSymbol.TokensLine;
            }
            else
            {
                if (tokenStart.TokensLine is CodeElementsLine)
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
                int lastLineIndexReset = lastParseSection != null ? lastParseSection.StopLineIndex : -1;
                IEnumerator<CodeElementsLine> reversedEnumerator = documentLines.GetEnumerator(previousLineIndex - 1, -1, true);
                bool previousLineHasCodeElements = false;
                while (reversedEnumerator.MoveNext() && (--previousLineIndex > lastLineIndexReset))
                {
                    // Get the previous line until the first code element is encountered
                    CodeElementsLine previousLine = reversedEnumerator.Current;

                    // The start of the parse section is delimited by the previous CodeElement
                    if (previousLine != null)
                    {
                        previousLineHasCodeElements = previousLine.HasCodeElements;
                        if (previousLineHasCodeElements)
                        {
                            currentParseSection.StartLineIndex = previousLineIndex;
                            currentParseSection.StartToken = previousLine.CodeElements.First().ConsumedTokens.FirstOrDefault();
                        }

                        // All lines contained in the parse section could be modified, and should be reset
                        previousLine = (CodeElementsLine)prepareDocumentLineForUpdate(previousLineIndex, previousLine, CompilationStep.CodeElementsParser);
                        previousLine.ResetCodeElements();
                        codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, previousLineIndex, previousLine));
                    }

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
                    if (nextLine != null)
                    {
                        nextLineHasCodeElements = nextLine.HasCodeElements;
                        bool nextCodeElementStartsAtTheBeginningOfTheLine = false;
                        if (nextLineHasCodeElements)
                        {
                            try
                            {
                                Token startTokenForNextParseSection = nextLine.CodeElements.First().ConsumedTokens.FirstOrDefault();
                                Token firstSourceTokenOfThisLine = nextLine.TokensWithCompilerDirectives.First(token => token.Channel == Token.CHANNEL_SourceTokens);
                                nextCodeElementStartsAtTheBeginningOfTheLine = startTokenForNextParseSection == firstSourceTokenOfThisLine;
                            }
                            catch (System.InvalidOperationException /*e*/)
                            {//JCM: 28/08/2017: I noticed that this Exception can occur if: it doesn't exists a token which verifies the predicate: token.Channel == Token.CHANNEL_SourceToken
                                nextCodeElementStartsAtTheBeginningOfTheLine = false;
                            }
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
                            currentParseSection.StopToken = nextLine.CodeElements.First().ConsumedTokens.FirstOrDefault();
                            currentParseSection.StopTokenIsFirstTokenOfTheLine = nextCodeElementStartsAtTheBeginningOfTheLine;
                            break;
                        }
                    }
                }
                // If no CodeElement was found on the next lines, current parse section current parse section ends at the end of the file
                if (!nextLineHasCodeElements)
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
