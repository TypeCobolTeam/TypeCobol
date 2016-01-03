using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
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
            public ParseSection(Token startToken, Token startTokenForNextSection)
            {
                StartToken = startToken;
                StartTokenForNextCodeElement = startTokenForNextSection;
            }
            public Token StartToken { get; private set; }
            public Token StartTokenForNextCodeElement { get; private set; }
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

            // --- PREPARATION PHASE : identify all parse sections where code element need to be refreshed ---

            IList<ParseSection> refreshParseSections = null;

            // Iterate over all processed tokens changes detected by the PreprocessorStep :
            // refresh all the adjacent lines participating in a CodeElement
            if (processedTokensLinesChanges != null)
            {
                refreshParseSections = new List<ParseSection>();

                int lastLineIndexReset = -1;
                foreach (DocumentChange<IProcessedTokensLine> tokensChange in processedTokensLinesChanges)
                {
                    if (tokensChange.LineIndex > lastLineIndexReset)
                    {
                        lastLineIndexReset = CheckIfAdjacentLinesNeedRefresh(tokensChange.Type, tokensChange.LineIndex, documentLines, prepareDocumentLineForUpdate, processedTokensLinesChanges, lastLineIndexReset, refreshParseSections);
                    }
                }
            }

            // Create a token iterator on top of pre-processed tokens lines
            ITokensLinesIterator tokensIterator = ProcessedTokensDocument.GetProcessedTokensIterator(textSourceInfo, documentLines);

            // Create an Antlr compatible token source on top a the token iterator
            TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                textSourceInfo.Name,
                tokensIterator);

            // Init parser
            ITokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
            TracingCobolParser cobolParser = new TracingCobolParser(tokenStream);
            // -> activate full ambiguities detection
            //parser.Interpreter.PredictionMode = PredictionMode.LlExactAmbigDetection; 
            IAntlrErrorStrategy cobolErrorStrategy = new CobolErrorStrategy();
            cobolParser.ErrorHandler = cobolErrorStrategy;

            // Register all parse errors in a list in memory
            DiagnosticSyntaxErrorListener errorListener = new DiagnosticSyntaxErrorListener();
            cobolParser.RemoveErrorListeners();
            cobolParser.AddErrorListener(errorListener);

            // Prepare to analyze the parse tree
            ParseTreeWalker walker = new ParseTreeWalker();
            CodeElementBuilder codeElementBuilder = new CodeElementBuilder();
            codeElementBuilder.Dispatcher = new CodeElementDispatcher();
            codeElementBuilder.Dispatcher.CreateListeners();

            // TO DO -- Iterate only over the code elements which need to be refreshed
            do
            {
                // TO DO -- Seek just before the next code element starting token
                // tokensIterator.SeekToToken(codeElementStartingToken);

                // !! Do not reset the Antlr BufferedTokenStream position while iterating on adjacent code elements
                //tokenStream.SetTokenSource(tokenSource);

                // Reset parsing error diagnostics
                //cobolErrorStrategy.Reset(cobolParser);
                errorListener.Diagnostics.Clear();

                // Reset parser traces (consumed tokens)
                cobolParser.ResetTraces();

                // Try to parse a code element starting with the current token
                CobolCodeElementsParser.CodeElementContext codeElementParseTree = cobolParser.codeElement();

                // If the parse tree is not empty
                if (codeElementParseTree.Start.Type > 0)
                {
                    // Get the first line that was parsed                
                    CodeElementsLine codeElementsLine = ((CodeElementsLine)((Token)codeElementParseTree.Start).TokensLine);

                    // Visit the parse tree to build a first class object representing the code elements
                    walker.Walk(codeElementBuilder, codeElementParseTree);
                    CodeElement codeElement = codeElementBuilder.CodeElement;
                    if (codeElement != null)
                    {
                        // Attach consumed tokens and main document line numbers information to the code element
                        codeElement.ConsumedTokens = cobolParser.ConsumedTokens;

                        // Add code element to the list                    
                        codeElementsLine.AddCodeElement(codeElement);
                        foreach (ParserDiagnostic d in errorListener.Diagnostics)
                        {
                            codeElement.Diagnostics.Add(d);
                        }
                        foreach (Diagnostic d in codeElement.Diagnostics)
                        {
                            codeElementsLine.AddParserDiagnostic(d);
                        }
                    }
                    else
                    {
                        foreach (ParserDiagnostic d in errorListener.Diagnostics)
                        {
                            codeElementsLine.AddParserDiagnostic(d);
                        }
                    }
                }
            }
            while (tokenStream.La(1) >= 0);

            return codeElementsLinesChanges;
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
        private static int CheckIfAdjacentLinesNeedRefresh(DocumentChangeType changeType, int lineIndex, ISearchableReadOnlyList<ProcessedTokensLine> documentLines, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, IList<DocumentChange<IProcessedTokensLine>> processedTokensLinesChanges, int lastLineIndexReset, IList<ParseSection> refreshParseSections)
        {
            // Navigate backwards to the start of the multiline compiler directive
            if (lineIndex > 0)
            {
                int previousLineIndex = lineIndex;
                IEnumerator<ProcessedTokensLine> reversedEnumerator = documentLines.GetEnumerator(previousLineIndex - 1, -1, true);
                while (reversedEnumerator.MoveNext() && (--previousLineIndex > lastLineIndexReset))
                {
                    // Get the previous line until a non continued line is encountered
                    ProcessedTokensLine previousLine = reversedEnumerator.Current;

                    // A reset line was already treated by the previous call to CheckIfAdjacentLinesNeedRefresh : stop searching
                    if (previousLine.PreprocessingState == ProcessedTokensLine.PreprocessorState.NeedsCompilerDirectiveParsing)
                    {
                        break;
                    }
                    // Previous line is a continuation : reset this line and continue navigating backwards
                    // Previous line is not a continuation but is continued : reset this line and stop navigating backwards
                    else if (previousLine.HasDirectiveTokenContinuationFromPreviousLine || previousLine.HasDirectiveTokenContinuedOnNextLine)
                    {
                        lineIndex = previousLineIndex;
                        previousLine = (ProcessedTokensLine)prepareDocumentLineForUpdate(previousLineIndex, previousLine, CompilationStep.Preprocessor);
                        processedTokensLinesChanges.Add(new DocumentChange<IProcessedTokensLine>(DocumentChangeType.LineUpdated, previousLineIndex, previousLine));
                        if (!previousLine.HasDirectiveTokenContinuationFromPreviousLine)
                        {
                            break;
                        }
                    }
                    // Previous line not involved in a multiline compiler directive : stop searching
                    else
                    {
                        break;
                    }
                }
            }

            // Navigate forwards to the end of the multiline compiler directive 
            if (lineIndex < (documentLines.Count - 1))
            {
                int nextLineIndex = lineIndex;
                IEnumerator<ProcessedTokensLine> enumerator = documentLines.GetEnumerator(nextLineIndex + 1, -1, true);
                while (enumerator.MoveNext())
                {
                    // Get the next line until non continuation line is encountered
                    nextLineIndex++;
                    ProcessedTokensLine nextLine = enumerator.Current;

                    // A reset line will be treated by the next call to CheckIfAdjacentLinesNeedRefresh : stop searching
                    if (nextLine.PreprocessingState == ProcessedTokensLine.PreprocessorState.NeedsCompilerDirectiveParsing)
                    {
                        break;
                    }
                    // Next line is a continuation and is continued: reset this line and continue navigating forwards
                    // Next line is a continuation but is not continued : reset this line and stop navigating forwards
                    else if (nextLine.HasDirectiveTokenContinuationFromPreviousLine)
                    {
                        nextLine = (ProcessedTokensLine)prepareDocumentLineForUpdate(nextLineIndex, nextLine, CompilationStep.Preprocessor);
                        processedTokensLinesChanges.Add(new DocumentChange<IProcessedTokensLine>(DocumentChangeType.LineUpdated, nextLineIndex, nextLine));
                        lastLineIndexReset = nextLineIndex;
                        if (!nextLine.HasDirectiveTokenContinuedOnNextLine)
                        {
                            break;
                        }
                    }
                    // Previous line not involved in a multiline compiler directive : stop searching
                    else
                    {
                        break;
                    }
                }
            }

            return lastLineIndexReset > lineIndex ? lastLineIndexReset : lineIndex;
        }
    }
}
