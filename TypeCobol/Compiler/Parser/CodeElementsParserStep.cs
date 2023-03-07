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
using TypeCobol.Logging;

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
        internal static void ParseDocument(ProcessedTokensDocument processedTokensDocument, TypeCobolOptions compilerOptions, PerfStatsForParserInvocation perfStatsForParserInvocation)
        {
            ParseProcessedTokensLinesChanges(processedTokensDocument, null, null, compilerOptions, perfStatsForParserInvocation);
        }

        /// <summary>
        /// Represents a section of code we need to re-parse in search of new code elements
        /// </summary>
        private class ParseSection
        {
            public ParseSection()
            { }

            public ParseSection(int startLineIndex, Token startToken, int stopLineIndex, Token stopToken)
            {
                StartLineIndex = startLineIndex;
                StartToken = startToken;
                StopLineIndex = stopLineIndex;
                StopToken = stopToken;
            }

            public int StartLineIndex { get; set; }
            public Token StartToken { get; set; }
            public int StopLineIndex { get; set; }
            public Token StopToken { get; set; }
        }

        // When not null, optionnaly used to gather Antlr performance profiling information
        public static AntlrPerformanceProfiler AntlrPerformanceProfiler;

        /// <summary>
        /// Incremental parsing of a set of processed tokens lines changes
        /// </summary>
        internal static IList<DocumentChange<ICodeElementsLine>> ParseProcessedTokensLinesChanges(ProcessedTokensDocument processedTokensDocument, IList<DocumentChange<IProcessedTokensLine>> processedTokensLinesChanges, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions, PerfStatsForParserInvocation perfStatsForParserInvocation)
        {
            // Collect all changes applied to the processed tokens lines during the incremental scan
            ISearchableReadOnlyList<CodeElementsLine> documentLines = (ISearchableReadOnlyList<CodeElementsLine>) processedTokensDocument.Lines;
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
                            lastParseSection = CheckIfAdjacentLinesNeedRefresh(tokensChange, documentLines, prepareDocumentLineForUpdate, codeElementsLinesChanges, lastParseSection);
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
                largestRefreshParseSection = new ParseSection(minParseSection.StartLineIndex, minParseSection.StartToken, maxParseSection.StopLineIndex, maxParseSection.StopToken);
            }


            // --- INITIALIZE ANTLR CodeElements parser ---

            // Create a token iterator on top of pre-processed tokens lines and position on start line
            ITokensLinesIterator tokensIterator = processedTokensDocument.GetProcessedTokensIterator();

            //For incremental mode, calculate best value for start line of the iterator
            if (largestRefreshParseSection != null)
            {
                //No need to check for ReplacedToken or ImportedToken here, as we directly interact with documentLines
                //which are the lines of the original document.

                //TODO find all ReplaceDirective that can target the current line.
                //Count the largest number of tokens that a Replace directive can target
                //Rollback by this number of tokens

                //Temporary: for now rollback X lines as it's unlikely that a Replace target more than that
                int startLine = Math.Max(0, largestRefreshParseSection.StartLineIndex - 40);
                if (startLine > 0)
                {
                    //Take previous line has long as there are tokens continued on this line
                    using (var iterator = documentLines.GetEnumerator(startLine, -1, true))
                    {
                        while (iterator.MoveNext())
                        {
                            var current = iterator.Current;
                            System.Diagnostics.Debug.Assert(current != null);
                            if (!current.HasTokenContinuationFromPreviousLine)
                            {
                                break;
                            }

                            startLine--;
                        }
                    }

                    //Position iterator at start line
                    tokensIterator.SeekToLineInMainDocument(startLine);
                }
            }

            // Create an Antlr compatible token source on top of the token iterator
            TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                processedTokensDocument.TextSourceInfo.Name,
                tokensIterator);

            // Init parser
            TokensLinesTokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
            CodeElementsParser cobolParser = new CodeElementsParser(tokenStream);
            // REVERT TO STD PARSER ==> TracingCobolParser cobolParser = new TracingCobolParser(tokenStream);

            // Optionnaly activate Antlr Parser performance profiling
            // WARNING : use this in a single-treaded context only (uses static field)     
            if (perfStatsForParserInvocation.ActivateDetailedAntlrPofiling)
                AntlrPerformanceProfiler = new AntlrPerformanceProfiler(cobolParser);
            else
                AntlrPerformanceProfiler = null;

            if (AntlrPerformanceProfiler != null)
            {
                // Replace the generated parser by a subclass which traces all rules invocations
                cobolParser = new CodeElementsTracingParser(tokenStream);

                var tokensCountIterator = processedTokensDocument.GetProcessedTokensIterator();
                AntlrPerformanceProfiler.BeginParsingFile(processedTokensDocument.TextSourceInfo, tokensCountIterator);
            }

            // Customize error recovery strategy
            IAntlrErrorStrategy cobolErrorStrategy = new CodeElementErrorStrategy();
            cobolParser.ErrorHandler = cobolErrorStrategy;

            // Register all parse errors in a list in memory
            ParserDiagnosticErrorListener errorListener = new ParserDiagnosticErrorListener();
            cobolParser.RemoveErrorListeners();
            cobolParser.AddErrorListener(errorListener);

            // Prepare to analyze the parse tree
            // Restore debugging mode flag using StartToken of the reparse section
            bool isDebuggingModeEnabled = false;
            if (largestRefreshParseSection?.StartToken != null)
            {
                isDebuggingModeEnabled = largestRefreshParseSection.StartToken.ScanStateSnapshot.WithDebuggingMode;
            }
            ParseTreeWalker walker = new ParseTreeWalker();
            CodeElementBuilder codeElementBuilder = new CodeElementBuilder(compilerOptions, isDebuggingModeEnabled);

            // --- INCREMENTAL PARSING ---

            //-------------------------------------------------------------------------------------
            // In case of incremental parsing, parse only the code sections we need to refresh
            //-------------------------------------------------------------------------------------
            // By https://github.com/TypeCobolTeam/TypeCobol/issues/1351 it can happend that the
            // The scanning can go beyond the section to be reparsed by one line, this can happend
            // If the previous line was only made of whitespaces or comments.
            // So we maintains a set of CodeElementsLine to be reseted.
            //-------------------------------------------------------------------------------------
            HashSet<CodeElementsLine> ResetedCodeElementsLines = null;//Set of new incremental lines to reseted
            int IncrementalLineLimit = -1;//Original limit of the incremental section
            if (largestRefreshParseSection != null)
            {
                // Seek just before the next code element starting token
                if (tokenStream.SeekToToken(largestRefreshParseSection.StartToken))
                {
                    tokenStream.StartLookingForStopToken(largestRefreshParseSection.StopToken);

                    //Remove all the code elements for the future line to parse.
                    IncrementalLineLimit = (largestRefreshParseSection.StopLineIndex == documentLines.Count - 1 && largestRefreshParseSection.StopToken == null //If the last index is equals to number of line in document, make sure to also reset the last line, otherwise, reset lines normally. 
                            ? largestRefreshParseSection.StopLineIndex + 1
                            : largestRefreshParseSection.StopLineIndex);
                    for (int i = largestRefreshParseSection.StartLineIndex; i < IncrementalLineLimit; i++)
                    {
                        documentLines[i].ResetCodeElements();
                    }
                }
                else
                {
                    // Fail to find a starting point ==> Reparse all.
                    tokenStream.Seek(0);
                    tokenStream.ResetStopTokenLookup();
                    foreach (var documentLine in documentLines)
                    {
                        documentLine.ResetCodeElements();
                    }
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
                    codeElementsLine.AddParserDiagnostic(new TokenDiagnostic(MessageCode.ImplementationError, currentToken, e));
                }
            }

            if (codeElementsParseTree != null)
            {
                // If the parse tree is not empty
                if (codeElementsParseTree.codeElement() != null && codeElementsParseTree.codeElement().Length > 0)
                {
                    var diagnosticsToReport = new List<Diagnostic>();
                    var codeElementsLineSet = new HashSet<CodeElementsLine>();

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

#if EUROINFO_RULES
                        // Now that the tokens for this line have been enumerated, we can access the ProcessingDiagnostics of the COPY directives (if any)
                        // Copy these diagnostics into ParserDiagnostics but do it only once for each line !
                        if (codeElementsLineSet.Add(codeElementsLine) && codeElementsLine.ImportedDocuments != null)
                        {
                            foreach (var copyDirective in codeElementsLine.ImportedDocuments.Keys)
                            {
                                if (copyDirective.ProcessingDiagnostics != null)
                                {
                                    foreach (var copyDirectiveProcessingDiagnostic in copyDirective.ProcessingDiagnostics)
                                    {
                                        codeElementsLine.AddParserDiagnostic(copyDirectiveProcessingDiagnostic);
                                    }
                                }
                            }
                        }
#endif

                        if (IncrementalLineLimit >= 0 && tokenStart.Line >= IncrementalLineLimit)
                        {
                            bool tokenStartIsImported = tokenStart is TypeCobol.Compiler.Preprocessor.ImportedToken;
                            if (!tokenStartIsImported)
                            {
                                if (ResetedCodeElementsLines == null)
                                {
                                    ResetedCodeElementsLines = new HashSet<CodeElementsLine>();
                                }
                                if (ResetedCodeElementsLines.Count == 0 || !ResetedCodeElementsLines.Contains(codeElementsLine))
                                {
                                    ResetedCodeElementsLines.Add(codeElementsLine);
                                    codeElementsLine.ResetCodeElements();
                                }
                            }
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
                            var position = Diagnostic.Position.Default;
                            if (codeElementsLine.SourceTokens != null && codeElementsLine.SourceTokens.Count > 0)
                            {
                                position = codeElementsLine.SourceTokens[0].Position();
                            }
                            codeElementsLine.AddParserDiagnostic(new ParserDiagnostic(ex.ToString(), position, null, code, ex));
                        }
                        perfStatsForParserInvocation.OnStopTreeBuilding();

                        CodeElement codeElement = codeElementBuilder.CodeElement;
                        if (codeElement != null)
                        {
                            // Add the multiline comments Token to the consumed Tokens in order to comment them in CodeGen
                            if (!codeElement.IsInsideCopy() && codeElement.ConsumedTokens.Count >= 2)
                            {
                                bool tokenHasBeenInjected = false;

                                // Do not iterate on a list that will be modified ☻
                                int stopLine = codeElement.LineEnd - 1;

                                for (int lineIndex = codeElement.ConsumedTokens[0].Line -1 ;
                                    lineIndex < stopLine && lineIndex < documentLines.Count;
                                    lineIndex++)
                                {
                                    var multilineTokens = documentLines[lineIndex].SourceTokens.Where(t =>
                                        t.TokenType == TokenType.MULTILINES_COMMENTS_START ||
                                        t.TokenType == TokenType.MULTILINES_COMMENTS_STOP ||
                                        t.TokenType == TokenType.CommentLine);


                                    foreach (var multilineToken in multilineTokens)
                                    {
                                        tokenHasBeenInjected = true;
                                        codeElement.ConsumedTokens.Add(multilineToken);
                                    }
                                }

                                if (tokenHasBeenInjected)
                                {
                                    codeElement.ConsumedTokens = codeElement.ConsumedTokens.OrderBy(t => t.Line)
                                        .ThenBy(t => t.Column).ToList();
                                }

                            }

                            if (codeElement.ConsumedTokens.Count == 0)
                            {
                                // Discard invalid CEs without consumed tokens but report their diagnostics on next CE
                                if (codeElement.Diagnostics != null)
                                {
                                    diagnosticsToReport.AddRange(codeElement.Diagnostics);
                                }
                                continue;
                            }

                            //TODO Issue #384 to discuss if this code should stay here:
                            //This should be in a Checker, but "codeElement.ConsumedTokens" is only set after all the checkers have been called
                            //Rule TCLIMITATION_NO_CE_ACROSS_SOURCES
                            if (codeElement.IsAcrossSourceFile())
                            {
                                if (compilerOptions.IsCobolLanguage)
                                {
                                    //Allowed in pure Cobol, emit a warning
                                    DiagnosticUtils.AddError(codeElement, "A Cobol statement should not be across 2 sources files (eg. Main program and a COPY)", MessageCode.Warning);
                                }
                                else
                                {
                                    //Incompatible with TC Codegen, create an error
                                    DiagnosticUtils.AddError(codeElement, null, MessageCode.TypeCobolParserLimitation);
                                }
                            }

                            //Should also be in a checker, however IsInsideCopy requires tokens to be set (just like IsAcrossSourceFile)
                            if (codeElement.IsInsideCopy())
                            {
                                CodeElementWithTokensChecker.CheckIsScanStateAlteringCodeElement(codeElement);
                            }

                            //Report diagnostics if some incomplete CE have been encountered previously
                            if (diagnosticsToReport.Count > 0)
                            {
                                if (codeElement.Diagnostics == null)
                                {
                                    codeElement.Diagnostics = new List<Diagnostic>(diagnosticsToReport);
                                }
                                else
                                {
                                    codeElement.Diagnostics.AddRange(diagnosticsToReport);
                                }

                                diagnosticsToReport.Clear();
                            }

                            // Add code element to the list
                            codeElementsLine.AddCodeElement(codeElement);
                        }
                    }

                    if (diagnosticsToReport.Count > 0)
                    {
                        //We still have unreported diagnostics but no CE to attach to... Should not happen.
                        LoggingSystem.LogMessage(LogLevel.Warning, "Unreported diagnostics found !");
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
        private static ParseSection CheckIfAdjacentLinesNeedRefresh(DocumentChange<IProcessedTokensLine> change, ISearchableReadOnlyList<CodeElementsLine> documentLines, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, IList<DocumentChange<ICodeElementsLine>> codeElementsLinesChanges, ParseSection lastParseSection)
        {
            ParseSection currentParseSection = new ParseSection();
            bool foundStopCodeElement;

            // Special case for REPLACE: if a REPLACE directive has been updated, we have to go over all tokens up to next REPLACE directive (or end of file if none found)
            var changedProcessedTokensLine = (ProcessedTokensLine)change.NewLine;
            bool lookForNextReplaceDirective = changedProcessedTokensLine?.ReplaceDirective != null;

            // Navigate backwards to the start of the multiline code element
            int lineIndex = change.LineIndex;
            if (lineIndex > 0)
            {
                int previousLineIndex = lineIndex;
                int lastLineIndexReset = lastParseSection != null ? lastParseSection.StopLineIndex : -1;
                IEnumerator<CodeElementsLine> reversedEnumerator = documentLines.GetEnumerator(previousLineIndex - 1, -1, true);
                foundStopCodeElement = false;
                while (reversedEnumerator.MoveNext() && (--previousLineIndex > lastLineIndexReset))
                {
                    // Get the previous line until the first code element is encountered
                    CodeElementsLine previousLine = reversedEnumerator.Current;
                    System.Diagnostics.Debug.Assert(previousLine != null);

                    // The start of the parse section is delimited by the previous CodeElement
                    bool previousCodeElementStartsAtBeginningOfTheLine = false;
                    Token previousCodeElementFirstToken = null;
                    if (previousLine.HasCodeElements)
                    {
                        previousCodeElementFirstToken = previousLine.CodeElements[0].ConsumedTokens.First();
                        Token previousLineFirstSourceToken = previousLine.SourceTokens.FirstOrDefault();
                        previousCodeElementStartsAtBeginningOfTheLine = previousCodeElementFirstToken == previousLineFirstSourceToken;
                    }

                    if (!previousCodeElementStartsAtBeginningOfTheLine)
                    {
                        /*
                         * Either:
                         * - previous line has no code element
                         * - previous line has code element but starts with the end of a multiline CE or a CE that failed to parse correctly
                         * => add line to the parse section
                         */
                        previousLine = (CodeElementsLine)prepareDocumentLineForUpdate(previousLineIndex, previousLine, CompilationStep.CodeElementsParser);
                        codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, previousLineIndex, previousLine));
                    }
                    else
                    {
                        foundStopCodeElement = true;
                        currentParseSection.StartLineIndex = previousLineIndex;
                        currentParseSection.StartToken = previousCodeElementFirstToken;
                        break;
                    }
                }

                // If no CodeElement was found on the previous lines, current parse section starts at the beginning of the file
                // (because last parseSection could only stop at a CodeElement or at the end of the file)
                if (!foundStopCodeElement)
                {
                    currentParseSection.StartLineIndex = 0;
                    currentParseSection.StartToken = null;
                }
            }
            // If line 0 was updated, current parse section starts at the beginning of the file
            else
            {
                currentParseSection.StartLineIndex = 0;
                currentParseSection.StartToken = null;
            }

            // Navigate forwards to the end of the multiline code element or to the next REPLACE directive
            if (lineIndex < (documentLines.Count - 1))
            {
                int nextLineIndex = lineIndex;
                IEnumerator<CodeElementsLine> enumerator = documentLines.GetEnumerator(nextLineIndex + 1, -1, false);
                bool stopOnNextCodeElement = false;
                foundStopCodeElement = false;
                while (enumerator.MoveNext())
                {
                    // Get the next line until new CodeElement is encountered
                    nextLineIndex++;
                    CodeElementsLine nextLine = enumerator.Current;
                    System.Diagnostics.Debug.Assert(nextLine != null);

                    // Check REPLACE directive
                    if (lookForNextReplaceDirective)
                    {
                        if (nextLine.ReplaceDirective != null)
                        {
                            // next line is a new REPLACE directive, stop looking for REPLACE and look for next CodeElement
                            lookForNextReplaceDirective = false;
                        }
                        else
                        {
                            // Line could have been updated through the effect of the modified REPLACE
                            nextLine = (CodeElementsLine)prepareDocumentLineForUpdate(nextLineIndex, nextLine, CompilationStep.CodeElementsParser);
                            codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, nextLineIndex, nextLine));
                            continue;
                        }
                    }

                    // Check if the next CodeElement starts at the beginning of the line
                    var nextLineHasCodeElements = nextLine.HasCodeElements;
                    bool nextCodeElementStartsAtTheBeginningOfTheLine = false;
                    Token nextLineCodeElementFirstToken = null;
                    if (nextLineHasCodeElements)
                    {
                        nextLineCodeElementFirstToken = nextLine.CodeElements[0].ConsumedTokens.First();
                        Token nextLineFirstSourceToken = nextLine.SourceTokens.FirstOrDefault();
                        nextCodeElementStartsAtTheBeginningOfTheLine = nextLineCodeElementFirstToken == nextLineFirstSourceToken;
                    }

                    // All lines contained in the parse section could be modified
                    if (!nextCodeElementStartsAtTheBeginningOfTheLine)
                    {
                        // TO DO : ERROR below, will not work if we have source tokens from previous CodeElement + one other CodeElement on the same line
                        // => the other CodeElement will be deleted by prepareDocumentLineForUpdate and not parsed again
                        nextLine = (CodeElementsLine)prepareDocumentLineForUpdate(nextLineIndex, nextLine, CompilationStep.CodeElementsParser);
                        codeElementsLinesChanges.Add(new DocumentChange<ICodeElementsLine>(DocumentChangeType.LineUpdated, nextLineIndex, nextLine));
                        stopOnNextCodeElement = false;
                    }

                    // Stop iterating forwards as soon as the start of a previously parsed CodeElement is found                   
                    if (nextLineHasCodeElements)
                    {
                        // Take one more CodeElement to refresh diagnostics related to one CE but located after it
                        if (stopOnNextCodeElement)
                        {
                            currentParseSection.StopLineIndex = nextLineIndex;
                            currentParseSection.StopToken = nextLineCodeElementFirstToken;
                            foundStopCodeElement = true;
                            break;
                        }

                        // Ok next line will be the last one.
                        stopOnNextCodeElement = true;
                    }
                }

                // If no CodeElement was found on the next lines, current parse section current parse section ends at the end of the file
                if (!foundStopCodeElement)
                {
                    currentParseSection.StopLineIndex = documentLines.Count - 1;
                    currentParseSection.StopToken = null;
                }
            }
            // If last line was updated, or if no CodeElement was found after the updated line, current parse section ends at the end of the file
            else
            {
                currentParseSection.StopLineIndex = documentLines.Count - 1;
                currentParseSection.StopToken = null;
            }

            return currentParseSection;
        }
    }
}
