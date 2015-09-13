using Antlr4.Runtime;
using Antlr4.Runtime.Tree;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Subjects;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Preprocessor.Generated;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// View of a source document after COPY and REPLACE processing
    /// </summary>
    public class ProcessedTokensDocument : IObserver<TokensChangedEvent>
    {
        /// <summary>
        /// Underlying TokensDocument (tokens before COPY and REPLACE processing)
        /// </summary>
        public TokensDocument TokensDocument { get; private set; }

        /// <summary>
        /// Lines of the source text file viewed as lists of processed tokens and error messages
        /// </summary>
        public IReadOnlyList<IProcessedTokensLine> ProcessedTokensLines { get { return processedTokensLines; } }

        // Implement this as an immutable list to protect consumers from changes happening on the producer side
        private ImmutableList<ProcessedTokensLine> processedTokensLines;

        /// <summary>
        /// Compiler options directing the preprocessor operations
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }       

        /// <summary>
        /// The build system implements an efficient way to retreive ProcessedTokensDocuments
        /// for all COPY compiler directives
        /// </summary>
        private IProcessedTokensDocumentProvider processedTokensDocumentProvider;

        /// <summary>
        /// Iterator over the tokens contained in this document after
        /// - compiler directives processing
        /// - COPY directives text imports
        /// - REPLACE directive token remplacements
        /// </summary>
        public ITokensLinesIterator GetTokensIterator()
        {
            ITokensLinesIterator copyIterator = new CopyTokensLinesIterator(TokensDocument.TextSourceInfo.Name, processedTokensLines, Token.CHANNEL_SourceTokens);
            ITokensLinesIterator replaceIterator = new ReplaceTokensLinesIterator(copyIterator);
            return replaceIterator;
        }

        /// <summary>
        /// Initialize the preprocessor with the tokens produced by the scanner
        /// </summary>
        public ProcessedTokensDocument(TokensDocument tokensDocument, TypeCobolOptions compilerOptions, IProcessedTokensDocumentProvider processedTokensDocumentProvider)
        {
            TokensDocument = tokensDocument;
            processedTokensLines = ImmutableList<ProcessedTokensLine>.Empty;
            CompilerOptions = compilerOptions;
            this.processedTokensDocumentProvider = processedTokensDocumentProvider;
        }

        public void OnNext(TokensChangedEvent tokensChangedEvent)
        {
            try
            {
                TokensChangedEvent processedTokensChangedEvent = new TokensChangedEvent();

                // --- PREPARATION PHASE : Initialize missing lines and include all lines involved in a processed Token continuation ---

                // Init. Optimization : use a builder to update the immutable list in case of a big update                
                ImmutableList<ProcessedTokensLine>.Builder tokensLinesBuilder = null;
                if (tokensChangedEvent.TokensChanges.Count > 4)
                {
                    tokensLinesBuilder = processedTokensLines.ToBuilder();
                }
                Func<int, ProcessedTokensLine> getTokensLineAtIndex = index => { if (tokensLinesBuilder != null) { return tokensLinesBuilder[index]; } else { return processedTokensLines[index]; } };
                Action<int, ProcessedTokensLine> setTokensLineAtIndex = (index, updatedLine) => { if (tokensLinesBuilder != null) { tokensLinesBuilder[index] = updatedLine; } else { processedTokensLines = processedTokensLines.SetItem(index, updatedLine); } };
                Action<int, ProcessedTokensLine> insertTokensLineAtIndex = (index, insertedLine) => { if (tokensLinesBuilder != null) { tokensLinesBuilder.Insert(index, insertedLine); } else { processedTokensLines = processedTokensLines.Insert(index, insertedLine); } };
                Action<ProcessedTokensLine> removeTokensLine = removedLine => { if (tokensLinesBuilder != null) { tokensLinesBuilder.Remove(removedLine); } else { processedTokensLines = processedTokensLines.Remove(removedLine); } };
                Action clearTokensLines = () => { if (tokensLinesBuilder != null) { tokensLinesBuilder.Clear(); } else { processedTokensLines = ImmutableList<ProcessedTokensLine>.Empty; } };

                // 1. Iterate over all tokens changes detected by the Scanner:
                // - refresh and reset all changed ProcessedTokensLines accordingly
                // - refresh all the adjacent lines participating in a ContinuationTokensGroup
                foreach (TokensChange tokensChange in tokensChangedEvent.TokensChanges)
                {
                    switch (tokensChange.Type)
                    {
                        case TokensChangeType.DocumentCleared:
                            // Reset the immutable list
                            clearTokensLines();
                            // Register a DocumentCleared change 
                            processedTokensChangedEvent.TokensChanges.Add(tokensChange);
                            break;
                        case TokensChangeType.LineInserted:
                            // Insert a new line in the immutable list
                            ProcessedTokensLine processedTokensLine = new ProcessedTokensLine(tokensChange.NewLine);
                            insertTokensLineAtIndex(tokensChange.LineIndex, processedTokensLine);
                            // Also reset adjacent lines in case of continuation
                            CheckIfAdjacentLinesNeedRefresh(tokensChange.Type, tokensChange.LineIndex, setTokensLineAtIndex);
                            break;
                        case TokensChangeType.LineUpdated:
                        case TokensChangeType.LineRescanned:
                            // Replace an existing line in the immutable list 
                            processedTokensLine = new ProcessedTokensLine(tokensChange.NewLine);
                            setTokensLineAtIndex(tokensChange.LineIndex, processedTokensLine);
                            // Also reset adjacent lines in case of continuation
                            CheckIfAdjacentLinesNeedRefresh(tokensChange.Type, tokensChange.LineIndex, setTokensLineAtIndex);
                            break;
                        case TokensChangeType.LineRemoved:
                            // Remove an existing line in the immutable list
                            processedTokensLine = getTokensLineAtIndex(tokensChange.LineIndex);
                            removeTokensLine(processedTokensLine);
                            // Also reset adjacent lines in case of continuation
                            CheckIfAdjacentLinesNeedRefresh(tokensChange.Type, tokensChange.LineIndex, setTokensLineAtIndex);
                            break;
                    }
                }

                // Term. End of optimization : revert the builder to an immutable list
                if (tokensLinesBuilder  != null)
                {
                    processedTokensLines = tokensLinesBuilder.ToImmutable();
                }

                // --- COMPILER DIRECTIVES PHASE : Find and parse all compiler directives ---
                
                // Init. Prepare a compiler directive parser

                // Create a token iterator on top of tokens lines
                TokensLinesIterator tokensIterator = new TokensLinesIterator(
                    TokensDocument.TextSourceInfo.Name,
                    TokensDocument.TokensLines,
                    null,
                    Token.CHANNEL_SourceTokens);

                // Crate an Antlr compatible token source on top a the token iterator
                TokensLinesTokenSource tokenSource = new TokensLinesTokenSource(
                   TokensDocument.TextSourceInfo.Name,
                   tokensIterator);                

                // Init a compiler directive parser
                CommonTokenStream tokenStream = new TokensLinesTokenStream(tokenSource, Token.CHANNEL_SourceTokens);
                CobolCompilerDirectivesParser directivesParser = new CobolCompilerDirectivesParser(tokenStream);
                IAntlrErrorStrategy compilerDirectiveErrorStrategy = new CompilerDirectiveErrorStrategy();
                directivesParser.ErrorHandler = compilerDirectiveErrorStrategy;

                // Register all parse errors in a list in memory
                DiagnosticSyntaxErrorListener errorListener = new DiagnosticSyntaxErrorListener();
                directivesParser.RemoveErrorListeners();
                directivesParser.AddErrorListener(errorListener);

                // Prepare to analyze the parse tree
                ParseTreeWalker walker = new ParseTreeWalker();
                CompilerDirectiveBuilder directiveBuilder = new CompilerDirectiveBuilder();

                // 1. Iterate over all compiler directive starting tokens found in the lines which were updated 
                foreach (Token compilerDirectiveStartingToken in processedTokensLines
                    .Where(line => line.ProcessingState == ProcessedTokensLine.PreprocessorState.NeedsCompilerDirectiveParsing)
                    .SelectMany(line => line.SourceTokens)
                    .Where(token => token.TokenFamily == TokenFamily.CompilerDirectiveStartingKeyword) )
                {                    
                    // 2. Reset the compiler directive parser state

                    // Reset tokens iterator position before parsing
                    // -> seek just before the compiler directive starting token
                    tokensIterator.SeekToToken(compilerDirectiveStartingToken);
                    tokensIterator.PreviousToken();
                    // Special case : for efficiency reasons, in EXEC SQL INCLUDE directives
                    // only the third token INCLUDE is recognized as a compiler directive
                    // starting keyword by the scanner. In this case, we must rewind the 
                    // iterator two tokens backwards to start parsing just before the EXEC token.
                    if(compilerDirectiveStartingToken.TokenType == TokenType.EXEC_SQL_INCLUDE)
                    {
                        tokensIterator.PreviousToken();
                        tokensIterator.PreviousToken();
                    }

                    // Reset Antlr BufferedTokenStream position
                    tokenStream.SetTokenSource(tokenSource);

                    // Reset parsing error diagnostics
                    compilerDirectiveErrorStrategy.Reset(directivesParser);
                    errorListener.Diagnostics.Clear();

                    // 3. Try to parse a compiler directive starting with the current token
                    CobolCompilerDirectivesParser.CompilerDirectingStatementContext directiveParseTree = directivesParser.compilerDirectingStatement();

                    // 4. Visit the parse tree to build a first class object representing the compiler directive
                    walker.Walk(directiveBuilder, directiveParseTree);
                    CompilerDirective compilerDirective = directiveBuilder.CompilerDirective;
                    bool errorFoundWhileParsingDirective = errorListener.Diagnostics.Count > 0 || directiveBuilder.Diagnostics.Count > 0;
                                        
                    // 5. Get all tokens consumed while parsing the compiler directive
                    //    and partition them line by line 
                    Token startToken = (Token)directiveParseTree.Start;
                    Token stopToken = (Token)directiveParseTree.Stop;
                    if (stopToken == null) stopToken = startToken;
                    MultilineTokensGroupSelection tokensSelection = tokensIterator.SelectAllTokensBetween(startToken, stopToken);

                    if (compilerDirective != null)
                    {
                        // 6. Replace all matched tokens by :
                        // - a CompilerDirectiveToken on the first line
                        ProcessedTokensLine firstProcessedTokensLine = getTokensLineAtIndex(tokensSelection.FirstLineIndex);
                        if (tokensSelection.SelectedTokensOnSeveralLines.Length == 1)
                        {
                            firstProcessedTokensLine.InsertCompilerDirectiveTokenOnFirstLine(
                                tokensSelection.TokensOnFirstLineBeforeStartToken,
                                compilerDirective, errorFoundWhileParsingDirective,
                                tokensSelection.SelectedTokensOnSeveralLines[0],
                                tokensSelection.TokensOnLastLineAfterStopToken);
                        }
                        else
                        {
                            TokensGroup continuedTokensGroup = firstProcessedTokensLine.InsertCompilerDirectiveTokenOnFirstLine(
                                tokensSelection.TokensOnFirstLineBeforeStartToken,
                                compilerDirective, errorFoundWhileParsingDirective,
                                tokensSelection.SelectedTokensOnSeveralLines[0],
                                null);

                            // - a ContinuationTokensGroup on the following lines
                            int selectionLineIndex = 1;
                            int lastLineIndex = tokensSelection.FirstLineIndex + tokensSelection.SelectedTokensOnSeveralLines.Length - 1;
                            for (int nextLineIndex = tokensSelection.FirstLineIndex + 1; nextLineIndex <= lastLineIndex; nextLineIndex++, selectionLineIndex++)
                            {
                                IList<Token> compilerDirectiveTokensOnNextLine = tokensSelection.SelectedTokensOnSeveralLines[selectionLineIndex];
                                if (compilerDirectiveTokensOnNextLine.Count > 0)
                                {
                                    ProcessedTokensLine nextProcessedTokensLine = getTokensLineAtIndex(nextLineIndex);
                                    if (nextLineIndex != lastLineIndex)
                                    {
                                        continuedTokensGroup = nextProcessedTokensLine.InsertCompilerDirectiveTokenOnNextLine(
                                            continuedTokensGroup,
                                            compilerDirectiveTokensOnNextLine,
                                            null);
                                    }
                                    else
                                    {
                                        continuedTokensGroup = nextProcessedTokensLine.InsertCompilerDirectiveTokenOnNextLine(
                                            continuedTokensGroup,
                                            compilerDirectiveTokensOnNextLine,
                                            tokensSelection.TokensOnLastLineAfterStopToken);
                                    }
                                }
                            }
                        }
                    }

                    // 7. Register compiler directive parse errors
                    if (errorFoundWhileParsingDirective)
                    {
                        ProcessedTokensLine compilerDirectiveLine = processedTokensLines[tokensSelection.FirstLineIndex];
                        foreach(ParserDiagnostic parserDiag in errorListener.Diagnostics)
                        {
                            compilerDirectiveLine.AddDiagnostic(parserDiag);
                        }
                        foreach (Diagnostic directiveDiag in directiveBuilder.Diagnostics)
                        {
                            compilerDirectiveLine.AddDiagnostic(directiveDiag);
                        }
                    }
                }

                // 8. Advance the state off all ProcessedTokensLines : 
                // NeedsCompilerDirectiveParsing => NeedsCopyDirectiveProcessing if it contains a COPY directive
                // 
                foreach (ProcessedTokensLine parsedLine in processedTokensLines
                    .Where(line => line.ProcessingState == ProcessedTokensLine.PreprocessorState.NeedsCompilerDirectiveParsing))
                {
                    if (parsedLine.ImportedDocuments != null)
                    {
                        parsedLine.ProcessingState = ProcessedTokensLine.PreprocessorState.NeedsCopyDirectiveProcessing;
                    }
                    else
                    {
                        parsedLine.ProcessingState = ProcessedTokensLine.PreprocessorState.NeedsReplaceDirectiveProcessing;
                    }
                }

                // --- COPY IMPORT PHASE : Process COPY (REPLACING) directives ---

                // 1. Iterate over all updated lines containing a new COPY directive
                foreach (ProcessedTokensLine tokensLineWithCopyDirective in processedTokensLines
                    .Where(line => line.ProcessingState == ProcessedTokensLine.PreprocessorState.NeedsCopyDirectiveProcessing))
                {
                    // Iterate over all COPY directives found on one updated line
                    foreach(CopyDirective copyDirective in tokensLineWithCopyDirective.ImportedDocuments.Keys.ToArray<CopyDirective>())
                    {
                        try
                        {
                            // Load (or retrieve in cache) the document referenced by the COPY directive
                            ProcessedTokensDocument importedDocumentSource = processedTokensDocumentProvider.GetProcessedTokensDocument(copyDirective.LibraryName, copyDirective.TextName);

                            // Store it on the current line after appying the REPLACING directive
                            ImportedTokensDocument importedDocument = new ImportedTokensDocument(copyDirective, importedDocumentSource);
                            tokensLineWithCopyDirective.ImportedDocuments[copyDirective] = importedDocument;
                        }
                        catch(Exception e)
                        {
                            // Text name refenced by COPY directive was not found
                            // => register a preprocessor error on this line                            
                            Token failedDirectiveToken = tokensLineWithCopyDirective.TokensWithCompilerDirectives
                                .Where(token => token.TokenType == TokenType.CopyImportDirective && ((CompilerDirectiveToken)token).CompilerDirective == copyDirective)
                                .First();
                            Diagnostic diag = new Diagnostic(
                                MessageCode.FailedToLoadTextDocumentReferencedByCopyDirective,
                                failedDirectiveToken.Column, failedDirectiveToken.EndColumn,
                                e.Message);
                            tokensLineWithCopyDirective.AddDiagnostic(diag);
                        }
                    }

                    // Advance processing status of the line
                    tokensLineWithCopyDirective.ProcessingState = ProcessedTokensLine.PreprocessorState.NeedsReplaceDirectiveProcessing;
                }

                // --- REPLACE PHASE : Process REPLACE directives ---

                /* Algorithm :
                 * 
                 * one REPLACE directive can express several replacement operations
                 * one replacement operation can be of several types (distinguished for optimization purposes)
                 * - SimpleTokenReplace : one source token / zero or one replacement token
                 * - PartialWordReplace : one pure partial word / zero or one replacement token
                 * - SimpleToMultipleTokenReplace : one source token / several replacement tokens
                 * - MultipleTokenReplace : one first + several following source tokens / zero to many replacement tokens
                 * 
                 * an iterator maintains a current set of replacement operations
                 * 
                 * if nextToken is replace directive
                 *    the current set of replacement operations is updated
                 * else 
                 *    nextToken is compared to each replacement operation in turn
                 *       if single -> single source token operation : return replacement token
                 *       if single -> multiple operation : setup a secondary iterator with the list of replacement tokens
                 *       if multiple -> multiple operation
                 *          snapshot of the underlying iterator
                 *          try to match all of the following source tokens
                 *          if failure : restore snapshot and try next operation
                 *          if success : setup a secondary iterator
                 * 
                 * token comparison sourceToken / replacementCandidate :
                 * 1. Compare Token type
                 * 2. If same token type and for families
                 *   AlphanumericLiteral
                 *   NumericLiteral
                 *   SyntaxLiteral
                 *   Symbol
                 *  => compare also Token text
                 *  
                 * PartialCobolWord replacement :
                 * p535 : The COPY statement with REPLACING phrase can be used to replace parts of words.
                 * By inserting a dummy operand delimited by colons into the program text, 
                 * the compiler will replace the dummy operand with the required text. 
                 * Example 3 shows how this is used with the dummy operand :TAG:. 
                 * The colons serve as separators and make TAG a stand-alone operand. 
                 */

                // 1. Find all lines which could be affected

    
                


                tokensChangedEventsSource.OnNext(processedTokensChangedEvent);
            }
            catch (Exception ex)
            {
                // Register and forward errors
                LastException = ex;
                tokensChangedEventsSource.OnError(ex);
            }
        }

        /// <summary>
        /// Illustration : lines with directives continued from previous line are marked with a cross
        /// [     ]
        /// [x    ]
        /// [x    ]
        /// [     ]
        /// Insert : 
        /// - a line inserted at lineIndex intersects with an existing continuation if lineIndex+1 is continuation
        /// Update / Remove :
        /// - a line update at lineIndex intersects with an existing continuation if lineIndex is continuation (before update) or if lineIndex+1 is continuation
        /// All cases :
        ///  - refresh lineIndex - n until no continuation (included) and lineIndex + n until no continuation (excluded)
        /// </summary>
        private void CheckIfAdjacentLinesNeedRefresh(TokensChangeType changeType, int lineIndex, Action<int, ProcessedTokensLine> setTokensLineAtIndex)
        {
            int lastIndex = processedTokensLines.Count - 1;
 	        if((changeType == TokensChangeType.LineInserted && lineIndex < lastIndex && processedTokensLines[lineIndex + 1].IsContinuedFromPreviousLine) ||
               ((changeType == TokensChangeType.LineUpdated || changeType == TokensChangeType.LineRemoved) &&
                (processedTokensLines[lineIndex + 1].IsContinuedFromPreviousLine || (lineIndex < lastIndex && processedTokensLines[lineIndex + 1].IsContinuedFromPreviousLine))))
            {
                // Mark previous lines for refresh
                if (lineIndex > 0)
                {
                    bool exitAfterCurrentLine = !processedTokensLines[lineIndex].IsContinuedFromPreviousLine;
                    for (int i = lineIndex - 1; i >= 0; i--)
                    {
                        ProcessedTokensLine currentLine = processedTokensLines[i];
                        if (currentLine.ProcessingState == ProcessedTokensLine.PreprocessorState.NeedsCompilerDirectiveParsing // previous lines already marked for refresh
                            || exitAfterCurrentLine) 
                        {
                            break;
                        }
                        ProcessedTokensLine freshTokensLine = new ProcessedTokensLine(currentLine);
                        setTokensLineAtIndex(i, freshTokensLine);
                        if (!currentLine.IsContinuedFromPreviousLine)
                        {
                            exitAfterCurrentLine = true;
                        }
                    }
                }
                // Mark next lines for refresh
                if(lineIndex < lastIndex)
                {
                    for (int i = lineIndex + 1; i <= lastIndex; i++)
                    {
                        ProcessedTokensLine currentLine = processedTokensLines[i];
                        if (currentLine.ProcessingState == ProcessedTokensLine.PreprocessorState.NeedsCompilerDirectiveParsing) // next lines already marked for refresh
                        {
                            break;
                        }
                        if (currentLine.IsContinuedFromPreviousLine)
                        {
                            ProcessedTokensLine freshTokensLine = new ProcessedTokensLine(currentLine);
                            setTokensLineAtIndex(i, freshTokensLine);
                        }
                        else
                        {
                            break;
                        }
                    }
                }
            }
        }

        public void OnCompleted()
        {
            // do nothing here
        }

        public void OnError(Exception error)
        {
            // do nothing here
        }

        // Debug only
        public Exception LastException { get; private set; }

        // --- Implement IObservable<TokensChangedEvent>

        private ISubject<TokensChangedEvent> tokensChangedEventsSource = new Subject<TokensChangedEvent>();

        public IObservable<TokensChangedEvent> TokensChangedEventsSource
        {
            get { return tokensChangedEventsSource; }
        }
    }
}
