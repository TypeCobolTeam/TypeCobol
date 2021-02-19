using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Incrementally scan a set of text changes
    /// </summary>
    public static class ScannerStep
    {
        /// <summary>
        /// Initial scan of a complete document
        /// </summary>
        public static void ScanDocument(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations, [NotNull] MultilineScanState initialScanState)
        {
            TokensLine tokensLine = null;
            MultilineScanState lastScanState = initialScanState;
            IList<TokensLine> lineGroup = new List<TokensLine>();
            bool groupIsContinuationGroup = false;

            using (IEnumerator<TokensLine> documentLinesEnumerator = documentLines.GetEnumerator())
            {
                // Get the first line
                if (documentLinesEnumerator.MoveNext())
                {
                    tokensLine = documentLinesEnumerator.Current;
                }

                while (tokensLine != null)
                {
                    lineGroup.Add(tokensLine);

                    //Peek at next line type to decide whether to scan the current line group or continue to accumulate
                    var nextTokensLine = documentLinesEnumerator.MoveNext() ? documentLinesEnumerator.Current : null;
                    switch (nextTokensLine?.Type)
                    {
                        case null:
                        case CobolTextLineType.Source:
                            //tokensLine is the last one or next line is a new Source line
                            //Scan the current group
                            ScanGroup();
                            break;
                        case CobolTextLineType.Continuation:
                            //Remember that the current group contains at least one continuation line
                            groupIsContinuationGroup = true;
                            break;
                            //default: keep accumulating lines (Blank, Comment, etc) into current group
                    }

                    tokensLine = nextTokensLine;

                    void ScanGroup()
                    {
                        if (groupIsContinuationGroup)
                        {
                            //Group must be scanned as a whole
                            Scanner.ScanTokensLineContinuationGroup(lineGroup, lastScanState, textSourceInfo.ColumnsLayout, compilerOptions, copyTextNameVariations);
                            lastScanState = lineGroup.Last().ScanState;
                        }
                        else
                        {
                            //Scan each line separately
                            foreach (var line in lineGroup)
                            {
                                Scanner.ScanTokensLine(line, lastScanState, compilerOptions, copyTextNameVariations);
                                lastScanState = line.ScanState;
                            }
                        }

                        //Reset group state
                        groupIsContinuationGroup = false;
                        lineGroup.Clear();
                    }
                }
            }
        }

        /// <summary>
        /// Incremental scan of a set of text lines changes
        /// </summary>
        internal static IList<DocumentChange<ITokensLine>> ScanTextLinesChanges(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, IList<DocumentChange<ICobolTextLine>> textLinesChanges, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations, [NotNull] MultilineScanState scanState)
        {
            // Collect all changes applied to the tokens lines during the incremental scan
            IList<DocumentChange<ITokensLine>> tokensLinesChanges = new List<DocumentChange<ITokensLine>>();

            // There are 3 reasons to scan a line after a text change :
            // 1. New text lines which were just inserted or updated must be scanned for the first time
            // 2. Text lines must be scanned again if their initial scan state changed : a new scan of the previous line can alter the scan state at the beginning of the following line  
            // 3. Continuation lines and multiline tokens : if a line participates in a continuation on several lines, scan the group of lines as a whole

            // IMPORTANT : the text changes are ordered in increasing order of line index
            foreach (var textChange in textLinesChanges)
            {
                // Local variables used to optimize navigation in the document
                int nextLineToScanIndex = -1;
                TokensLine nextLineToScan = null;

                // Update tokens depending on the current text change
                if (textChange.Type == DocumentChangeType.DocumentCleared)
                {
                    tokensLinesChanges.Add(new DocumentChange<ITokensLine>(DocumentChangeType.DocumentCleared, 0, null));
                    continue;
                }
                else if (textChange.Type == DocumentChangeType.LineInserted || textChange.Type == DocumentChangeType.LineUpdated)
                {
                    // We update lines as a group below, but we remember here which lines were inserted
                    if (textChange.Type == DocumentChangeType.LineInserted)
                    {
                        tokensLinesChanges.Add(new DocumentChange<ITokensLine>(DocumentChangeType.LineInserted, textChange.LineIndex, (ITokensLine)textChange.NewLine));
                    }

                    // Text lines which were inserted or updated must be scanned again
                    ScanTokensLineWithMultilineScanState(textChange.LineIndex, (TokensLine)textChange.NewLine, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, copyTextNameVariations, tokensLinesChanges, scanState, out nextLineToScanIndex, out nextLineToScan);
                }
                else if (textChange.Type == DocumentChangeType.LineRemoved)
                {
                    tokensLinesChanges.Add(new DocumentChange<ITokensLine>(DocumentChangeType.LineRemoved, textChange.LineIndex, (ITokensLine)textChange.NewLine));

                    // Get the last line just before the line that was removed
                    TokensLine previousLine = null;
                    if (textChange.LineIndex > 0)
                    {
                        previousLine = documentLines[textChange.LineIndex - 1];
                    }

                    // When a text line is removed :
                    // - the previous line must be scanned again if the line which was removed was a member of a multiline continuation group or a multi line comment.
                    if (previousLine != null &&
                        (previousLine.HasTokenContinuedOnNextLine ||
                         (previousLine.ScanState != null && (previousLine.ScanState.InsideMultilineComments || previousLine.ScanState.InsideFormalizedComment))))
                    {
                        ScanTokensLineWithMultilineScanState(textChange.LineIndex - 1, previousLine, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, copyTextNameVariations, tokensLinesChanges, scanState, out nextLineToScanIndex, out nextLineToScan);
                    }
                    if (nextLineToScan == null && textChange.LineIndex < documentLines.Count)
                    {
                        nextLineToScanIndex = textChange.LineIndex;
                        nextLineToScan = documentLines[nextLineToScanIndex];
                    }
                    // - the next line must be scanned again if the scan state at the end of the previous line is different from the scan state at the beginning of the next line
                    if (nextLineToScan != null && nextLineToScanIndex == textChange.LineIndex && previousLine != null &&
                        !nextLineToScan.InitialScanState.Equals(previousLine.ScanState))
                    {
                        ScanTokensLineWithMultilineScanState(textChange.LineIndex, nextLineToScan, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, copyTextNameVariations, tokensLinesChanges, scanState, out nextLineToScanIndex, out nextLineToScan);
                    }
                }

                // We can skip all text changes with an index smaller than the index of the last line which was already scanned
                if (nextLineToScan == null)
                {
                    break;
                }
            }

            return tokensLinesChanges;
        }

        /// <summary>
        /// Check if two Tokens set are different
        /// </summary>
        /// <param name="curTokens">Current Tokens set</param>
        /// <param name="newTokens">New Token Set</param>
        /// <returns>true if different, false otherwise</returns>
        private static bool AreTokensChanging(Token[] curTokens, Token[] newTokens)
        {
            if (curTokens == newTokens)
                return false;
            if (curTokens.Length != newTokens.Length)
                return true;
            for (int i = 0; i < curTokens.Length; i++)
                if (!object.Equals(curTokens[i], newTokens[i]))
                    return true;
            return false;
        }

        private static void ScanTokensLineWithMultilineScanState(int lineToScanIndex, TokensLine lineToScan, TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations, IList<DocumentChange<ITokensLine>> tokensLinesChanges, MultilineScanState scanState, out int nextLineToScanIndex, out TokensLine nextLineToScan)
        {
            // Scan the current line (or continuation lines group)
            MultilineScanState currentScanState = ScanTokensLineWithContinuations(lineToScanIndex, lineToScan, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, copyTextNameVariations, tokensLinesChanges, scanState, out nextLineToScanIndex, out nextLineToScan);

            // Scan the following lines until we find that the scan state at the beginning of the next line has been updated
            while (nextLineToScan?.InitialScanState != null)
            {
                if (nextLineToScan.InitialScanState.Equals(currentScanState))
                {//The state is no longer changing, but before stopping we check that tokens are not also changing on that line.
                    TokensLine currentScanLine = nextLineToScan;
                    Token[] curTokens = new Token[currentScanLine.SourceTokens.Count];
                    currentScanLine.SourceTokens.CopyTo(curTokens, 0);

                    currentScanState = ScanTokensLineWithContinuations(nextLineToScanIndex, nextLineToScan, textSourceInfo,
                        documentLines, prepareDocumentLineForUpdate, compilerOptions, copyTextNameVariations,
                        tokensLinesChanges, currentScanState, out nextLineToScanIndex, out nextLineToScan);

                    Token[] newTokens = new Token[currentScanLine.SourceTokens.Count];
                    currentScanLine.SourceTokens.CopyTo(newTokens, 0);
                    bool bNotChanging = !AreTokensChanging(curTokens, newTokens);
                    if (bNotChanging)
                        break;//The tokens of the line have not changed.
                }
                else
                {
                    currentScanState = ScanTokensLineWithContinuations(nextLineToScanIndex, nextLineToScan, textSourceInfo,
                        documentLines, prepareDocumentLineForUpdate, compilerOptions, copyTextNameVariations,
                        tokensLinesChanges, currentScanState, out nextLineToScanIndex, out nextLineToScan);
                }
            }
        }

        private static MultilineScanState ScanTokensLineWithContinuations(int lineToScanIndex, TokensLine lineToScan, TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations, IList<DocumentChange<ITokensLine>> tokensLinesChanges,
            MultilineScanState scanState, out int nextLineToScanIndex, out TokensLine nextLineToScan)
        {
            // Initialize out parameters
            if (lineToScanIndex < (documentLines.Count - 1))
            {
                nextLineToScanIndex = lineToScanIndex + 1;
                nextLineToScan = documentLines[nextLineToScanIndex];
            }
            else
            {
                nextLineToScanIndex = -1;
                nextLineToScan = null;
            }

            // Check if the line to scan participates in a multiline continuation
            // - because it is itself a continuation line
            bool partOfMultilineContinuation = lineToScan.Type == CobolTextLineType.Continuation;
            // - or because the following line is a continuation line
            if (!partOfMultilineContinuation && lineToScanIndex < (documentLines.Count - 1))
            {
                nextLineToScanIndex = lineToScanIndex + 1;
                nextLineToScan = documentLines[nextLineToScanIndex];
                partOfMultilineContinuation = nextLineToScan.Type == CobolTextLineType.Continuation;                 
            }
            // ^-- LIMITATION : we don't support the case where one or more comment lines follow the current line before a continuation line 

            // Case 1 : the line is not part of a multiline continuation => we can parse it as as a standalone line
            if (!partOfMultilineContinuation)
            {
                // Create a new copy of the line before the update if necessary
                lineToScan = (TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner);
                lineToScan.SourceTokens.Clear(); //Erase previous SourceTokens to let the scanner creates the new tokens
                if (lineToScanIndex == 0)
                {
                    // Use start ScanState
                    Scanner.ScanTokensLine(lineToScan, scanState, compilerOptions, copyTextNameVariations);
                }
                else
                {
                    // Get the scan state at the end of the previous line
                    int tempLineIndex = lineToScanIndex - 1;
                    TokensLine previousLine = documentLines[tempLineIndex];
                    while (previousLine.ScanState == null)
                    {
                        tempLineIndex--;
                        previousLine = documentLines[tempLineIndex];
                    }
                    // Scan the current line with this initial scan state 
                    Scanner.ScanTokensLine(lineToScan, previousLine.ScanState, compilerOptions, copyTextNameVariations);
                }
                tokensLinesChanges.Add(new DocumentChange<ITokensLine>(DocumentChangeType.LineUpdated, lineToScanIndex, lineToScan));
                return lineToScan.ScanState;
            }
            // Case 2 : the line is part of a multiline continuation => we must parse all continuation lines as a group
            else
            {
                // Build a list of the lines we will have to scan as a group :
                IList<TokensLine> continuationLinesGroup = new List<TokensLine>();
                int firstLineIndex = lineToScanIndex;
                lineToScan.SourceTokens.Clear();
                continuationLinesGroup.Insert(0, (TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner));

                // Navigate backwards to the start of the multiline continuation 
                if (lineToScan.Type == CobolTextLineType.Continuation && lineToScanIndex > 0)
                {
                    int revLineToScanIndex = lineToScanIndex;
                    using (var reversedEnumerator = documentLines.GetEnumerator(lineToScanIndex - 1, -1, true))
                    {
                        while (reversedEnumerator.MoveNext())
                        {
                            // Get the previous line until a non continuation and non comment line is encountered
                            revLineToScanIndex--;
                            lineToScan = reversedEnumerator.Current;

                            if (lineToScan?.Type == CobolTextLineType.Continuation /*&&  <-- LIMITATION : this compiler does not support comment or blank lines between two continuation line
                           lineToScan.Type != CobolTextLineType.Comment && lineToScan.Type != CobolTextLineType.Blank*/) // see p54 : for continuation, blank lines are treated like comment lines
                            {
                                firstLineIndex = revLineToScanIndex;
                                continuationLinesGroup.Insert(0, (TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner));
                            }
                            else
                            {
                                break;
                            }
                        }
                    }
                }

                // Reuse our knowledge of the next line if it is available and if it is a continuation
                if (nextLineToScan != null && nextLineToScan.Type == CobolTextLineType.Continuation)
                {
                    lineToScanIndex++;
                    lineToScan = nextLineToScan;
                    lineToScan.SourceTokens.Clear();
                    continuationLinesGroup.Add((TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner));

                    nextLineToScanIndex = -1;
                    nextLineToScan = null;
                }

                // Navigate forwards to the end of the multiline continuation 
                if (lineToScanIndex < (documentLines.Count - 1))
                {
                    using (var enumerator = documentLines.GetEnumerator(lineToScanIndex + 1, -1, false))
                    {
                        while (enumerator.MoveNext())
                        {
                            // Get the next line until a non continuation and non comment line is encountered
                            lineToScanIndex++;
                            lineToScan = enumerator.Current;

                            if (lineToScan?.Type == CobolTextLineType.Continuation /*|| <-- LIMITATION : this compiler does not support comment or blank lines between two continuation line
                           lineToScan.Type == CobolTextLineType.Comment || lineToScan.Type == CobolTextLineType.Blank*/) // see p54 : for continuation, blank lines are treated like comment lines
                            {
                                lineToScan.SourceTokens.Clear();
                                // Add this line at the end of the list of continuation lines
                                continuationLinesGroup.Add((TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner));
                            }
                            else
                            {
                                // Save the knowledge of the next line and exit the loop
                                nextLineToScanIndex = lineToScanIndex;
                                nextLineToScan = lineToScan;
                                break;
                            }
                        }
                    }
                }

                // Scan the group of continuation lines
                if (firstLineIndex == 0)
                {
                    // Use start ScanState
                    Scanner.ScanTokensLineContinuationGroup(continuationLinesGroup, scanState, textSourceInfo.ColumnsLayout, compilerOptions, copyTextNameVariations);
                }
                else
                {
                    // Get the scan state at the end of the previous line
                    TokensLine previousLine = documentLines[firstLineIndex - 1];
                    // Scan the current line group with this initial scan state 
                    Scanner.ScanTokensLineContinuationGroup(continuationLinesGroup, previousLine.ScanState, textSourceInfo.ColumnsLayout, compilerOptions, copyTextNameVariations);
                }
                int updatedLineIndex = firstLineIndex;
                foreach(TokensLine updatedLine in continuationLinesGroup)
                {
                    tokensLinesChanges.Add(new DocumentChange<ITokensLine>(DocumentChangeType.LineUpdated, updatedLineIndex, updatedLine));
                    updatedLineIndex++;
                }
                return continuationLinesGroup[continuationLinesGroup.Count - 1].ScanState;
            }
        }
    }
}
