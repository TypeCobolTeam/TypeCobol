using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Incrementally scan a set of text changes
    /// </summary>
    static class ScannerStep
    {
        /// <summary>
        /// Initial scan of a complete document
        /// </summary>
        internal static void ScanDocument(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, TypeCobolOptions compilerOptions)
        {
            MultilineScanState lastScanState = null;
            foreach (ITokensLine documentLine in documentLines)
            {
                TokensLine tokensLine = (TokensLine)documentLines;
                if (lastScanState == null)
                {
                    Scanner.ScanFirstLine(tokensLine, false, false, false, textSourceInfo.EncodingForAlphanumericLiterals, compilerOptions);
                }
                else
                {
                    Scanner.ScanTokensLine(tokensLine, lastScanState, compilerOptions);
                }
                lastScanState = tokensLine.ScanState;
            }
        }

        /// <summary>
        /// Incremental scan of a set of text lines changes
        /// </summary>
        internal static IList<DocumentChange<ITokensLine>> ScanTextLinesChanges(TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, IList<DocumentChange<ICobolTextLine>> textLinesChanges, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions)
        {
            // Collect all changes applied to the tokens lines during the incremental scan
            IList<DocumentChange<ITokensLine>> tokensLinesChanges = new List<DocumentChange<ITokensLine>>();

            // There are 3 reasons to scan a line after a text change :
            // 1. New text lines which were just inserted or updated must be scanned for the first time
            // 2. Text lines must be scanned again if their initial scan state changed : a new scan of the previous line can alter the scan state at the beginning of the following line  
            // 3. Continuation lines and multiline tokens : if a line participates in a continuation on several lines, scan the group of lines as a whole

            // IMPORTANT : the text changes are ordered in increasing order of line index
            for (int textChangeIndex = 0; textChangeIndex < textLinesChanges.Count; textChangeIndex++)
            {
                // Local variables used to optimize navigation in the document
                int nextLineToScanIndex = -1;
                TokensLine nextLineToScan = null;

                // Update tokens depending on the current text change
                DocumentChange<ICobolTextLine> textChange = textLinesChanges[textChangeIndex];
                if (textChange.Type == DocumentChangeType.DocumentCleared)
                {
                    continue;
                }
                else if (textChange.Type == DocumentChangeType.LineInserted || textChange.Type == DocumentChangeType.LineUpdated)
                {
                    // Text lines which were inserted or updated must be scanned again
                    ScanTokensLineWithMultilineScanState(textChange.LineIndex, (TokensLine)textChange.NewLine, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, tokensLinesChanges, out nextLineToScanIndex, out nextLineToScan);
                }
                else if (textChange.Type == DocumentChangeType.LineRemoved)
                {
                    // Get the last line just before the line that was removed
                    TokensLine previousLine = null;
                    if (textChange.LineIndex > 0)
                    {
                        previousLine = documentLines[textChange.LineIndex - 1];
                    }

                    // When a text line is removed :
                    // - the previous line must be scanned again if the line which was removed was a member of a multiline continuation group
                    if (previousLine != null && previousLine.HasTokenContinuedOnNextLine)
                    {
                        ScanTokensLineWithMultilineScanState(textChange.LineIndex - 1, previousLine, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, tokensLinesChanges, out nextLineToScanIndex, out nextLineToScan);
                    }
                    if (nextLineToScan == null && textChange.LineIndex < documentLines.Count)
                    {
                        nextLineToScanIndex = textChange.LineIndex;
                        nextLineToScan = documentLines[nextLineToScanIndex];
                    }
                    // - the next line must be scanned again if the scan state at the end of the previous line is different from the scan state at the beginning of the next line
                    if (nextLineToScan != null && nextLineToScanIndex == textChange.LineIndex && previousLine != null &&
                        nextLineToScan.InitialScanState.Equals(previousLine.ScanState))
                    {
                        ScanTokensLineWithMultilineScanState(textChange.LineIndex, nextLineToScan, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, tokensLinesChanges, out nextLineToScanIndex, out nextLineToScan);
                    }
                }

                // We can skip all text changes with an index smaller than the index of the last line which was already scanned
                if (nextLineToScan == null)
                {
                    break;
                }
                else if (textChangeIndex < (textLinesChanges.Count - 1))
                {
                    int nextTextChangeIndex = textChangeIndex;
                    DocumentChange<ICobolTextLine> nextTextChange = null;
                    do
                    {
                        nextTextChangeIndex++;
                        nextTextChange = textLinesChanges[nextTextChangeIndex];
                    }
                    while (nextTextChangeIndex < (textLinesChanges.Count - 1) &&
                           nextTextChange.LineIndex <= nextLineToScanIndex);
                    textChangeIndex = nextTextChangeIndex - 1;
                }
            }

            return tokensLinesChanges;
        }

        private static void ScanTokensLineWithMultilineScanState(int lineToScanIndex, TokensLine lineToScan, TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions, IList<DocumentChange<ITokensLine>> tokensLinesChanges, out int nextLineToScanIndex, out TokensLine nextLineToScan)
        {
            // Scan the current line (or continuation lines group)
            MultilineScanState scanState = ScanTokensLineWithContinuations(lineToScanIndex, lineToScan, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, tokensLinesChanges, out nextLineToScanIndex, out nextLineToScan);
            
            // Scan the following lines until we find that the scan state at the beginning of the next line has been updated
            while(nextLineToScan != null && !nextLineToScan.InitialScanState.Equals(scanState))
            {
                scanState = ScanTokensLineWithContinuations(nextLineToScanIndex, nextLineToScan, textSourceInfo, documentLines, prepareDocumentLineForUpdate, compilerOptions, tokensLinesChanges, out nextLineToScanIndex, out nextLineToScan);
            }
        }

        private static MultilineScanState ScanTokensLineWithContinuations(int lineToScanIndex, TokensLine lineToScan, TextSourceInfo textSourceInfo, ISearchableReadOnlyList<TokensLine> documentLines, PrepareDocumentLineForUpdate prepareDocumentLineForUpdate, TypeCobolOptions compilerOptions, IList<DocumentChange<ITokensLine>> tokensLinesChanges, out int nextLineToScanIndex, out TokensLine nextLineToScan)
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
            // - LIMITATION : we don't support the case where one or more comment lines follow the current line before a continuation line 

            // Case 1 : the line is not part of a multiline continuation => we can parse it as as a standalone line
            if (!partOfMultilineContinuation)
            {
                // Create a new copy of the line before the update if necessary
                lineToScan = (TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner);
                if (lineToScanIndex == 0)
                {
                    // Scan the first line of the document
                    Scanner.ScanFirstLine(lineToScan, false, false, false, textSourceInfo.EncodingForAlphanumericLiterals, compilerOptions);
                }
                else
                {
                    // Get the scan state at the end of the previous line
                    TokensLine previousLine = documentLines[lineToScanIndex - 1];
                    // Scan the current line with this initial scan state 
                    Scanner.ScanTokensLine(lineToScan, previousLine.ScanState, compilerOptions);
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
                continuationLinesGroup.Insert(0, (TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner));

                // Navigate backwards to the start of the multiline continuation 
                if (lineToScan.Type == CobolTextLineType.Continuation && lineToScanIndex > 0)
                {
                    int revLineToScanIndex = lineToScanIndex;
                    IEnumerator<TokensLine> reversedEnumerator = documentLines.GetEnumerator(lineToScanIndex - 1, -1, true);
                    while(reversedEnumerator.MoveNext())
                    {
                        // Get the previous line until a non continuation and non comment line is encountered
                        revLineToScanIndex--;
                        lineToScan = reversedEnumerator.Current;

                        if(lineToScan.Type != CobolTextLineType.Continuation &&
                           lineToScan.Type != CobolTextLineType.Comment && lineToScan.Type != CobolTextLineType.Blank) // see p54 : for continuation, blank lines are treated like comment lines
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

                // Reuse our knowledge of the next line if it is available and if it is a continuation
                if (nextLineToScan != null && nextLineToScan.Type == CobolTextLineType.Continuation)
                {
                    lineToScanIndex++;
                    lineToScan = nextLineToScan;
                    continuationLinesGroup.Add((TokensLine)prepareDocumentLineForUpdate(lineToScanIndex, lineToScan, CompilationStep.Scanner));

                    nextLineToScanIndex = -1;
                    nextLineToScan = null;
                }

                // Navigate forwards to the end of the multiline continuation 
                if (lineToScanIndex < (documentLines.Count - 1))
                {
                    IEnumerator<TokensLine> enumerator = documentLines.GetEnumerator(lineToScanIndex + 1, -1, false);
                    while (enumerator.MoveNext())
                    {
                        // Get the next line until a non continuation and non comment line is encountered
                        lineToScanIndex++;
                        lineToScan = enumerator.Current;

                        if (lineToScan.Type == CobolTextLineType.Continuation ||
                           lineToScan.Type == CobolTextLineType.Comment || lineToScan.Type == CobolTextLineType.Blank) // see p54 : for continuation, blank lines are treated like comment lines
                        {
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

                // Scan the group of continuation lines
                if (firstLineIndex == 0)
                {
                    // Scan the first line group of the document
                    Scanner.ScanFirstLineContinuationGroup(continuationLinesGroup, false, false, false, textSourceInfo.EncodingForAlphanumericLiterals, compilerOptions);
                }
                else
                {
                    // Get the scan state at the end of the previous line
                    TokensLine previousLine = documentLines[firstLineIndex - 1];
                    // Scan the current line group with this initial scan state 
                    Scanner.ScanTokensLineContinuationGroup(continuationLinesGroup, previousLine.ScanState, compilerOptions);
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