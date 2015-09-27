﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Incrementally scan a set of text changes
    /// </summary>
    static class ScannerStep
    {
        internal static void ScanDocument()
        {
            throw new NotImplementedException();
        }

        internal static IList<DocumentChange<ITokensLine>> ScanDocumentChanges(IEnumerable<DocumentChange<ICobolTextLine>> textLineChanges)
        {
            try
            {
                TokensChangedEvent tokensChangedEvent = new TokensChangedEvent();

                // Optimization : use a builder to update the immutable list in case of a big update                
                ImmutableList<ITokensLine>.Builder tokensLinesBuilder = null;
                if (textChangedEvent.TextChanges.Count > 4)
                {
                    tokensLinesBuilder = tokensLines.ToBuilder();
                }
                Func<int, TokensLine> getTokensLineAtIndex = index => { if (tokensLinesBuilder != null) { return (TokensLine)tokensLinesBuilder[index]; } else { return (TokensLine)tokensLines[index]; } };
                Action<int, TokensLine> setTokensLineAtIndex = (index, updatedLine) => { if (tokensLinesBuilder != null) { tokensLinesBuilder[index] = updatedLine; } else { tokensLines = tokensLines.SetItem(index, updatedLine); } };
                Action<int, TokensLine> insertTokensLineAtIndex = (index, insertedLine) => { if (tokensLinesBuilder != null) { tokensLinesBuilder.Insert(index, insertedLine); } else { tokensLines = tokensLines.Insert(index, insertedLine); } };
                Action<TokensLine> removeTokensLine = removedLine => { if (tokensLinesBuilder != null) { tokensLinesBuilder.Remove(removedLine); } else { tokensLines = tokensLines.Remove(removedLine); } };
                Action clearTokensLines = () => { if (tokensLinesBuilder != null) { tokensLinesBuilder.Clear(); } else { tokensLines = ImmutableList<ITokensLine>.Empty; } };

                // Analyze all text changes
                int lastTextChangeIndex = textChangedEvent.TextChanges.Count - 1;
                for (int textChangeIndex = 0; textChangeIndex <= lastTextChangeIndex; textChangeIndex++)
                {
                    // Get the current text change
                    TextChange textChange = textChangedEvent.TextChanges[textChangeIndex];

                    // Check if it is useful to propagate change to the following lines ?
                    // Principle of the propagation :
                    // - the initial scan state at the beginning of each line must be initialized with the last scan state at the end of the previous line 
                    // - when a line is inserted or updated (at index i)
                    //   => the initial state of the following line (at index i+1) must be updated 
                    //      with the last scan state of the newly modified line (at index i)
                    // - when a line is removed (at index i)
                    //   => the initial state of the following line (now replacing the removed line at index i) must be updated 
                    //      with the last scan state of the previous line (at index i-1)
                    // It is not useful to propagate immediately the current change if we know that the following change
                    // will update either the last scan state of the first line or the initial state of the second line to compare.
                    bool propagateChangeToFollowingLines = true;
                    if (textChangeIndex < lastTextChangeIndex)
                    {
                        TextChange nextTextChange = textChangedEvent.TextChanges[textChangeIndex + 1];

                        if (((textChange.Type == TextChangeType.LineInserted || textChange.Type == TextChangeType.LineUpdated) &&
                             (nextTextChange.LineIndex == textChange.LineIndex || nextTextChange.LineIndex == (textChange.LineIndex + 1))) ||
                            (textChange.Type == TextChangeType.LineRemoved &&
                             (nextTextChange.LineIndex == (textChange.LineIndex - 1) || nextTextChange.LineIndex == textChange.LineIndex)) ||
                            nextTextChange.Type == TextChangeType.DocumentCleared)
                        {
                            propagateChangeToFollowingLines = false;
                        }
                    }

                    switch (textChange.Type)
                    {
                        // --- Case 1 : text document cleared ---
                        case TextChangeType.DocumentCleared:
                            // Reset the immutable list
                            clearTokensLines();
                            // Register a DocumentCleared change
                            tokensChangedEvent.TokensChanges.Add(new TokensChange(TokensChangeType.DocumentCleared, 0, null));
                            break;
                        // --- Case 2 : line inserted in the text document ---
                        case TextChangeType.LineInserted:
                            // Scan the newly inserted line
                            TokensLine insertedLine = null;
                            if (textChange.LineIndex > 0)
                            {
                                // If it was not the first line : continue with the scan state of the previous line
                                TokensLine previousLine = getTokensLineAtIndex(textChange.LineIndex - 1);
                                insertedLine = Scanner.ScanTextLine(textChange.NewLine, previousLine, TextSourceInfo, CompilerOptions);
                            }
                            else
                            {
                                // If it was the first line : initialize a new scan state
                                insertedLine = Scanner.ScanFirstLine(textChange.NewLine, false, false, false, TextSourceInfo, CompilerOptions);
                            }
                            // Insert a new line in the immutable list
                            insertTokensLineAtIndex(textChange.LineIndex, insertedLine);
                            // Register a LineInserted change
                            tokensChangedEvent.TokensChanges.Add(new TokensChange(TokensChangeType.LineInserted, textChange.LineIndex, insertedLine));
                            // See if the following lines need to be scanned again because the scan state changed
                            if (propagateChangeToFollowingLines)
                            {
                                PropagateChangeAfterLine(textChange.LineIndex, insertedLine.ScanState, getTokensLineAtIndex, setTokensLineAtIndex, tokensChangedEvent);
                            }
                            break;
                        case TextChangeType.LineUpdated:
                            // Scan the updated line
                            TokensLine updatedLine = null;
                            if (textChange.LineIndex > 0)
                            {
                                // If it was not the first line : continue with the scan state of the previous line
                                TokensLine previousLine = getTokensLineAtIndex(textChange.LineIndex - 1);
                                updatedLine = Scanner.ScanTextLine(textChange.NewLine, previousLine, TextSourceInfo, CompilerOptions);

                                // If the updated line is a continuation line, the last token of previous line may also
                                // have been updated as part of the continuation => signal this change
                                if (updatedLine.Type == CobolTextLineType.Continuation)
                                {
                                    for (int previousLineIndex = textChange.LineIndex - 1; previousLineIndex >= 0; previousLineIndex--)
                                    {
                                        // Get previous line
                                        previousLine = getTokensLineAtIndex(previousLineIndex);
                                        // Signal change on the previous line
                                        tokensChangedEvent.TokensChanges.Add(new TokensChange(TokensChangeType.LineRescanned, previousLineIndex, previousLine));
                                        // Continue to iterate backward until the end of the continuation set
                                        if (previousLine.Type != CobolTextLineType.Continuation)
                                        {
                                            break;
                                        }
                                    }
                                }
                            }
                            else
                            {
                                // If it was the first line : initialize a new scan state
                                updatedLine = Scanner.ScanFirstLine(textChange.NewLine, false, false, false, TextSourceInfo, CompilerOptions);
                            }
                            // Update the line in the immutable list
                            setTokensLineAtIndex(textChange.LineIndex, updatedLine);
                            // Register a LineUpdated change
                            tokensChangedEvent.TokensChanges.Add(new TokensChange(TokensChangeType.LineUpdated, textChange.LineIndex, updatedLine));
                            // See if the following lines need to be scanned again because the scan state changed
                            if (propagateChangeToFollowingLines)
                            {
                                PropagateChangeAfterLine(textChange.LineIndex, updatedLine.ScanState, getTokensLineAtIndex, setTokensLineAtIndex, tokensChangedEvent);
                            }
                            break;
                        case TextChangeType.LineRemoved:
                            // Remove the line from the immutable list
                            TokensLine removedLine = getTokensLineAtIndex(textChange.LineIndex);
                            removeTokensLine(removedLine);
                            // Register a LineRemoved change
                            tokensChangedEvent.TokensChanges.Add(new TokensChange(TokensChangeType.LineRemoved, textChange.LineIndex, removedLine));
                            // See if the following lines need to be scanned again because the scan state changed                            
                            if (propagateChangeToFollowingLines)
                            {
                                MultilineScanState previousScanState = null;
                                if (textChange.LineIndex > 0)
                                {
                                    // If it was not the first line : get the scan state of the previous line
                                    TokensLine previousLine = getTokensLineAtIndex(textChange.LineIndex - 1);
                                    previousScanState = previousLine.ScanState;
                                }
                                else
                                {
                                    // If it was the first line : get the initial scan state for the file
                                    previousScanState = removedLine.InitialScanState;
                                }
                                PropagateChangeAfterLine(textChange.LineIndex - 1, previousScanState, getTokensLineAtIndex, setTokensLineAtIndex, tokensChangedEvent);
                            }
                            break;
                    }
                }

                // End of optimization : revert the builder to an immutable list
                if (tokensLinesBuilder != null)
                {
                    tokensLines = tokensLinesBuilder.ToImmutable();
                }

                // Send all the events in one batch to the next step of the pipeline
                tokensChangedEventsSource.OnNext(tokensChangedEvent);
            }
            catch (Exception ex)
            {
                // Register and forward errors
                LastException = ex;
                tokensChangedEventsSource.OnError(ex);
            }
        }

        /// <summary>
        /// Rescan all the lines for which the initial scan state has been modified by a text update on a previous line
        /// </summary>
        private void PropagateChangeAfterLine(int updatedLineIndex, MultilineScanState lastScanState, Func<int, TokensLine> getTokensLineAtIndex, Action<int, TokensLine> setTokensLineAtIndex, TokensChangedEvent tokensChangedEvent)
        {
            // Study all the lines following one line change
            for (int lineIndex = updatedLineIndex + 1; lineIndex < tokensLines.Count; lineIndex++)
            {
                TokensLine currentLine = (TokensLine)tokensLines[lineIndex];
                // As soon as we find a line where the initial scan state is not modified
                if (currentLine.InitialScanState.Equals(lastScanState))
                {
                    // -> STOP the propagation of the change
                    break;
                }
                else
                {
                    // Else we need to re-scan the line with a new initial state
                    TokensLine updatedLine = null;
                    if (lineIndex > 0)
                    {
                        // If it was not the first line : continue with the scan state of the previous line
                        TokensLine previousLine = getTokensLineAtIndex(lineIndex - 1);
                        updatedLine = Scanner.ScanTextLine(currentLine.TextLine, previousLine, TextSourceInfo, CompilerOptions);
                    }
                    else
                    {
                        // If it was the first line : initialize a new scan state
                        updatedLine = Scanner.ScanFirstLine(currentLine.TextLine, false, false, false, TextSourceInfo, CompilerOptions);
                    }
                    // Update the line in the immutable list
                    setTokensLineAtIndex(lineIndex, updatedLine);

                    // Adjust the last scan state
                    lastScanState = updatedLine.ScanState;

                    // Register a LineRescanned change
                    tokensChangedEvent.TokensChanges.Add(new TokensChange(TokensChangeType.LineRescanned, lineIndex, updatedLine));
                }
            }
        }
    }
}
