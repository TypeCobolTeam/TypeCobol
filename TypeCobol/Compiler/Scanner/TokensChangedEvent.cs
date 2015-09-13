using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// A tokens line in the compilation unit can be inserted, updated, or removed.
    /// The whole document can be cleared of any content and all the tokens lines reset.
    /// A tokens line can also be rescanned because an ajacent line was updated and the scan state was modified.
    /// </summary>
    public enum TokensChangeType
    {        
        LineInserted,
        LineUpdated,
        LineRemoved,
        DocumentCleared,
        LineRescanned
    }

    /// <summary>
    /// Tokens changes are tracked at the line level.
    /// This class models a simple change on one line of tokens.
    /// </summary>
    public class TokensChange
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="type">Type of change applied to the line</param>
        /// <param name="lineIndex">Index of the line which was changed</param>
        /// <param name="newLine">New line content after the update (null in case of a LineRemoved event)</param>
        public TokensChange(TokensChangeType type, int lineIndex, ITokensLine newLine)
        {
            Type = type;
            LineIndex = lineIndex;
            NewLine = newLine;
        }

        /// <summary>
        /// LineInserted, LineUpdated, or LineRemoved
        /// </summary>
        public TokensChangeType Type { get; private set; }

        /// <summary>
        /// If a new line is inserted at index 2, the line previously stored at index 2 is now at index 3, and son on ...
        /// If the line at index 2 is removed, the previous previously stored at index 3 is now at index 2, and son on ...
        /// </summary>
        public int LineIndex { get; private set; }

        /// <summary>
        /// New line content after the update (null in case of a LineRemoved event)
        /// </summary>
        public ITokensLine NewLine { get; private set; }
    }

    /// <summary>
    /// Tokens changes are tracked at the line level.
    /// This class models a change which can span several lines of tokens.
    /// </summary>
    public class TokensChangedEvent
    {
        public TokensChangedEvent()
        {
            TokensChanges = new List<TokensChange>();
        }

        /// <summary>
        /// List of tokens lines which were simultaneoulsy changed in the compilation unit
        /// </summary>
        public IList<TokensChange> TokensChanges { get; private set; }

        /// <summary>
        /// Compute the indexes of all the lines which will have been updated after applying all the changes
        /// (indexes in the final resulting document)
        /// </summary>
        public IList<int> GetIndexesOfLinesUpdatedAfterThisEvent(TokensChangeType changeTypeFilter)
        {
            if(changeTypeFilter == TokensChangeType.LineRemoved)
            {
                throw new InvalidOperationException("Removed lines are not part of this document anymore after the event");
            }

            IList<int> updatedLinesIndexes = new List<int>();
            foreach(TokensChange change in TokensChanges)
            {
                if(change.Type == TokensChangeType.LineInserted)
                {
                    int insertedLineIndex = change.LineIndex;
                    for(int i = 0 ; i < updatedLinesIndexes.Count ; i++)
                    {
                        int lineIndex = updatedLinesIndexes[i];
                        if(lineIndex >= insertedLineIndex)
                        {
                            updatedLinesIndexes[i] = updatedLinesIndexes[i] + 1;
                        }
                    }
                    if(changeTypeFilter == TokensChangeType.LineInserted)
                    {
                        updatedLinesIndexes.Add(insertedLineIndex);
                    }
                }
                else if (change.Type == TokensChangeType.LineRemoved)
                {
                    int removedLineIndex = change.LineIndex;
                    updatedLinesIndexes.Remove(removedLineIndex);
                    for (int i = 0; i < updatedLinesIndexes.Count; i++)
                    {
                        int lineIndex = updatedLinesIndexes[i];
                        if (lineIndex > removedLineIndex)
                        {
                            updatedLinesIndexes[i] = updatedLinesIndexes[i] - 1;
                        }
                    }
                }
                else if (change.Type == TokensChangeType.LineUpdated)
                {                    
                    if (changeTypeFilter == TokensChangeType.LineUpdated)
                    {
                        updatedLinesIndexes.Add(change.LineIndex);
                    }
                }
                else if (change.Type == TokensChangeType.LineRescanned)
                {
                    if (changeTypeFilter == TokensChangeType.LineRescanned)
                    {
                        updatedLinesIndexes.Add(change.LineIndex);
                    }
                }
            }            
            return updatedLinesIndexes; 
        }

        /// <summary>
        /// Useful when buffering change events
        /// </summary>
        public static TokensChangedEvent Flatten(IList<TokensChangedEvent> tokensChangedEventsList)
        {
            TokensChangedEvent resultTokensChangedEvent = new TokensChangedEvent();
            foreach(TokensChangedEvent inputTokensChangedEvent in tokensChangedEventsList)
            {
                foreach(TokensChange tokensChange in inputTokensChangedEvent.TokensChanges)
                {
                    resultTokensChangedEvent.TokensChanges.Add(tokensChange);
                }
            }
            return resultTokensChangedEvent;
        }
    }
}
