using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{    
    /// <summary>
    /// Iterator over tokens stored in TokensLines
    /// and originating from a single document
    /// </summary>
    public class TokensLinesIterator : ITokensLinesIterator
    {
        // Source data
        private string textName;
        private ISearchableReadOnlyList<ITokensLine> tokensLines;

        // Start conditions
        private Token startToken;
        private int channelFilter;

        // Iterator position
        private struct TokensLineIteratorPosition
        {
            public int LineIndex;           
            public int TokenIndexInLine;
        }

        // Current iterator position
        private TokensLineIteratorPosition currentPosition;

        // Current line and token
        private ITokensLine currentLine;
        private Token currentToken;

        // Previous snapshot position
        private object snapshotPosition;

        /// <summary>
        /// Set the initial position of the iterator with startToken.
        /// Filter 
        /// </summary>
        public TokensLinesIterator(string textName, ISearchableReadOnlyList<ITokensLine> tokensLines, Token startToken, int channelFilter)
        {
            this.textName = textName;
            this.tokensLines = tokensLines;

            this.startToken = startToken;
            this.channelFilter = channelFilter;

            // Start just before the first token in the document
            if (startToken == null)
            {
                Reset();
            }
            // Start iterating just before start token, in the middle of the document
            else
            {
                SeekToToken(startToken);
                PreviousToken();
            }
        }

        /// <summary>
        /// Text name of the document where the current Token was found.
        /// If the current token was found in COPY CPY1 imported by PROGRAM PRGM1 :
        /// DocumentPath = "PGM1/CPY1"
        /// </summary>
        public string DocumentPath 
        {
            get            
            {
                return textName;
            }
        }

        /// <summary>
        /// Current line index
        /// </summary>
        public int LineIndexInMainDocument
        {
            get
            {
                return currentPosition.LineIndex;
            }
        }

        /// <summary>
        /// Current column index
        /// </summary>
        public int ColumnIndex
        {
            get
            {
                if (currentToken == null)
                {
                    return 0;
                }
                else
                {
                    return currentToken.StopIndex;
                }
            }
        }

        /// <summary>
        /// Current line index
        /// </summary>
        public int LineIndex
        {
            get
            {
                return currentPosition.LineIndex;
            }
        }

        /// <summary>
        /// Current tokens line
        /// </summary>
        public ITokensLine CurrentLine
        {
            get
            {
                return currentLine;
            }
        }

        /// <summary>
        /// Returns the last token of the last line before EOF
        /// </summary>
        public ITokensLine LastLine
        {
            get
            {
                if (tokensLines.Count > 0)
                {
                    var lastLineIndex = tokensLines.Count - 1;
                    return tokensLines[lastLineIndex];
                }
                else
                {
                    return null;
                }
            }
        }

        /// <summary>
        /// Resets the iterator position : before the first token of the document
        /// </summary>
        public void Reset()
        {
            currentPosition.LineIndex = 0;
            if (tokensLines.Count > 0)
            {
                currentLine = tokensLines[currentPosition.LineIndex];
            }
            currentPosition.TokenIndexInLine = -1;
            currentToken = null;
        }

        /// <summary>
        /// Sets the current iterator position to point to a specific token.
        /// After a call to this method, GetNextToken returns the token FOLLOWING startToken.
        /// </summary>
        public void SeekToToken(Token startToken)
        {
            // Find line for the start token
            currentPosition.LineIndex = tokensLines.IndexOf(startToken.TokensLine, startToken.TokensLine.LineIndex);
            currentLine = startToken.TokensLine;
            // Find index in line for the start token
            currentPosition.TokenIndexInLine = currentLine.SourceTokens.IndexOf(startToken);
            currentToken = startToken;
        }

        /// <summary>
        /// Get next token or EndOfFile
        /// </summary>
        public Token NextToken()
        {
            return NextToken(true);
        }

        /// <summary>
        /// Get null (before the first call to NextToken()), current token, or EndOfFile
        /// </summary>
        public Token CurrentToken
        {
            get { return currentToken; }
        }

        /// <summary>
        /// Get the next token, with or without applying the channel filter
        /// </summary>
        public Token NextToken(bool applyChannelFilter)
        {
            // If document is empty, immediately return EndOfFile
            if (currentLine == null)
            {
                return Token.END_OF_FILE;
            }

            // While we can find a next token
            currentToken = null;
            while (currentToken == null)
            {
                // try to find the next token on the same line
                currentPosition.TokenIndexInLine++;
                // but if we reached the end of the current line ...
                while (currentPosition.TokenIndexInLine >= currentLine.SourceTokens.Count)
                {
                    // .. advance to next line
                    currentPosition.LineIndex++;
                    currentPosition.TokenIndexInLine = 0;
                    if (currentPosition.LineIndex < tokensLines.Count)
                    {
                        // TO DO - OPTIMIZATION : should we use here an Enumerator on tokensLines ?
                        currentLine = tokensLines[currentPosition.LineIndex];
                    }
                    // and if we reached the last line of the document ...
                    else
                    {
                        // return EndOfFile
                        currentLine = null;
                        return Token.END_OF_FILE;
                    }
                }
                // Check if the next token found matches the filter criteria
                Token nextTokenCandidate = currentLine.SourceTokens[currentPosition.TokenIndexInLine];
                if (!applyChannelFilter || nextTokenCandidate.Channel == channelFilter)
                {
                    currentToken = nextTokenCandidate;
                }
            }
            // found a next token matching the filter criteria
            return currentToken;
        }

        /// <summary>
        /// Get previous token or EndOfFile
        /// </summary>
        public Token PreviousToken()
        {
            return PreviousToken(true);
        }

        /// <summary>
        /// Get the previous token, with or without applying the channel filter
        /// </summary>
        public Token PreviousToken(bool applyChannelFilter)
        {
            // If the iterator already points before the first token :
            // impossible to get previous token, do nothing
            if (currentPosition.LineIndex == 0 && currentPosition.TokenIndexInLine == -1)
            {
                return null;
            }

            // Look for previous tokens until we find one that matches the filter
            for (; ; )
            {
                // Try to get the previous token on the same line
                currentPosition.TokenIndexInLine -= 1;
                // Current token was not the first token on his line 
                if (currentPosition.TokenIndexInLine >= 0)
                {
                    // => simply return previous token on the same line
                    currentToken = currentLine.SourceTokens[currentPosition.TokenIndexInLine];
                }
                // Current token was the first token on the line
                // => look for a previous token on the preceding line
                else
                {
                    // If there is a preceding line
                    if (currentPosition.LineIndex > 0)
                    {
                        // => find the first preceding line which is not empty
                        while (currentPosition.LineIndex > 0)
                        {
                            currentPosition.LineIndex = currentPosition.LineIndex - 1;
                            // TO DO - OPTIMIZATION - IMPORTANT : here, we should use an Enumerator on tokensLines
                            currentLine = tokensLines[currentPosition.LineIndex];
                            if (currentLine.SourceTokens.Count > 0) break;
                        }
                        // If such a line was found :
                        if (currentLine.SourceTokens.Count > 0)
                        {
                            // Select the last token on this line
                            currentPosition.TokenIndexInLine = currentLine.SourceTokens.Count - 1;
                            currentToken = currentLine.SourceTokens[currentPosition.TokenIndexInLine];
                        }
                        else
                        {
                            // => position the iterator before the first token of the document
                            currentPosition.TokenIndexInLine = -1;
                            currentToken = null;
                            break;
                        }
                    }
                    // If there is no preceding line
                    else
                    {
                        // => position the iterator before the first token of the document
                        currentPosition.TokenIndexInLine = -1;
                        currentToken = null;
                        break;
                    }
                }
                // Check if the previous token found matches the filter criteria                
                if (!applyChannelFilter || currentToken == null | currentToken.Channel == channelFilter)
                {
                    break;
                }
            }
            return currentToken;
        }


        /// <summary>
        /// Get an opaque object representing the current position of the iterator.
        /// Use it with the SeekToPosition method to restore this position later.
        /// </summary>
        public object GetCurrentPosition()
        {
            return currentPosition;
        }
        
        /// <summary>
        /// Sets the current iterator position to a previous position returned by GetCurrentPosition.
        /// After a call to this method, GetNextToken returns the token FOLLOWING the current position.
        /// </summary>
        public void SeekToPosition(object iteratorPosition)
        {
            currentPosition = (TokensLineIteratorPosition)iteratorPosition;
            if (currentPosition.LineIndex >= 0 && currentPosition.LineIndex < tokensLines.Count)
            {
                currentLine = tokensLines[currentPosition.LineIndex];
            }
            else
            {
                currentLine = null;
            }
            if (currentPosition.TokenIndexInLine >= 0 && currentLine != null)
            {
                currentToken = currentLine.SourceTokens[currentPosition.TokenIndexInLine];
            }
            else
            {
                currentToken = null;
            }
        }

        /// <summary>
        /// Saves the current position of the iterator, to be able to restore it later
        /// </summary>
        public void SaveCurrentPositionSnapshot()
        {
            snapshotPosition = GetCurrentPosition();
        }

        /// <summary>
        /// Restores the last position snapshot
        /// </summary>
        public void ReturnToLastPositionSnapshot()
        {
            SeekToPosition(snapshotPosition);
        }

        /// <summary>
        /// Returns all tokens (no channel filter), line per line, selected between startToken and stopToken.
        /// This method takes a snapshot of the current position before its execution and restores it before returning.
        /// </summary>
        public MultilineTokensGroupSelection SelectAllTokensBetween(Token startToken, Token stopToken)
        {
            // Check parameters
            ITokensLine startLine = startToken.TokensLine;
            int startLineIndex = tokensLines.IndexOf(startLine, startLine.LineIndex);
            ITokensLine stopLine = stopToken.TokensLine;
            int stopLineIndex = tokensLines.IndexOf(stopLine, stopLine.LineIndex);
            if (startLineIndex < 0 || stopLineIndex < 0 || stopLineIndex < startLineIndex || 
                ((startLineIndex == stopLineIndex) && (stopToken.StartIndex < startToken.StartIndex)))
            {
                throw new InvalidOperationException("Invalid start or stop token : line or columns number do not define a valid selection interval");
            }

            // Save iterator position (to restore it before return)
            SaveCurrentPositionSnapshot();

            // Seek to the first token at the start of the line where startToken appears
            currentPosition.LineIndex = startLineIndex;
            currentLine = startToken.TokensLine;
            currentPosition.TokenIndexInLine = 0;
            currentToken = currentLine.SourceTokens[0]; 

            // List of tokens not selected on the first line before startToken
            IList<Token> tokensOnFirstLineBeforeStartToken = null;
            if(currentToken != startToken)
            {
                tokensOnFirstLineBeforeStartToken = new List<Token>();
                do
                {
                    tokensOnFirstLineBeforeStartToken.Add(currentToken);
                }
                while (NextToken(false) != startToken);
            }

            // List of tokens selected ...
            int numberOfSelectedLines = stopLineIndex - startLineIndex + 1;
            IList<Token>[] selectedTokensOnSeveralLines = new IList<Token>[numberOfSelectedLines];
            // ... on the first line
            IList<Token> tokensSelectedOnFirstLine = new List<Token>();
            selectedTokensOnSeveralLines[0] = tokensSelectedOnFirstLine;
            if(numberOfSelectedLines == 1)
            {
                if (currentToken == stopToken)
                {
                    tokensSelectedOnFirstLine.Add(currentToken);
                }
                else
                {
                    do
                    {
                        tokensSelectedOnFirstLine.Add(currentToken);
                    }
                    while (NextToken(false) != stopToken);
                    tokensSelectedOnFirstLine.Add(currentToken);
                }
                NextToken(false);
            }
            else
            {
                do
                {
                    tokensSelectedOnFirstLine.Add(currentToken);
                }
                while (NextToken(false).TokensLine == startLine);
            }
            // ... on intermediate lines
            // => optimization : reuse the complete lists of the source lines
            if(numberOfSelectedLines > 2)
            {
                for(int intermediateLineIndex = startLineIndex+1 ; intermediateLineIndex < stopLineIndex ; intermediateLineIndex++)
                {
                    // TO DO - OPTIMIZATION - IMPORTANT : here, we should use an Enumerator on tokensLines
                    selectedTokensOnSeveralLines[intermediateLineIndex - startLineIndex] = tokensLines[intermediateLineIndex].SourceTokens;
                }
                currentPosition.LineIndex = stopLineIndex;
                currentLine = tokensLines[currentPosition.LineIndex];
                currentPosition.TokenIndexInLine = 0;
                currentToken = currentLine.SourceTokens[0];
            }
            // ... on the last line
            if (numberOfSelectedLines > 1)
            {
                IList<Token> tokensSelectedOnLastLine = new List<Token>();
                selectedTokensOnSeveralLines[numberOfSelectedLines - 1] = tokensSelectedOnLastLine;
                if (currentToken == stopToken)
                {
                    tokensSelectedOnLastLine.Add(currentToken);
                }
                else
                {
                    do
                    {
                        tokensSelectedOnLastLine.Add(currentToken);
                    }
                    while (NextToken(false) != stopToken);
                    tokensSelectedOnLastLine.Add(currentToken);
                }
                NextToken(false);
            }

            // List of tokens not selected on the last line after stopToken
            IList<Token> tokensOnLastLineAfterStopToken = null;
            if (currentToken != null && currentToken.TokensLine == stopToken.TokensLine)
            {
                Token lastTokenOfLastLine = currentLine.SourceTokens[currentLine.SourceTokens.Count - 1];
                if (stopToken != lastTokenOfLastLine)
                {
                    tokensOnLastLineAfterStopToken = new List<Token>();
                    if (currentToken == lastTokenOfLastLine)
                    {
                        tokensOnLastLineAfterStopToken.Add(currentToken);
                    }
                    else 
                    { 
                        do
                        {
                            tokensOnLastLineAfterStopToken.Add(currentToken);
                        }
                        while (NextToken(false) != lastTokenOfLastLine);
                        tokensOnLastLineAfterStopToken.Add(currentToken);
                    }
                }
            }

            // Restore initial iterator position before return
            ReturnToLastPositionSnapshot();

            return new MultilineTokensGroupSelection(startLineIndex,
                tokensOnFirstLineBeforeStartToken,
                selectedTokensOnSeveralLines,
                tokensOnLastLineAfterStopToken);
        }
    }

    /// <summary>
    /// Example :
    /// - three TokensLine
    /// - X = selected tokens
    /// - A/B = unselected tokens
    /// [ B B B X X X ]
    /// [ X X X X X X ]
    /// [ X X A A A A ]
    /// Then :
    /// - TokensOnFirstLineBeforeStartToken = <B,B,B>
    /// - TokensOnLinesBetweenStartTokenAndStopToken = <<X,X,X>,<X,X,X,X,X,X>,<X,X>>
    /// - TokensOnLastLineAfterStopToken = <A,A,A,A>
    /// </summary>
    public class MultilineTokensGroupSelection
    {
        public MultilineTokensGroupSelection(
            int firstLineIndex,
            IList<Token> tokensOnFirstLineBeforeStartToken,
            IList<Token>[] selectedTokensOnSeveralLines,
            IList<Token> tokensOnLastLineAfterStopToken)
        {
            FirstLineIndex = firstLineIndex;
            TokensOnFirstLineBeforeStartToken = tokensOnFirstLineBeforeStartToken;
            SelectedTokensOnSeveralLines = selectedTokensOnSeveralLines;
            TokensOnLastLineAfterStopToken = tokensOnLastLineAfterStopToken;
        }

        public int FirstLineIndex { get; private set; }

        public IList<Token> TokensOnFirstLineBeforeStartToken { get; private set; }

        public IList<Token>[] SelectedTokensOnSeveralLines { get; private set; }

        public IList<Token> TokensOnLastLineAfterStopToken { get; private set; }
    }
}
