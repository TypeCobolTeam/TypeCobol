using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Concurrency;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Iterator over tokens outside a document
    /// </summary>
    public class TokensIterator : ITokensLinesIterator
    {
        // Source data
        private string textName;
        private IList<Token> tokens;

        private int _tokenIndexInLine;
        

        // Current token
        private Token currentToken;
        private int _tokenIndexInLineSnapshot;


        /// <summary>
        /// Set the initial position of the iterator with startToken.
        /// Filter 
        /// </summary>
        public TokensIterator(string textName, IList<Token> tokens)
        {
            this.textName = textName;
            _tokenIndexInLine = 0;
            this.tokens = tokens;

        }

        /// <summary>
        /// Text name of the document where the current Token was found.
        /// If the current token was found in COPY CPY1 imported by PROGRAM PRGM1 :
        /// DocumentPath = "PGM1/CPY1"
        /// </summary>
        public string DocumentPath => textName;

        /// <summary>
        /// Current line index
        /// </summary>
        public int LineIndexInMainDocument => 0; //TODO ReplaceAndReplacing 

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
                return currentToken.StopIndex;
            }
        }

        /// <summary>
        /// Current line index
        /// </summary>
        public int LineIndex => 0; //TODO ReplaceAndReplacing 

        /// <summary>
        /// Current tokens line
        /// </summary>
        public ITokensLine CurrentLine => throw new NotImplementedException(); //TODO ReplaceAndReplacing 

        /// <summary>
        /// Returns the last token of the last line before EOF
        /// </summary>
        public ITokensLine LastLine => throw new NotImplementedException(); //TODO ReplaceAndReplacing 

        /// <summary>
        /// Resets the iterator position : before the first token of the document
        /// </summary>
        /// <param name="startLine">Zero-based index of the start line to be enumerated after reset.</param>
        protected void Reset(int startLine)
        {
            throw new NotImplementedException(); //TODO ReplaceAndReplacing 
        }
        

        /// <summary>
        /// Get next token or EndOfFile
        /// </summary>
        public Token NextToken()
        {
            // If document is empty, immediately return EndOfFile
            if (_tokenIndexInLine >= this.tokens.Count)
            {
                return Token.EndOfFile();
            }

            // While we can find a next token
            currentToken = this.tokens[_tokenIndexInLine++];
            
            return currentToken;
        }

        /// <summary>
        /// Get null (before the first call to NextToken()), current token, or EndOfFile
        /// </summary>
        public Token CurrentToken => currentToken;
        
        

        /// <summary>
        /// Get an opaque object representing the current position of the iterator.
        /// Use it with the SeekToPosition method to restore this position later.
        /// </summary>
        public object GetCurrentPosition() => _tokenIndexInLine;

        /// <summary>
        /// Sets the current iterator position to a previous position returned by GetCurrentPosition.
        /// After a call to this method, GetNextToken returns the token FOLLOWING the current position.
        /// </summary>
        public void SeekToPosition(object iteratorPosition)
        {
            _tokenIndexInLine = (int)iteratorPosition;
            currentToken = tokens[_tokenIndexInLine];
        }

        public void SeekToLineInMainDocument(int line) => Reset(line);

        /// <summary>
        /// Saves the current position of the iterator, to be able to restore it later
        /// </summary>
        public void SaveCurrentPositionSnapshot()
        {
            _tokenIndexInLineSnapshot = _tokenIndexInLine;
        }

        /// <summary>
        /// Restores the last position snapshot
        /// </summary>
        public void ReturnToLastPositionSnapshot()
        {
            SeekToPosition(_tokenIndexInLineSnapshot);
        }
    }
}
