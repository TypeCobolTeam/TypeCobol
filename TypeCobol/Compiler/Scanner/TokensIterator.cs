using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Iterator over tokens outside a document
    /// </summary>
    public class TokensIterator : ITokensLinesIterator
    {
        // Source data
        private readonly string _textName;
        private readonly IList<Token> _tokens;

        // Position
        private int _tokenIndex;
        private Token _currentToken;

        // Saved position
        private int _tokenIndexSnapshot;

        /// <summary>
        /// Constructor
        /// </summary>
        public TokensIterator(string textName, IList<Token> tokens)
        {
            _textName = textName;
            _tokens = tokens;
        }

        /// <summary>
        /// Text name of the document where the current Token was found.
        /// If the current token was found in COPY CPY1 imported by PROGRAM PGM1 :
        /// DocumentPath = "PGM1/CPY1"
        /// </summary>
        public string DocumentPath => _textName;

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
                if (_currentToken == null)
                {
                    return 0;
                }
                return _currentToken.StopIndex;
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
        /// Get next token or EndOfFile
        /// </summary>
        public Token NextToken()
        {
            // If document is empty, immediately return EndOfFile
            if (_tokenIndex >= this._tokens.Count)
            {
                return Token.EndOfFile();
            }

            // While we can find a next token
            _currentToken = this._tokens[_tokenIndex++];
            
            return _currentToken;
        }

        /// <summary>
        /// Get null (before the first call to NextToken()), current token, or EndOfFile
        /// </summary>
        public Token CurrentToken => _currentToken;

        /// <summary>
        /// Get an opaque object representing the current position of the iterator.
        /// Use it with the SeekToPosition method to restore this position later.
        /// </summary>
        public object GetCurrentPosition() => _tokenIndex;

        /// <summary>
        /// Sets the current iterator position to a previous position returned by GetCurrentPosition.
        /// After a call to this method, GetNextToken returns the token FOLLOWING the current position.
        /// </summary>
        public void SeekToPosition(object iteratorPosition)
        {
            _tokenIndex = (int)iteratorPosition;
            _currentToken = _tokens[_tokenIndex];
        }

        public void SeekToLineInMainDocument(int line) => throw new NotImplementedException(); //TODO ReplaceAndReplacing

        /// <summary>
        /// Saves the current position of the iterator, to be able to restore it later
        /// </summary>
        public void SaveCurrentPositionSnapshot()
        {
            _tokenIndexSnapshot = _tokenIndex;
        }

        /// <summary>
        /// Restores the last position snapshot
        /// </summary>
        public void ReturnToLastPositionSnapshot()
        {
            SeekToPosition(_tokenIndexSnapshot);
        }
    }
}
