using Antlr4.Runtime;
using System;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Override of CommonTokenStream to return a non-empty text for token intervals
    /// </summary>
    public class TokensLinesTokenStream : CommonTokenStream
    {
        public TokensLinesTokenStream(ITokenSource tokenSource, int channel) : base(tokenSource, channel)
        { }

        /// <summary>
        /// In our implementation, Token.TokenIndex always returns -1.
        /// We can not insert all the intermediate tokens like Antlr does in the default implementation.
        /// This method just returns the text of the starting and ending tokens.
        /// </summary>
        public override string GetText(IToken start, IToken stop)
        {
            if (start != null || stop != null)
            {
                if (Equals(start, stop))
                {
                    return start.Text;
                }
                else
                {
                    return start?.Text + " ... " + stop.Text;
                }
            }
            return String.Empty;
        }

        /// <summary>
        /// Advance the stream to one specific token.
        /// <return>false if the token is not found, true otherwise</return>
        /// </summary>
        public bool SeekToToken(IToken searchedToken)
        {
            Seek(0);
            ResetStopTokenLookup();

            // TO DO : optimize this naive implementation
            // Not easy because of the underlying Copy and Replace iterators            
            if (searchedToken != null)
            {
                var currentToken = Lt(1);
                while (!currentToken.Equals(searchedToken) && currentToken.Type != TokenConstants.Eof)
                {
                    Consume();
                    currentToken = Lt(1);
                }
                if (!currentToken.Equals(searchedToken) && searchedToken.Type != TokenConstants.Eof)
                {
                    // See GitHub #2053:
                    // Assert here the problem in debug mode.
                    // Avoid to throw an uncaught exception in a bad context, return false.
                    System.Diagnostics.Debug.Assert(false, "Token not found in this stream");
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        /// Start monitoring if the token stream reached a specific token which marks the end of an interesting section
        /// </summary>
        public void StartLookingForStopToken(Token stopToken)
        {
            ResetStopTokenLookup();
            if (stopToken != null && stopToken.Type != TokenConstants.Eof)
            {
                _stopToken = stopToken;
                _stopTokenReplacedByEOF = new ReplacedToken(Token.EndOfFile(), stopToken);

                // Check tokens already fetched
                for (int index = tokens.Count - 1; index >= 0; index--)
                {
                    if (stopToken.Equals(tokens[index]))
                    {
                        tokens[index] = _stopTokenReplacedByEOF;
                        _indexOfStopTokenReplacedByEOF = index;
                        fetchedEOF = true;
                        break;
                    }
                }
            }
        }

        /// <summary>
        /// Cancel a previous replacement of stop token by EOF
        /// </summary>
        public void ResetStopTokenLookup()
        {
            // Reset replacement of stop token by EOF
            if (_indexOfStopTokenReplacedByEOF >= 0)
            {
                System.Diagnostics.Debug.Assert(_indexOfStopTokenReplacedByEOF < tokens.Count);
                System.Diagnostics.Debug.Assert(tokens[_indexOfStopTokenReplacedByEOF] != null && tokens[_indexOfStopTokenReplacedByEOF].Equals(_stopTokenReplacedByEOF));

                tokens[_indexOfStopTokenReplacedByEOF] = _stopTokenReplacedByEOF.OriginalToken;
                fetchedEOF = false;
                _indexOfStopTokenReplacedByEOF = -1;
            }
        }

        /// <summary>
        /// Token which marks the end of an interesting code section
        /// </summary>
        private IToken _stopToken;

        // EOF token which replaces the original stop token
        private ReplacedToken _stopTokenReplacedByEOF;

        // Index of the replaced stop token in the buffer
        private int _indexOfStopTokenReplacedByEOF = -1;

        /// <summary>
        /// Override the original Fetch method from BufferedTokenStream : same behavior,
        /// except that stop token is replaced with EOF on the fly.
        /// </summary>
        protected override int Fetch(int n)
        {
            if (fetchedEOF)
            {
                return 0;
            }

            for (int i = 0; i < n; i++)
            {
                IToken t = TokenSource.NextToken();
                if (t is IWritableToken writableToken)
                {
                    writableToken.TokenIndex = tokens.Count;
                }

                // >>> replacement added
                if (_stopToken != null && _stopToken.Equals(t))
                {
                    t = _stopTokenReplacedByEOF;
                    _indexOfStopTokenReplacedByEOF = tokens.Count;
                }
                // <<< end of replacement

                tokens.Add(t);
                if (t.Type == TokenConstants.Eof)
                {
                    fetchedEOF = true;
                    return i + 1;
                }
            }

            return n;
        }
    }
}
