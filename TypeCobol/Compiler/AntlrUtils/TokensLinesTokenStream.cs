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

                // Check tokens already fetched and not yet consumed
                if (Index < 0)
                {
                    // Nothing fetched yet
                    return;
                }

                for (int index = Index; index < tokens.Count; index++)
                {
                    if (stopToken.Equals(tokens[index]))
                    {
                        tokens[index] = _stopTokenReplacedByEOF;
                        fetchedEOF = true;
                    }
                }
            }
        }

        /// <summary>
        /// Cancel a previous replacement of stop token by EOF
        /// </summary>
        public void ResetStopTokenLookup()
        {
            if (_stopToken == null)
            {
                // No stop token defined, nothing to do
                System.Diagnostics.Debug.Assert(_stopTokenReplacedByEOF == null);
                return;
            }

            System.Diagnostics.Debug.Assert(_stopToken.Type != TokenConstants.Eof);

            // Iterate over fetched tokens to restore previously replaced token
            fetchedEOF = false;
            for (int index = 0; index < tokens.Count; index++)
            {
                if (ReferenceEquals(tokens[index], _stopTokenReplacedByEOF))
                {
                    tokens[index] = _stopTokenReplacedByEOF.OriginalToken;
                }

                // Restore correct fetchedEOF by checking non-consumed tokens
                if (index >= Index && tokens[index].Type == TokenConstants.Eof)
                {
                    fetchedEOF = true;
                }
            }

            _stopToken = null;
            _stopTokenReplacedByEOF = null;
        }

        /// <summary>
        /// Token which marks the end of an interesting code section
        /// </summary>
        private IToken _stopToken;

        // EOF token which replaces the original stop token
        private ReplacedToken _stopTokenReplacedByEOF;

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
