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
            if (stopToken != null)
            {
                StopToken = stopToken;
                stopTokenReplacedByEOF = new ReplacedToken(Token.EndOfFile(), stopToken);
                if (tokens.Count > 0 && stopToken.Equals(tokens[tokens.Count - 1]))
                {
                    // Last fetched token is stopToken
                    tokens[tokens.Count - 1] = stopTokenReplacedByEOF;
                    indexOfStopTokenReplacedByEOF = tokens.Count - 1;
                    fetchedEOF = true;
                }
            }
        }

        /// <summary>
        /// Cancel a previous replacement of StopToken by EOF
        /// </summary>
        public void ResetStopTokenLookup()
        {
            // Reset replacement of stop token by EOF
            if (indexOfStopTokenReplacedByEOF >= 0 && indexOfStopTokenReplacedByEOF < tokens.Count)
            {
                if (tokens[indexOfStopTokenReplacedByEOF] == stopTokenReplacedByEOF)
                {
                    tokens[indexOfStopTokenReplacedByEOF] = stopTokenReplacedByEOF.OriginalToken;
                    fetchedEOF = false;
                }
                indexOfStopTokenReplacedByEOF = -1;
            }
        }

        /// <summary>
        /// Token which marks the end of an interesting code section
        /// </summary>
        public IToken StopToken { get; private set; }

        // EOF token which replaces the original StopToken
        private ReplacedToken stopTokenReplacedByEOF;

        // Index of the replaced stop token in the buffer
        private int indexOfStopTokenReplacedByEOF = -1;

        /// <summary>
        /// Override the original Fetch method from BufferedTokenStream : same behavior,
        /// except that StopToken is replaced with EOF on the fly.
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
                if (StopToken != null && StopToken.Equals(t))
                {
                    t = stopTokenReplacedByEOF;
                    indexOfStopTokenReplacedByEOF = tokens.Count;
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
