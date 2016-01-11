using Antlr4.Runtime;
using System;

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
                if (start == stop)
                {
                    return start.Text;
                }
                else
                {
                    return start.Text + " ... " + stop.Text;
                }
            }
            else if (start != null)
            {
                return start.Text + " ...";
            }
            else if (stop != null)
            {
                return "... " + stop.Text;
            }
            else
            {
                return String.Empty;
            }
        }

        /// <summary>
        /// Advance the stream to one specific token.
        /// Throw an exception if the token is not found.
        /// </summary>
        public void SeekToToken(IToken searchedToken)
        {
            // TO DO : optimize this naive implementation
            // Not easy because of the underlying Copy and Replace iterators
            while(Lt(1) != searchedToken)
            {
                Consume();
            }
            if(Lt(1) != searchedToken)
            {
                throw new InvalidOperationException("Token not found in this stream");
            }
        }

        /// <summary>
        /// Token which marks the end of an interesting code section
        /// </summary>
        public IToken StopToken { get; private set; }

        /// <summary>
        /// True when the token stream has reached StopToken
        /// </summary>
        public bool StreamReachedStopToken { get; private set; }

        /// <summary>
        /// Start monitoring if the token stream reached a specific token which marks the end of an instersting section
        /// </summary>
        public void StartLookingForStopToken(IToken stopToken)
        {
            StopToken = stopToken;
            StreamReachedStopToken = false;
        }

        public override void Consume()
        {
            base.Consume();
            if(StopToken != null)
            {
                if(Lt(1) == StopToken)
                {
                    StreamReachedStopToken = true;
                }
            }
        }
    }
}
