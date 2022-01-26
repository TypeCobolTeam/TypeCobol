using System;
using System.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Class used for tokens which participate in a multiline continuation with other tokens on previous or next lines
    /// </summary>
    public class ContinuationToken : Token
    {
        internal ContinuationToken(Token virtualConcatenatedToken, int startIndex, int stopIndex, ITokensLine tokensLine, bool isContinuationFromPreviousLine, bool isContinuedOnNextLine) :
            base(virtualConcatenatedToken.TokenType, startIndex, stopIndex, virtualConcatenatedToken.UsesVirtualSpaceAtEndOfLine, tokensLine)
        {
            //  Store the concatenated source text
            MultilineContinuationText = virtualConcatenatedToken.Text;
            IsContinuationFromPreviousLine = isContinuationFromPreviousLine;
            IsContinuedOnNextLine = isContinuedOnNextLine;

            // Copy the delimiter properties
            if (virtualConcatenatedToken.UsesDelimiters)
            {
                UsesDelimiters = virtualConcatenatedToken.UsesDelimiters;
                HasOpeningDelimiter = virtualConcatenatedToken.HasOpeningDelimiter;
                HasClosingDelimiter = virtualConcatenatedToken.HasClosingDelimiter;
                ExpectedClosingDelimiter = virtualConcatenatedToken.ExpectedClosingDelimiter;
            }

            // Copy the literal value
            LiteralValue = virtualConcatenatedToken.LiteralValue;

            // Set specific Channel to enable filtering of all tokens participating in the continuation, except the first one
            if (IsContinuationFromPreviousLine)
            {
                Channel = CHANNEL_ContinuationTokens;
            }
        }

        /// <summary>
        /// True if this token participates in a multiline continuation
        /// </summary>
        public override bool IsContinuationToken { get { return true; } }

        /// <summary>
        /// Concatenation of the source text of all individual tokens participating in the continuation 
        /// </summary>
        public string MultilineContinuationText { get; private set; }

        /// <summary>
        /// True if this token continues a token found on the previous line
        /// </summary>
        public bool IsContinuationFromPreviousLine { get; protected set; }

        /// <summary>
        /// True if a token found on the next line continues the current token 
        /// </summary>
        public bool IsContinuedOnNextLine { get; private set; }
        
        /// <summary>
        /// Text returned to the parser :
        /// - ContinuedSourceText if the token is not continued on the next line
        /// - ContinuationToken.Text if the token IsContinuedOnTheNextLine
        /// </summary>
        public override string Text
        {
            get
            {
                return MultilineContinuationText;
            }
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            StringBuilder sb = new StringBuilder();
            if (IsContinuationFromPreviousLine)
            {
                sb.Append("=>continuation:");
            }
            if(IsContinuedOnNextLine)
            {
                sb.Append("=>continued:");
            }
            sb.Append(base.ToString());
            return sb.ToString();
        }
    }
}
