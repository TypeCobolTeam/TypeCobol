using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Class used for tokens which are a continuation of other tokens situated on previous lines
    /// </summary>
    public class ContinuationToken : Token
    {
        internal ContinuationToken(Token virtualConcatenatedToken, int startIndex, int offsetForLiteralContinuation, int stopIndex, ITokensLine tokensLine, Token continuedToken) :
            base(virtualConcatenatedToken.TokenType, startIndex, stopIndex, virtualConcatenatedToken.UsesVirtualSpaceAtEndOfLine, tokensLine)
        {
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

            // Register the continued token
            ContinuedToken = continuedToken;

            //  Store the concatenated source text
            string sourceTextForConcat = this.SourceText.Substring(offsetForLiteralContinuation); // An extra delimiter must be removed in case of a continued alphanumeric literal
            if (continuedToken is ContinuationToken)
            {
                ContinuedSourceText = ((ContinuationToken)continuedToken).ContinuedSourceText + sourceTextForConcat;
            }
            else
            {
                ContinuedSourceText = continuedToken.SourceText + sourceTextForConcat;
            }

            // Ragister backward link (doubly linked list)
            continuedToken.LinkToContinuationToken(this);

            // Set specific Channel to enable filtering
            Channel = CHANNEL_ContinuationTokens;
        }

        /// <summary>
        /// Previous Token which was continued by this one
        /// </summary>
        public Token ContinuedToken { get; private set; }

        /// <summary>
        /// Concatenation of the source text from the previous continued token and the source text this one
        /// </summary>
        public string ContinuedSourceText { get; private set; }

        /// <summary>
        /// Recursively set the type of the continued tokens to the same type as the continuation token
        /// </summary>
        internal override void SetPropertiesFromContinuationToken()
        {
            base.SetPropertiesFromContinuationToken();

            // Recursively propagate the properties to previous continued tokens
            ContinuedToken.SetPropertiesFromContinuationToken();
        }

        /// <summary>
        /// Text returned to the parser :
        /// - ContinuedSourceText if the token is not continued on the next line
        /// - ContinuationToken.Text if the token IsContinuedOnTheNextLine
        /// </summary>
        public override string Text
        {
            get
            {
                if (!IsContinuedOnTheNextLine)
                {
                    return ContinuedSourceText;
                }
                else
                {
                    return ContinuationToken.Text;
                }
            }
        }

        /// <summary>
        /// Debug string
        /// </summary>
        public override string ToString()
        {
            return "=>continuation:" + base.ToString();
        }
    }
}
