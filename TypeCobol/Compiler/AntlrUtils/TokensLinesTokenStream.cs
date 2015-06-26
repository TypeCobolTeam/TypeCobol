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
    }
}
