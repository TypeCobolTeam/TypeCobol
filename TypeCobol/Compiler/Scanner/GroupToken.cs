using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// A class that represents a Group Of Tokens.
    /// </summary>
    public class GroupToken : Token
    {
        /// <summary>
        /// The Group of tokens.
        /// </summary>
        public IList<Token> Group
        {
            get;
            private set;
        }
        public GroupToken()
        {
        }
        public GroupToken(TokenType tokenType, int startIndex, int stopIndex, ITokensLine tokensLine) :
            base(tokenType, startIndex, stopIndex, false, tokensLine)
        { }
        internal GroupToken(TokenType tokenType, int startIndex, int stopIndex, bool usesVirtualSpaceAtEndOfLine, ITokensLine tokensLine) :
            base(tokenType, startIndex, stopIndex, usesVirtualSpaceAtEndOfLine, tokensLine)
        {
        }

        /// <summary>
        /// Add a token to the group
        /// </summary>
        /// <param name="token"></param>
        public void AddToken(Token token)
        {
            if (Group == null)
            {
                Group = new List<Token>(2);
            }
            ((List < Token > )Group).Add(token);
        }
    }
}
