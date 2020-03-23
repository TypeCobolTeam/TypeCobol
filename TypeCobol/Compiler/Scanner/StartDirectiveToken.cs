using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// This is the Start Compiler Directive Token
    /// </summary>
    public class StartDirectiveToken : Token
    {
        /// <summary>Empty constructor for mock.</summary>
        public StartDirectiveToken() { }

        /// <summary>
        /// Constructor for tokens without delimiters
        /// </summary>
        public StartDirectiveToken(TokenType tokenType, int startIndex, int stopIndex, ITokensLine tokensLine) :
            base(tokenType, startIndex, stopIndex, false, tokensLine)
        { }

        /// <summary>
        /// The Associated Compiler Directive build by the PreprocessorStep phase
        /// </summary>
        public CompilerDirective CompilerDirective
        {
            get;
            internal set;
        }
    }
}
