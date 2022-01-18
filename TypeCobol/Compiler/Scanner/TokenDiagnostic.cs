using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Used to register a diagnostic specifically attached to a Token
    /// </summary>
    public class TokenDiagnostic : Diagnostic
    {
        internal TokenDiagnostic(MessageCode messageCode, Token token, params object[] messageArgs)
            : base(messageCode, token.Position(), messageArgs)
        {
            Token = token;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="usePredefinedArguments">True if predefined arguments are used, false otherwise</param>
        /// <param name="messageCode">Message's code</param>
        /// <param name="token">The associate dtoken</param>
        /// <param name="messageArgs">Message arguments</param>
        internal TokenDiagnostic(bool usePredefinedArguments, MessageCode messageCode, Token token, params object[] messageArgs)
            : base(usePredefinedArguments, messageCode, token.Position(), messageArgs)
        {
            Token = token;
        }

        private TokenDiagnostic(TokenDiagnostic other)
            : base(other)
        {
            Token = other.Token;
        }

        /// <summary>
        /// Token which is the subject of the diagnostics
        /// </summary>
        public Token Token { get; }

        protected override Diagnostic Duplicate() => new TokenDiagnostic(this);
    }
}
