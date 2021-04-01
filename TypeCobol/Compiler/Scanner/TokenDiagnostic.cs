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
        /// Token which is the subject of the diagnostics
        /// </summary>
        public Token Token { get; private set; }
    }
}
