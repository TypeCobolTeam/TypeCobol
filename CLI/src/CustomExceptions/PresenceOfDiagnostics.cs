using TypeCobol.Compiler.Diagnostics;
using TypeCobol.CustomExceptions;

namespace TypeCobol.CLI.CustomExceptions
{
    public class PresenceOfDiagnostics : TypeCobolException
    {
        public PresenceOfDiagnostics(string message, string path)
            : base(MessageCode.SyntaxErrorInParser, message, path)
        {
            Logged = false;
        }
    }
}
