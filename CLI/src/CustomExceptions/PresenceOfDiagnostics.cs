using TypeCobol.Compiler.Diagnostics;
using TypeCobol.CustomExceptions;

namespace TypeCobol.CLI.CustomExceptions
{
    public class PresenceOfDiagnostics : TypeCobolException
    {
        public PresenceOfDiagnostics(string message)
            : base(MessageCode.SyntaxErrorInParser, message, null, null, false, false)
        {
            //Here you can do special thinks for this kind of exception... 
        }

    }
}
