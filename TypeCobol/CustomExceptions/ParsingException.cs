using System;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CustomExceptions
{
    public class ParsingException : TypeCobolException
    {
        public ParsingException(MessageCode messageCode, string message, string path, Exception innerException = null, bool logged = true, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1) 
            : base (messageCode, message, path, innerException, logged, columnStartIndex, columnEndIndex, lineNumber)
        {
            //Here you can do special thinks for this kind of exception... 
        }

    }
}
