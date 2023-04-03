using System;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CustomExceptions
{
    public class ParsingException : TypeCobolException
    {
        public ParsingException(MessageCode messageCode, string message, string path, Exception innerException = null, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1) 
            : base (messageCode, message, path, innerException, columnStartIndex, columnEndIndex, lineNumber)
        {
            //Here you can do special things for this kind of exception...
        }
    }
}
