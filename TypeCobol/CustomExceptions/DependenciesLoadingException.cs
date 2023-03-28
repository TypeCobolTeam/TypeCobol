using System;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CustomExceptions
{
    public class DependenciesLoadingException : TypeCobolException
    {
        public DependenciesLoadingException(string message, string path, Exception innerException = null, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1)
            : base(MessageCode.DependenciesLoading, message, path, innerException, columnStartIndex, columnEndIndex, lineNumber)
        {
            //Here you can do special things for this kind of exception...
        }
    }
}
