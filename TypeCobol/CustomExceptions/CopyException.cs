using System;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CustomExceptions
{
    public class CopyLoadingException : TypeCobolException
    {
        public CopyLoadingException(string message, string path, Exception innerException = null, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1)
            : base(MessageCode.IntrinsicLoading, message, path, innerException, columnStartIndex, columnEndIndex, lineNumber)
        {
            //Here you can do special things for this kind of exception...
        }
    }

    public class MissingCopyException : TypeCobolException
    {
        public MissingCopyException(string message, string path, Exception innerException = null)
            : base(MessageCode.FailedToLoadTextDocumentReferencedByCopyDirective, message, path, innerException)
        {
            //Here you can do special things for this kind of exception...
        }
    }
}
