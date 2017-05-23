using System;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CustomExceptions
{
    public class CopyLoadingException : TypeCobolException
    {
        public CopyLoadingException(string message, string path, Exception innerException = null, bool logged = true, bool needMail = true, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1)
            : base(MessageCode.IntrinsicLoading, message, path, innerException, logged, needMail, columnStartIndex, columnEndIndex, lineNumber)
        {
            //Here you can do special thinks for this kind of exception... 
        }

    }



    public class MissingCopyException : TypeCobolException
    {
        public MissingCopyException(string message, string path, Exception innerException = null, bool logged = true, bool needMail = true)
            : base(Compiler.Diagnostics.MessageCode.FailedToLoadTextDocumentReferencedByCopyDirective, message, path, innerException, logged, needMail)
        {
            //Here you can do special thinks for this kind of exception... 
        }
    }
}
