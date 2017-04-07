using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CLI.CustomExceptions
{
    public class CopyLoadingException : TypeCobolException
    {
        public CopyLoadingException(MessageCode messageCode, string message, string path, bool logged = false, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1)
            : base(messageCode, message, path, columnStartIndex, columnEndIndex, lineNumber)
        {
            //Here you can do special thinks for this kind of exception... 
        }

    }
}
