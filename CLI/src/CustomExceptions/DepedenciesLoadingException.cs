using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CLI.CustomExceptions
{
    public class DepedenciesLoadingException : TypeCobolException
    {
        public DepedenciesLoadingException(string message, string path, bool logged = true, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1)
            : base(MessageCode.DependenciesLoading, message, path, logged, columnStartIndex, columnEndIndex, lineNumber)
        {
            //Here you can do special thinks for this kind of exception... 
        }

    }
}
