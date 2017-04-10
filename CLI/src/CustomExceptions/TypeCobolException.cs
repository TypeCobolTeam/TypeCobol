using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CLI.CustomExceptions
{
    public abstract class TypeCobolException : Exception
    {
        public MessageCode MessageCode { get; set; }
        public string Path { get; set; }

        public int ColumnStartIndex { get; set; }
        public int ColumnEndIndex { get; set; }
        public int LineNumber { get; set; }
        public bool Logged { get; set; }    


        public TypeCobolException(MessageCode messageCode, string message, string path, bool logged = true, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1) : base (message)
        {
            MessageCode = messageCode;
            //Message is set by the base constructor of Exception. 
            ColumnStartIndex = columnStartIndex;
            ColumnEndIndex = columnEndIndex;
            LineNumber = lineNumber;
            Path = path;
            Logged = logged;
        }
    }
}
