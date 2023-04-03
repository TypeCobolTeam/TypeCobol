using System;
using System.Diagnostics;
using System.Globalization;
using System.Threading;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.CustomExceptions
{
    public abstract class TypeCobolException : Exception
    {
        public MessageCode MessageCode { get; set; }
        /// <summary>
        /// Path to the source file
        /// </summary>
        public string Path { get; set; }

        public int ColumnStartIndex { get; set; }
        public int ColumnEndIndex { get; set; }
        public int LineNumber { get; set; }
        public bool Logged { get; set; }
        public override string StackTrace
        {
            get
            {
                //As we currently have error message in english, we will log exception message and its stacktrace in InvariantCulture
                var CurrentCulture = Thread.CurrentThread.CurrentCulture;
                var CurrentUICulture = Thread.CurrentThread.CurrentUICulture;

                Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
                Thread.CurrentThread.CurrentUICulture = CultureInfo.InvariantCulture;

                var ex = this as Exception;
                var stackTrace = new StackTrace(ex).ToString();

                while (ex.InnerException != null)
                {
                    stackTrace = stackTrace + "\n\n" + new StackTrace(ex.InnerException).ToString();
                    ex = ex.InnerException;
                }

                //set back the correct culture
                Thread.CurrentThread.CurrentCulture = CurrentCulture;
                Thread.CurrentThread.CurrentUICulture = CurrentUICulture;

                return stackTrace;
            }
        }


        protected TypeCobolException(MessageCode messageCode, string message, string path, Exception innerException = null, int columnStartIndex = 0, int columnEndIndex = 0, int lineNumber = 1) : base (message, innerException)
        {
            MessageCode = messageCode;
            //Message is set by the base constructor of Exception. 
            ColumnStartIndex = columnStartIndex;
            ColumnEndIndex = columnEndIndex;
            LineNumber = lineNumber;
            Path = path;
            Logged = true;
        }
    }
}
