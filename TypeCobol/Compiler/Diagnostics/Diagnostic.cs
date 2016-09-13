using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Diagnostics
{
    /// <summary>
    /// Detailed error message for the end user, with the location of the problem dected in the source text
    /// </summary>
    public class Diagnostic
    {
        internal Diagnostic(MessageCode messageCode, int columnStart, int columnEnd, params object[] messageArgs)
        {
            Info = DiagnosticMessage.GetFromCode[(int)messageCode];

            ColumnStart = Math.Max(columnStart, 0);
            ColumnEnd   = Math.Max(columnEnd,   0);

            Message = String.Format(Info.MessageTemplate, messageArgs);
            MessageArgs = messageArgs;
        }

        public DiagnosticMessage Info { get; private set; }

        public int ColumnStart { get; private set; }
        public int ColumnEnd { get; private set; }

        public string Message { get; private set; }
        internal object[] MessageArgs { get; private set; }

         /// <summary>
        /// Text representation of a token for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            return "[" + ColumnStart + "," + ColumnEnd + "]<" + Info.Code + "," + Info.Severity + "," + Info.Category + ">" + Message;
        }
    }
}
