﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Analytics;

namespace TypeCobol.Compiler.Diagnostics
{
    /// <summary>
    /// Detailed error message for the end user, with the location of the problem dected in the source text
    /// </summary>
    public class Diagnostic : IEquatable<Diagnostic>
    {
        private readonly int _hashCode;

        public Diagnostic(MessageCode messageCode, int columnStart, int columnEnd, int lineNumber, params object[] messageArgs)
        {
            Info = DiagnosticMessage.GetFromCode[(int)messageCode];

            ColumnStart = Math.Max(columnStart, 0);
            ColumnEnd   = Math.Max(columnEnd,   0);

            Line = lineNumber;
            Message = String.Format(Info.MessageTemplate, messageArgs ?? new object[0]);
            if (messageArgs != null)
            {
                CatchedException = messageArgs.FirstOrDefault(x => x is Exception) as Exception;
                MessageArgs = messageArgs;
            }
            _hashCode = $"{Line}{ColumnStart}{ColumnEnd}{Message}{Info.Code}{Info.Severity}".GetHashCode();
        }

        public DiagnosticMessage Info { get; set; }

        public int ColumnStart { get; set; }
        public int ColumnEnd { get; set; }

        public int Line { get; set; }

        public string Message { get; set; }
        internal object[] MessageArgs { get; set; }
        public Exception CatchedException { get; private set; }

         /// <summary>
        /// Text representation of a token for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            return string.Format("Line {0}[{1},{2}] <{3}, {4}, {5}> - {6}", Line, ColumnStart, ColumnEnd, Info.Code, Info.Severity, Info.Category, Message);
        }

        public override bool Equals(object obj)
        {
            return Equals(obj as Diagnostic);
        }

        public bool Equals(Diagnostic diagnostic)
        { 
            if (Object.ReferenceEquals(this, diagnostic)) return true;
            if (diagnostic == null) return false;

            return diagnostic.Line == this.Line &&
                   diagnostic.ColumnStart == this.ColumnStart &&
                   diagnostic.ColumnEnd == this.ColumnEnd &&
                   diagnostic.Message == this.Message &&
                   (diagnostic.Info != null && this.Info != null) &&
                   diagnostic.Info.Code == this.Info.Code &&
                   diagnostic.Info.Severity == this.Info.Severity;
        }

        public override int GetHashCode()
        {
            return _hashCode;
        }
    }
}
