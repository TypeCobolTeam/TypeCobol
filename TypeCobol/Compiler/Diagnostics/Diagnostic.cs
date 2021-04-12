using System;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Diagnostics
{
    /// <summary>
    /// Detailed error message for the end user, with the location of the problem detected in the source text.
    /// </summary>
    /// <remarks>Derived classes must override Duplicate method to ensure their own data is properly duplicated.</remarks>
    public class Diagnostic
    {
        public class Position
        {
            public static readonly Position Default = new Position(0, 0, 0, null);

            private readonly string _messageAdapter;

            public Position(int line, int columnStart, int columnEnd, CopyDirective includingDirective)
            {
                line = Math.Max(0, line);
                if (includingDirective != null)
                {
                    //Position diagnostic on including copy directive and adapt message
                    Line = includingDirective.COPYToken.Line;
                    var startToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.FirstOrDefault()?.FirstOrDefault();
                    var endToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.LastOrDefault()?.LastOrDefault();
                    ColumnStart = startToken?.Column ?? 0;
                    ColumnEnd = endToken?.EndColumn ?? ColumnStart;
                    _messageAdapter = $"Error in copy '{includingDirective.TextName}' at line {line} : {{0}}";
                }
                else
                {
                    //Position diagnostic directly at specified location
                    Line = line;
                    ColumnStart = Math.Max(0, columnStart);
                    ColumnEnd = Math.Max(0, columnEnd);
                    _messageAdapter = null;
                }
            }

            public int Line { get; }
            public int ColumnStart { get; }
            public int ColumnEnd { get; }

            internal string AdaptMessage(string message)
            {
                return _messageAdapter != null ? string.Format(_messageAdapter, message) : message;
            }
        }

        public Diagnostic(MessageCode messageCode, [NotNull] Position position, params object[] messageArgs)
            : this(DiagnosticMessage.GetFromCode(messageCode), position, messageArgs)
        {

        }

        protected Diagnostic(DiagnosticMessage info, [NotNull] Position position, params object[] messageArgs)
        {
            Info = info;
            MessageArgs = messageArgs ?? new object[0];
            Message = string.Format(Info.MessageTemplate, MessageArgs);
            ApplyPosition(position);
            CaughtException = MessageArgs.OfType<Exception>().FirstOrDefault();
        }

        protected Diagnostic([NotNull] Diagnostic other)
        {
            System.Diagnostics.Debug.Assert(other != null);

            Info = other.Info;//DiagnosticMessage is a readonly class so it's ok to keep the same reference instead of creating a copy instance
            Line = other.Line;
            ColumnStart = other.ColumnStart;
            ColumnEnd = other.ColumnEnd;
            Message = other.Message;
            CaughtException = other.CaughtException;
            MessageArgs = other.MessageArgs;
        }

        public DiagnosticMessage Info { get; }

        public int Line
        {
            get;
            internal set;//Internal setter is required for incremental mode. When lines are inserted/removed, associated diagnostics are shifted up/down accordingly
        }

        public int ColumnStart { get; private set;  }
        public int ColumnEnd { get; private set; }
        public string Message { get; private set; }
        public Exception CaughtException { get; }

        //Required when a diagnostic has to be duplicated
        internal object[] MessageArgs { get; }//TODO remove this

        /// <summary>
        /// Text representation of a diagnostic for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            return $"Line {Line}[{ColumnStart},{ColumnEnd}] <{Info.Code}, {Info.Severity}, {Info.Category}> - {Message}";
        }

        private void ApplyPosition([NotNull] Position position)
        {
            System.Diagnostics.Debug.Assert(position != null);
            Line = position.Line;
            ColumnStart = position.ColumnStart;
            ColumnEnd = position.ColumnEnd;
            Message = position.AdaptMessage(Message);
        }

        /// <summary>
        /// Equivalent to Clone() method but restricted to inheritors.
        /// </summary>
        /// <returns>Duplicate instance of this Diagnostic.</returns>
        protected virtual Diagnostic Duplicate() => new Diagnostic(this);

        /// <summary>
        /// Creates a copy of this diagnostic and position it on the new supplied location.
        /// </summary>
        /// <param name="newPosition">Non-null instance of Position to locate the diagnostic in code.</param>
        /// <returns>New instance of diagnostic, copied from this one.</returns>
        public Diagnostic CopyAt([NotNull] Position newPosition)
        {
            var copy = Duplicate();
            copy.ApplyPosition(newPosition);
            return copy;
        }
    }
}
