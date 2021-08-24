using System;
using System.Diagnostics;
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
            public static readonly Position Default = new Position(0, 0, 0, 0, null);

            private readonly string _messageAdapter;

            public Position(int lineStart, int lineEnd, int columnStart, int columnEnd, CopyDirective includingDirective)
            {
                if (includingDirective != null)
                {
                    //Position diagnostic on including copy directive and adapt message
                    var startToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.FirstOrDefault()?.FirstOrDefault();
                    var endToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.LastOrDefault()?.LastOrDefault();
                    LineStart = startToken?.Line ?? 0;
                    LineEnd = endToken?.Line ?? LineStart;
                    ColumnStart = startToken?.Column ?? 0;
                    ColumnEnd = endToken?.EndColumn ?? ColumnStart;
                    if (LineStart == LineEnd)
                    {
                        _messageAdapter = $"Error in copy '{includingDirective.TextName}' at line {lineStart} : {{0}}";
                    }
                    else
                    {
                        _messageAdapter = $"Error in copy '{includingDirective.TextName}' at lines {lineStart}-{lineEnd} : {{0}}";
                    }
                }
                else
                {
                    //Position diagnostic directly at specified location
                    LineStart = Math.Max(0, lineStart);
                    LineEnd = Math.Max(lineStart, lineEnd);
                    ColumnStart = Math.Max(0, columnStart);
                    ColumnEnd = Math.Max(0, columnEnd);
                    _messageAdapter = null;
                }
            }

            public int LineStart { get; }
            public int LineEnd { get; }
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
            messageArgs = messageArgs ?? new object[0];
            Message = string.Format(Info.MessageTemplate, messageArgs);
            ApplyPosition(position);
            CaughtException = messageArgs.OfType<Exception>().FirstOrDefault();
        }

        protected Diagnostic([NotNull] Diagnostic other)
        {
            System.Diagnostics.Debug.Assert(other != null);

            Info = other.Info;//DiagnosticMessage is a readonly class so it's ok to keep the same reference instead of creating a copy instance
            LineStart = other.LineStart;
            LineEnd = other.LineEnd;
            ColumnStart = other.ColumnStart;
            ColumnEnd = other.ColumnEnd;
            Message = other.Message;
            CaughtException = other.CaughtException;
        }

        public DiagnosticMessage Info { get; }

        public int LineStart
        {
            get;
            internal set;//Internal setter is required for incremental mode. When lines are inserted/removed, associated diagnostics are shifted up/down accordingly
        }

        public int LineEnd
        {
            get;
            internal set;//Internal setter is required for incremental mode. When lines are inserted/removed, associated diagnostics are shifted up/down accordingly
        }

        public int ColumnStart { get; private set;  }
        public int ColumnEnd { get; private set; }
        public string Message { get; private set; }
        public Exception CaughtException { get; }

        /// <summary>
        /// Text representation of a diagnostic for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            if (LineStart == LineEnd)
                return $"Line {LineStart}[{ColumnStart},{ColumnEnd}] <{Info.Code}, {Info.Severity}, {Info.Category}> - {Message}";
            return $"Lines {LineStart}:{LineEnd}[{ColumnStart},{ColumnEnd}] <{Info.Code}, {Info.Severity}, {Info.Category}> - {Message}";
        }

        private void ApplyPosition([NotNull] Position position)
        {
            System.Diagnostics.Debug.Assert(position != null);
            LineStart = position.LineStart;
            LineEnd = position.LineEnd;
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
