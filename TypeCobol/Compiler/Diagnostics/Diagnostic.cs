using System;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Directives;
using TypeCobol.CustomExceptions;

namespace TypeCobol.Compiler.Diagnostics
{
    /// <summary>
    /// Detailed error message for the end user, with the location of the problem detected in the source text
    /// </summary>
    public class Diagnostic
    {
        public class Position
        {
            public static readonly Position Default = new Position(0, 0, 0);

            private readonly string _messageAdapter;

            public Position(int line, int columnStart, int columnEnd, CopyDirective includingDirective = null)
            {
                line = Math.Max(0, line);
                if (includingDirective != null)
                {
                    Line = includingDirective.COPYToken.Line;
                    var startToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.FirstOrDefault()?.FirstOrDefault();
                    var endToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.LastOrDefault()?.LastOrDefault();
                    ColumnStart = startToken?.Column ?? 0;
                    ColumnEnd = endToken?.EndColumn ?? ColumnStart;
                    _messageAdapter = $"Error in copy '{includingDirective.TextName}' at line {line} : {{0}}";
                }
                else
                {
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

        public static Diagnostic FromException(MessageCode messageCode, [NotNull] Exception exception)
        {
            string message = exception.Message + Environment.NewLine + exception.StackTrace;
            return new Diagnostic(messageCode, Position.Default, message);
        }

        public static Diagnostic FromTypeCobolException([NotNull] TypeCobolException typeCobolException)
        {
            string message = typeCobolException.Message + Environment.NewLine + typeCobolException.StackTrace;
            var position = new Position(typeCobolException.LineNumber, typeCobolException.ColumnStartIndex, typeCobolException.ColumnEndIndex);
            return new Diagnostic(typeCobolException.MessageCode, position, message);
        }

        public Diagnostic(MessageCode messageCode, Position position, params object[] messageArgs)
            : this(DiagnosticMessage.GetFromCode(messageCode), position, messageArgs)
        {

        }

        protected Diagnostic(DiagnosticMessage info, Position position, params object[] messageArgs)
        {
            Info = info;
            _messageArgs = messageArgs ?? new object[0];
            CaughtException = _messageArgs.OfType<Exception>().FirstOrDefault();
            Relocate(position);
        }

        [Obsolete]
        public Diagnostic(MessageCode messageCode, int columnStart, int columnEnd, int lineNumber, params object[] messageArgs)
            : this(DiagnosticMessage.GetFromCode(messageCode), new Position(lineNumber, columnStart, columnEnd), messageArgs)
        {

        }

        public DiagnosticMessage Info { get; }

        public int Line { get; internal set; } //TODO private setter
        public int ColumnStart { get; private set; }
        public int ColumnEnd { get; private set; }

        public string Message { get; internal set; } //TODO private setter

        private readonly object[] _messageArgs;
        public object[] MessageArgs => _messageArgs; //TODO remove this
        public Exception CaughtException { get; }

        public void Relocate(Position position)
        {
            position = position ?? Position.Default;
            Line = position.Line;
            ColumnStart = position.ColumnStart;
            ColumnEnd = position.ColumnEnd;
            Message = position.AdaptMessage(string.Format(Info.MessageTemplate, _messageArgs));
        }

         /// <summary>
        /// Text representation of a diagnostic for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            return $"Line {Line}[{ColumnStart},{ColumnEnd}] <{Info.Code}, {Info.Severity}, {Info.Category}> - {Message}";
        }
    }
}
