using System;
using System.Linq;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Diagnostics
{
    /// <summary>
    /// Detailed error message for the end user, with the location of the problem detected in the source text
    /// </summary>
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

        public Diagnostic(MessageCode messageCode, Position position, params object[] messageArgs)
            : this(DiagnosticMessage.GetFromCode(messageCode), position, messageArgs)
        {

        }

        protected Diagnostic(DiagnosticMessage info, Position position, params object[] messageArgs)
        {
            Info = info;

            _messageArgs = messageArgs ?? new object[0];
            CaughtException = _messageArgs.OfType<Exception>().FirstOrDefault();

            position = position ?? Position.Default;
            Line = position.Line;
            ColumnStart = position.ColumnStart;
            ColumnEnd = position.ColumnEnd;
            Message = position.AdaptMessage(string.Format(Info.MessageTemplate, _messageArgs));
        }

        public DiagnosticMessage Info { get; }

        public int Line { get; internal set; } //TODO private setter ?
        public int ColumnStart { get; }
        public int ColumnEnd { get; }

        public string Message { get; internal set; } //TODO private setter ?

        private readonly object[] _messageArgs;
        public object[] MessageArgs => _messageArgs; //TODO remove this ?
        public Exception CaughtException { get; }

        /// <summary>
        /// Text representation of a diagnostic for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            return $"Line {Line}[{ColumnStart},{ColumnEnd}] <{Info.Code}, {Info.Severity}, {Info.Category}> - {Message}";
        }
    }
}
