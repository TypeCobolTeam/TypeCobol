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
            public static readonly Position Default = new Position(0, 0, 0, 0, null);

            internal readonly string _messageAdapter;
            internal readonly bool _fromIcludingDirective;

            public Position(int lineStart, int columnStart, int lineEnd, int columnEnd, CopyDirective includingDirective)
            {
                if (includingDirective != null)
                {
                    _fromIcludingDirective = true;
                    //Position diagnostic on including copy directive and adapt message
                    var startToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.FirstOrDefault()?.FirstOrDefault();
                    var endToken = includingDirective.ConsumedTokens.SelectedTokensOnSeveralLines.LastOrDefault()?.LastOrDefault();
                    
                    if (startToken != null)
                    {
                        LineStart = startToken.Line;
                        ColumnStart = startToken.Column;
                    }
                    else
                    {
                        LineStart = 0;
                        ColumnStart = 0;
                    }

                    if (endToken != null)
                    {
                        LineEnd = endToken.Line;
                        ColumnEnd = endToken.EndColumn;
                    }
                    else
                    {
                        LineEnd = LineStart;
                        ColumnEnd = ColumnStart;
                    }

                    if (LineStart == LineEnd)
                    {
                        _messageAdapter = $"Error in copy '{includingDirective.TextName}' at line {lineStart} : {{0}}";
                    }
                    else
                    {
                        _messageAdapter = $"Error in copy '{includingDirective.TextName}' from line {lineStart} to line {lineEnd} : {{0}}";
                    }
                }
                else
                {
                    //Position diagnostic directly at specified location
                    LineStart = Math.Max(0, lineStart);
                    ColumnStart = Math.Max(0, columnStart);
                    LineEnd = Math.Max(LineStart, lineEnd);
                    ColumnEnd = Math.Max(0, columnEnd);
                    _messageAdapter = null;
                }
            }

            public int LineStart { get; }
            public int ColumnStart { get; }
            public int LineEnd { get; }
            public int ColumnEnd { get; }

            internal string AdaptMessage(string message)
            {
                return _messageAdapter != null ? string.Format(_messageAdapter, message) : message;
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="messageCode">Diagnostic's message code</param>
        /// <param name="position">Position in the source code</param>        
        /// <param name="messageArgs">The message arguments</param>
        public Diagnostic(MessageCode messageCode, [NotNull] Position position, params object[] messageArgs)
            : this(DiagnosticMessage.GetFromCode(messageCode), position, messageArgs)
        {

        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="usePredefinedArguments">True if predefined arguments are used, false otherwise</param>
        /// <param name="messageCode">Diagnostic's message code</param>
        /// <param name="position">Position in the source code</param>        
        /// <param name="messageArgs">The message arguments</param>
        public Diagnostic(bool usePredefinedArguments, MessageCode messageCode, [NotNull] Position position, params object[] messageArgs)
            : this(usePredefinedArguments, DiagnosticMessage.GetFromCode(messageCode), position, messageArgs)
        {

        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="info">Diagnostics info</param>
        /// <param name="position">Position in the source code</param>
        /// <param name="messageArgs">The message arguments</param>
        protected Diagnostic(DiagnosticMessage info, [NotNull] Position position, params object[] messageArgs)
           : this(false, info, position, messageArgs)
        {

        }
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="usePredefinedArguments">True if predefined arguments are used, false otherwise</param>
        /// <param name="info">Diagnostics info</param>
        /// <param name="position">Position in the source code</param>        
        /// <param name="messageArgs">The message arguments</param>
        protected Diagnostic(bool usePredefinedArguments, DiagnosticMessage info, [NotNull] Position position, params object[] messageArgs)
        {
            Info = info;
            _UsePredefinedArguments = usePredefinedArguments;
            _MessageArgs = messageArgs ?? Array.Empty<object>();
            ApplyMessageAndPosition(position);
            CaughtException = messageArgs.OfType<Exception>().FirstOrDefault();
        }

        /// <summary>
        /// Predefined value for Dynamic Column Position
        /// </summary>
        public static object PredefArgColumnStart = new object();
        /// <summary>
        /// Translate message arguments to predefined dynamic values.
        /// </summary>
        /// <param name="messageArgs">The arguments to be translated</param>
        /// <returns>The resulting arguments</returns>
        private object[] TranslateArguments(object[] messageArgs)
        {
            if (messageArgs.Length == 0)
                return messageArgs;
            object[] newMessageArgs = new object[messageArgs.Length];
            for (int i = 0; i < messageArgs.Length; i++)
            {
                if (messageArgs[i] == PredefArgColumnStart)
                {
                    newMessageArgs[i] = this.ColumnStart;
                }
                else
                {
                    newMessageArgs[i] = messageArgs[i];
                }
            }
            return newMessageArgs;
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
            _UsePredefinedArguments = other._UsePredefinedArguments;
            _MessageArgs = other._MessageArgs;
            CaughtException = other.CaughtException;
        }

        public DiagnosticMessage Info { get; }
        public int LineStart { get; private set; }
        public int LineEnd { get; private set; }
        public int ColumnStart { get; private set;  }
        public int ColumnEnd { get; private set; }
        public string Message { get; private set; }
        public Exception CaughtException { get; }
        private object[] _MessageArgs;
        /// <summary>
        /// Does this diagnostic use predefined arguments.
        /// </summary>
        private bool _UsePredefinedArguments;

        /// <summary>
        /// Text representation of a diagnostic for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            string location;
            if (LineStart == LineEnd)
            {
                //Single line diagnostic
                location = $"Line {LineStart}[{ColumnStart},{ColumnEnd}]";
            }
            else
            {
                //Multi-line diagnostic
                location = $"Range ({LineStart}, {ColumnStart}) -> ({LineEnd}, {ColumnEnd})";
            }

            return $"{location} <{Info.Code}, {Info.Severity}, {Info.Category}> - {Message}";
        }

        private void ApplyMessageAndPosition([NotNull] Position position)
        {
            System.Diagnostics.Debug.Assert(position != null);
            LineStart = position.LineStart;
            LineEnd = position.LineEnd;
            ColumnStart = position.ColumnStart;
            ColumnEnd = position.ColumnEnd;
            Message = position._fromIcludingDirective && Message != null ? Message : string.Format(Info.MessageTemplate,
                _UsePredefinedArguments ? TranslateArguments(_MessageArgs) : _MessageArgs);
            Message = position.AdaptMessage(Message);
        }

        /// <summary>
        /// Equivalent to Clone() method but restricted to inheritors.
        /// </summary>
        /// <returns>Duplicate instance of this Diagnostic.</returns>
        protected virtual Diagnostic Duplicate() => new Diagnostic(this);

        /// <summary>
        /// Shift the diagnostic in source. Used in incremental mode to relocate a diagnostic
        /// after a line has been inserted or removed.
        /// </summary>
        /// <param name="offset">Positive or negative number of lines</param>
        internal void Shift(int offset)
        {
            LineStart += offset;
            LineEnd += offset;
        }

        /// <summary>
        /// Creates a copy of this diagnostic and position it on the new supplied location.
        /// </summary>
        /// <param name="newPosition">Non-null instance of Position to locate the diagnostic in code.</param>
        /// <returns>New instance of diagnostic, copied from this one.</returns>
        public Diagnostic CopyAt([NotNull] Position newPosition)
        {
            var copy = Duplicate();
            copy.ApplyMessageAndPosition(newPosition);
            return copy;
        }
    }
}
