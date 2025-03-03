using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Logging
{
    /// <summary>
    /// Helper class to format data when used with LoggingSystem.
    /// </summary>
    public static class LoggingSystemExtensions
    {
        private const string NULL = "!NULL!";

        /// <summary>
        /// Format context-data.
        /// </summary>
        /// <param name="contextData">Context data to format.</param>
        /// <returns>Standard textual representation of the given context data.</returns>
        public static string ToText(this IDictionary<string, object> contextData)
        {
            if (contextData == null) return string.Empty;

            return string.Join(" ", contextData.Select(PairToText));

            string PairToText(KeyValuePair<string, object> pair)
            {
                string value = pair.Value != null ? pair.Value.ToString() : NULL;
                return $"{{{pair.Key} = {value}}}";
            }
        }

        /// <summary>
        /// Format an exception.
        /// </summary>
        /// <param name="exception">Exception to format.</param>
        /// <param name="onlyBaseException">True to format only the base exception, False to use the whole exception chain; default is True.</param>
        /// <param name="includeStackTrace">True to include exception StackTrace, False to discard it; default is False.</param>
        /// <returns>Standard textual representation of the given exception.</returns>
        public static string ToText(this Exception exception, bool onlyBaseException = true, bool includeStackTrace = false)
        {
            var builder = new StringBuilder();
            var currentException = onlyBaseException ? exception.GetBaseException() : exception;
            bool outermost = true;
            while (currentException != null)
            {
                string header;
                if (outermost)
                {
                    header = "Exception";
                    outermost = false;
                }
                else
                {
                    header = "Caused by";
                }

                builder.AppendLine($"{header} {exception.GetType().FullName}: {exception.Message}");

                if (includeStackTrace)
                {
                    builder.AppendLine(exception.StackTrace);
                }

                currentException = currentException.InnerException;
            }

            return builder.ToString();
        }

        public static IDictionary<string, object> CreateDebugData(Node contextNode)
        {
            var result = new Dictionary<string, object>();
            result.Add("contextNode", ToText(contextNode));

            if (contextNode == null) return result;

            if (contextNode.CodeElement != null)
            {
                result.Add("line", contextNode.CodeElement.Line);
            }

            if (contextNode.Parent != null)
            {
                string programName = contextNode.Root.MainProgram?.Name ?? contextNode.Root.Name;
                string parentType = contextNode.Parent.GetType().FullName;
                string nodeBefore = ToText(GetNodeBefore(contextNode));
                string nodeAfter = ToText(GetNodeAfter(contextNode));
                result.Add("programName", programName);
                result.Add("parentType", parentType);
                result.Add("nodeBefore", nodeBefore);
                result.Add("nodeAfter", nodeAfter);
            }

            return result;

            string ToText(Node node, int indent = 0)
            {
                string indentString = new string(' ', 2 * indent);

                if (node == null) return indentString + NULL;

                string text;
                if (node.CodeElement != null)
                {
                    text = node.CodeElement.SafeGetSourceText();
                }
                else
                {
                    text = indentString + node.GetType().FullName;
                }

                foreach (var child in node.Children)
                {
                    text += Environment.NewLine;
                    text += ToText(child, indent + 1);
                }

                return text;
            }

            Node GetNodeBefore(Node node)
            {
                System.Diagnostics.Debug.Assert(node != null && node.Parent != null);
                int index = node.Parent.ChildIndex(node);
                return index > 0 ? node.Parent.Children[index - 1] : null;
            }

            Node GetNodeAfter(Node node)
            {
                System.Diagnostics.Debug.Assert(node != null && node.Parent != null);
                int index = node.Parent.ChildIndex(node);
                return index < node.Parent.ChildrenCount - 1 ? node.Parent.Children[index + 1] : null;
            }
        }

        #region Temporary debug code for #2332

        internal static string SafeGetSourceText(this CodeElement codeElement)
        {
            if (codeElement == null) return NULL; // Should not happen

            string result;
            try
            {
                result = codeElement.SourceText;
            }
            catch (Exception exception)
            {
                // Trace exception and attempt to read tokens directly
                var builder = new StringBuilder();
                builder.Append($"Could not dump CodeElement: {exception.GetType().FullName} - {exception.Message}");
                if (exception is ArgumentOutOfRangeException argumentOutOfRangeException)
                {
                    builder.Append($" - {argumentOutOfRangeException.ActualValue}");
                }

                builder.AppendLine();
                foreach (var token in codeElement.ConsumedTokens)
                {
                    builder.Append('<');
                    builder.Append(token.Text);
                    builder.Append("> ");
                }

                result = builder.ToString();
            }

            return result;
        }

        #endregion

        private static readonly HashSet<string> _SourceCodeDumpReasons = new HashSet<string>();

        /// <summary>
        /// Debug helper creating a series of log traces containing full source code and last incremental changes.
        /// </summary>
        /// <param name="reasonId">Unique identifier for the dump, this id is used to avoid dumping multiple
        /// times for the same reason.</param>
        /// <param name="message">Error message.</param>
        /// <param name="codeElementsLines">List of CodeElementsLines at time of log.</param>
        /// <param name="history">History of last incremental changes.</param>
        public static void LogErrorWithSourceCode(string reasonId, string message, IReadOnlyList<CodeElementsLine> codeElementsLines, IncrementalChangesHistory history)
        {
            // Fail immediately in debug. There is no point in dumping source code and this allows to see the error.
            System.Diagnostics.Debug.Fail(message);

            if (!_SourceCodeDumpReasons.Add(reasonId)) return; // Dump only once for this reason to avoid huge logs

            // Build a correlation id to group traces
            string correlationId = DateTime.Now.ToString("s") + "@" + Environment.MachineName;
            var currentTrace = new StringBuilder();

            if (codeElementsLines != null)
            {
                int traceId = 0;

                // Dump file structure
                IterateCodeElementsLines(DumpCodeElementTypes);

                // Dump source code
                IterateCodeElementsLines(codeElementsLine => currentTrace.AppendLine(codeElementsLine.Text));

                void IterateCodeElementsLines(Action<CodeElementsLine> appendLine)
                {
                    const int MAX_LINES_PER_LOG = 1000;
                    for (int i = 0; i < codeElementsLines.Count; i++)
                    {
                        appendLine(codeElementsLines[i]);

                        if ((i + 1) % MAX_LINES_PER_LOG == 0 || i == codeElementsLines.Count - 1)
                        {
                            var contextData = new Dictionary<string, object>()
                            {
                                { $"{correlationId} - part {traceId}", currentTrace.ToString() }
                            };
                            LoggingSystem.LogMessage(LogLevel.Error, message, contextData);
                            currentTrace.Clear();
                            traceId++;
                        }
                    }
                }

                void DumpCodeElementTypes(CodeElementsLine codeElementsLine)
                {
                    if (!codeElementsLine.HasCodeElements) return;

                    foreach (var codeElement in codeElementsLine.CodeElements)
                    {
                        currentTrace.AppendLine($"{codeElement.Line}: {codeElement.Type}");
                    }
                }
            }
            else
            {
                currentTrace.Append("No source code available");
            }

            if (history != null)
            {
                // Dump last incremental changes
                int changeId = -history.Depth + 1; //Number last changes from -N+1 to 0
                foreach (var textChangedEvent in history.TextChangedEvents)
                {
                    currentTrace.AppendLine($"change {changeId}:");
                    foreach (var documentChange in textChangedEvent.DocumentChanges)
                    {
                        string text = documentChange.NewLine == null ? string.Empty : $"\"{documentChange.NewLine.Text}\"";
                        currentTrace.AppendLine($"{documentChange.Type}@{documentChange.LineIndex} {text}");
                    }

                    changeId++;
                }
            }
            else
            {
                currentTrace.Append(" - No incremental changes history available");
            }

            LoggingSystem.LogMessage(LogLevel.Error, message, new Dictionary<string, object>() { { $"{correlationId} - changes", currentTrace.ToString() } });
        }
    }
}
