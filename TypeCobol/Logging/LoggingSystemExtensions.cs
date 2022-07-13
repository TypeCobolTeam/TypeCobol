using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using TypeCobol.Compiler.Nodes;

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
                    text = node.CodeElement.SourceText;
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
    }
}
