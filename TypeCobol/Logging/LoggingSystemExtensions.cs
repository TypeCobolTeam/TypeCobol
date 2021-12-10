using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

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
	}
}
