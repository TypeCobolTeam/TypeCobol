using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TypeCobol.Logging
{
	public static class LoggingSystemExtensions
    {
        private const string NULL = "!NULL!";

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
