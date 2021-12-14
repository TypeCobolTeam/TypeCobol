using System;
using System.Collections.Generic;

namespace TypeCobol.Logging
{
    /// <summary>
    /// Writes messages and exceptions to the Console (metrics are ignored).
    /// </summary>
	public class ConsoleLogger : ILogger
    {
        private readonly int _threshold;

        /// <summary>
        /// Creates a new ConsoleLogger.
        /// </summary>
        /// <param name="level">Trace level, use null to log exceptions only.</param>
        public ConsoleLogger(LogLevel? level)
        {
            _threshold = level.HasValue ? (int) level.Value : int.MaxValue; //MaxValue -> do not display any message
        }

        public virtual void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData)
        {
            if ((int) level >= _threshold)
            {
                //Message level is above or equal our own threshold, display it.
                Console.WriteLine($"[{level}] {message} {contextData.ToText()}");
            }
        }

        public virtual void LogException(Exception exception, IDictionary<string, object> contextData)
        {
            Console.WriteLine(exception.ToText(includeStackTrace: true)); //Only base exception but with its StackTrace.
            Console.WriteLine(contextData.ToText());
        }

        public virtual void LogMetric(string name, double value, string unit, IDictionary<string, object> contextData)
        {
            //Do not display metrics
        }
    }
}
