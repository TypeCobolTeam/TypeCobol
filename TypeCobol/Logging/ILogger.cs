using System;
using System.Collections.Generic;

namespace TypeCobol.Logging
{
    /// <summary>
    /// Defines a suitable logger for TypeCobol.
    /// </summary>
    public interface ILogger
    {
        /// <summary>
        /// Log a custom string.
        /// </summary>
        /// <param name="level">Message level.</param>
        /// <param name="message">Message content.</param>
        /// <param name="contextData">Context data.</param>
        void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData);

        /// <summary>
        /// Log an exception.
        /// </summary>
        /// <param name="exception">Exception to log.</param>
        /// <param name="contextData">Context data.</param>
        void LogException(Exception exception, IDictionary<string, object> contextData);

        /// <summary>
        /// Log a metric.
        /// </summary>
        /// <param name="name">Name of the metric.</param>
        /// <param name="value">Measured value.</param>
        /// <param name="unit">Unit of the metric.</param>
        /// <param name="contextData">Context data.</param>
        void LogMetric(string name, double value, string unit, IDictionary<string, object> contextData);
    }
}
