using System;
using System.Collections.Generic;

namespace TypeCobol.Logging
{
	public interface ILogger : IDisposable
    {
        void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData);

        void LogException(Exception exception, IDictionary<string, object> contextData);

        void LogMetric(string name, double value, string unit, IDictionary<string, object> contextData);
    }
}
