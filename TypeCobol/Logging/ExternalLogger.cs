using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using TypeCobol.Tools;

namespace TypeCobol.Logging
{
	public class ExternalLogger : ILogger
    {
        private readonly ILogger[] _loggers;

        public ExternalLogger(string assemblyFilePath)
        {
            _loggers = Assembly.LoadFrom(assemblyFilePath).Activate<ILogger>().ToArray();
        }

        public void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData)
        {
            foreach (var logger in _loggers) logger.LogMessage(level, message, contextData);
        }

        public void LogException(Exception exception, IDictionary<string, object> contextData)
        {
            foreach (var logger in _loggers) logger.LogException(exception, contextData);
        }

        public void LogMetric(string name, double value, string unit, IDictionary<string, object> contextData)
        {
            foreach (var logger in _loggers) logger.LogMetric(name, value, unit, contextData);
        }

        public void Dispose()
        {
            foreach (var logger in _loggers) logger.Dispose();
        }
    }
}
