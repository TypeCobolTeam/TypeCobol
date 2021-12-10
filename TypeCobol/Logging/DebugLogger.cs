using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace TypeCobol.Logging
{
    public class DebugLogger : ILogger
    {
        public virtual void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData)
        {
            Debug.WriteLine($"[{level}] {message} {contextData.ToText()}");
        }

        public virtual void LogException(Exception exception, IDictionary<string, object> contextData)
        {
            Debug.WriteLine(exception.ToText(false, true)); //Complete exception chain with each StackTrace.
            Debug.WriteLine(contextData.ToText());
        }

        public virtual void LogMetric(string name, double value, string unit, IDictionary<string, object> contextData)
        {
            Debug.WriteLine($"{name} = {value} ({unit}) {contextData.ToText()}");
        }

        public virtual void Dispose()
        {
            //Nothing to dispose
        }
    }
}
