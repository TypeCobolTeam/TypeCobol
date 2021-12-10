using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace TypeCobol.Logging
{
	public class FileLogger : ILogger
    {
        public static readonly string DefaultTimestampFormat = "yyyy-MM-dd_HH-mm-ss";

        private readonly StreamWriter _output;
        private readonly int _threshold;

        public string TimestampFormat { get; set; }

        public FileLogger(string path, Encoding encoding, LogLevel? level)
        {
            _output = new StreamWriter(File.OpenWrite(path), encoding);
            _threshold = level.HasValue ? (int) level.Value : int.MaxValue; //MaxValue -> do not write any message
            TimestampFormat = DefaultTimestampFormat;
        }

        private void WriteTimeStamp()
        {
            if (TimestampFormat != null)
            {
                _output.Write($"[{DateTime.Now.ToString(TimestampFormat)}] ");
            }
        }

        public virtual void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData)
        {
            if ((int) level >= _threshold)
            {
                //Message level is above or equal our own threshold, write it.
                WriteTimeStamp();
                _output.WriteLine($"[{level}] {message} {contextData.ToText()}");
            }
        }

        public virtual void LogException(Exception exception, IDictionary<string, object> contextData)
        {
            WriteTimeStamp();
            _output.WriteLine(exception.ToText(false, true)); //Complete exception chain with each StackTrace.
            _output.WriteLine(contextData.ToText());
        }

        public virtual void LogMetric(string name, double value, string unit, IDictionary<string, object> contextData)
        {
            //Do not write metrics
        }

        public virtual void Dispose()
        {
            _output.Dispose();
        }
    }
}
