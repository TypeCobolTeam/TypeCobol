using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace TypeCobol.Logging
{
    /// <summary>
    /// Implements ILogger by writing to a text file.
    /// Does not support metrics.
    /// </summary>
	public class FileLogger : ILogger
    {
        public static readonly string DefaultTimestampFormat = "yyyy-MM-dd_HH-mm-ss";

        private readonly StreamWriter _output;
        private readonly int _threshold;

        /// <summary>
        /// By default, each log entry is timestamped. Use this property to control the timestamp format.
        /// Set to null to disable timestamps.
        /// </summary>
        public string TimestampFormat { get; set; }

        /// <summary>
        /// Creates a new FileLogger.
        /// </summary>
        /// <param name="path">Path to the file to write to. If the file does not exist, it is created. Otherwise it is overwritten.</param>
        /// <param name="encoding">Text encoding.</param>
        /// <param name="level">Trace level, use null to log exceptions only.</param>
        public FileLogger(string path, Encoding encoding, LogLevel? level)
        {
            _output = new StreamWriter(File.Create(path), encoding);
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
