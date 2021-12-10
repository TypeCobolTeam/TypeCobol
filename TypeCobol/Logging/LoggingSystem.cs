using System;
using System.Collections.Generic;
using System.Threading;

namespace TypeCobol.Logging
{
    /// <summary>
    /// Single point of logging in the TypeCobol solution.
    /// Allows to register loggers and provide methods to log exceptions, messages and metrics.
    /// </summary>
	public static class LoggingSystem
    {
        /// <summary>
        /// Wraps a thread to asynchronously dispatch logging actions to actual loggers.
        /// </summary>
        private class LoggerThread
        {
            //Check queue every...
            private static readonly TimeSpan _Period = TimeSpan.FromSeconds(1);

            //Allowed time to flush remaining actions before ending the process.
            private static readonly TimeSpan _StopTimeout = TimeSpan.FromMilliseconds(500);

            private readonly Queue<Action<ILogger>> _work;
            private Thread _thread;
            private readonly object _syncLock;
            private bool _stop;

            public LoggerThread()
            {
                _work = new Queue<Action<ILogger>>();
                _thread = new Thread(Log);
                _syncLock = new object();
                _stop = false;

                _thread.Start();
            }

            private void Log()
            {
                while (!_stop)
                {
                    lock (_syncLock)
                    {
                        Monitor.Wait(_syncLock, _Period); //Wait until pulsed or polling period expires
                        while (_work.Count > 0)
                        {
                            var action = _work.Dequeue();
                            //Dispatch to loggers
                            foreach (var logger in _Loggers)
                            {
                                try
                                {
                                    action(logger);
                                }
                                catch (Exception exception)
                                {
                                    Console.Error.WriteLine(exception.ToText(false)); //Complete exception chain but no StackTrace.
                                }
                            }
                        }
                    }
                }
            }

            public void AddWork(Action<ILogger> action) => _work.Enqueue(action);

            public void Stop()
            {
                _stop = true;

                //Pulse the thread to end as soon as possible
                lock (_syncLock)
                {
                    Monitor.Pulse(_syncLock);
                }

                //Wait for last actions to be processed
                _thread.Join(_StopTimeout);
                _thread = null;

                System.Diagnostics.Debug.Assert(_work.Count == 0);
            }
        }

        private static readonly LoggerThread _LoggerThread;
        private static readonly List<ILogger> _Loggers;

        static LoggingSystem()
        {
            _LoggerThread = new LoggerThread();
            _Loggers = new List<ILogger>();
        }

        /// <summary>
        /// Add a new logger.
        /// </summary>
        /// <param name="logger">Logger instance to register.</param>
        public static void RegisterLogger(ILogger logger)
        {
            _Loggers.Add(logger);
        }

        /// <summary>
        /// Log a custom string.
        /// </summary>
        /// <param name="level">Message level.</param>
        /// <param name="message">Message content.</param>
        /// <param name="contextData">Optional context data.</param>
        public static void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogMessage(level, message, contextData));
        }

        /// <summary>
        /// Log an exception.
        /// </summary>
        /// <param name="exception">Exception to log.</param>
        /// <param name="contextData">Optional context data.</param>
        public static void LogException(Exception exception, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogException(exception, contextData));
        }

        /// <summary>
        /// Log a metric.
        /// </summary>
        /// <param name="name">Name of the metric.</param>
        /// <param name="value">Measured value.</param>
        /// <param name="unit">Optional unit of the metric.</param>
        /// <param name="contextData">Optional context data.</param>
        public static void LogMetric(string name, double value, string unit = null, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogMetric(name, value, unit, contextData));
        }

        /// <summary>
        /// Process every remaining logging operations and close all loggers.
        /// </summary>
        /// <remarks>Do not use LoggingSystem after calling this !</remarks>
        public static void Shutdown()
        {
            _LoggerThread.Stop();
            foreach (var logger in _Loggers)
            {
                logger.Dispose();
            }
            _Loggers.Clear();
        }
    }
}
