using System;
using System.Collections.Concurrent;
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
            private static readonly TimeSpan _Period = TimeSpan.FromMilliseconds(1500);

            //Allowed time to flush remaining actions before ending the process.
            private static readonly TimeSpan _StopTimeout = TimeSpan.FromMilliseconds(500);

            private readonly ConcurrentQueue<Action<ILogger>> _work;
            private readonly Thread _thread;
            private readonly object _waitLock;
            private bool _stop;

            public LoggerThread()
            {
                _work = new ConcurrentQueue<Action<ILogger>>();
                _thread = new Thread(Log) { IsBackground = true };
                _waitLock = new object();
                _stop = false;
                _thread.Start();
            }

            private void Log()
            {
                while (!_stop)
                {
                    lock (_waitLock)
                    {
                        Monitor.Wait(_waitLock, _Period); //Wait until pulsed or polling period expires
                        while (_work.TryDequeue(out var action))
                        {
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
                lock (_waitLock)
                {
                    Monitor.Pulse(_waitLock);
                }

                //Wait for last actions to be processed
                _thread.Join(_StopTimeout);

                System.Diagnostics.Debug.Assert(_work.Count == 0);
            }
        }

        private static readonly LoggerThread _LoggerThread;
        private static readonly List<ILogger> _Loggers;

        static LoggingSystem()
        {
            _LoggerThread = new LoggerThread();
            _Loggers = new List<ILogger>();

            var appDomain = AppDomain.CurrentDomain;
            appDomain.UnhandledException += OnUnhandledException;
            if (AppDomain.CurrentDomain.IsDefaultAppDomain())
            {
                //'Normal' event for .NET application exit
                appDomain.ProcessExit += OnApplicationExit;
            }
            else
            {
                //Used when loaded in a manually created AppDomain
                appDomain.DomainUnload += OnApplicationExit;
            }
        }

        private static void OnUnhandledException(object sender, UnhandledExceptionEventArgs args)
        {
            LogException((Exception) args.ExceptionObject);
        }

        private static void OnApplicationExit(object sender, EventArgs args)
        {
            _LoggerThread.Stop();
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
    }
}
