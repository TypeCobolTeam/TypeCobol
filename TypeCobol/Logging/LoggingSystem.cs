using System.Collections.Concurrent;

namespace TypeCobol.Logging
{
    /// <summary>
    /// Single point of logging in the TypeCobol solution.
    /// Allows to register loggers and provides methods to log exceptions, messages and metrics.
    /// </summary>
    public static partial class LoggingSystem
    {
        /// <summary>
        /// Wraps a thread to asynchronously dispatch logging actions to actual loggers.
        /// </summary>
        private class LoggerThread
        {
            //Check action queue every 1.5s, this is also the maximum allotted
            //time to flush remaining logging actions before ending the process.
            private static readonly TimeSpan _Period = TimeSpan.FromMilliseconds(1500);

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

                // Pulse the thread to end as soon as possible
                lock (_waitLock)
                {
                    Monitor.Pulse(_waitLock);
                }

                // Wait for last actions to be processed
                _thread.Join(_Period);

                // Last chance ! Log errors into the console if we did not have enough time to use configured loggers
                if (!_work.IsEmpty)
                {
                    var remainingWork = _work.ToArray();
                    var consoleLogger = new ConsoleLogger(LogLevel.Error); // Drop infos and warnings, keep errors and exceptions
                    foreach (var action in remainingWork)
                    {
                        action(consoleLogger);
                    }
                }
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

        private static IDictionary<string, object> AddMinimalContext(IDictionary<string, object> contextData)
        {
            var result = contextData ?? new Dictionary<string, object>(2);
            result[ContextKeys.ParserVersion] = Parser.Version;
            result[ContextKeys.EnvironmentCommandLine] = Environment.CommandLine;
            return result;
        }

        /// <summary>
        /// Log a custom string.
        /// </summary>
        /// <param name="level">Message level.</param>
        /// <param name="message">Message content.</param>
        /// <param name="contextData">Optional context data.</param>
        public static void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogMessage(level, message, AddMinimalContext(contextData)));
        }

        /// <summary>
        /// Log an exception.
        /// </summary>
        /// <param name="exception">Exception to log.</param>
        /// <param name="contextData">Optional context data.</param>
        public static void LogException(Exception exception, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogException(exception, AddMinimalContext(contextData)));
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
            _LoggerThread.AddWork(logger => logger.LogMetric(name, value, unit, AddMinimalContext(contextData)));
        }
    }
}
