using System.Collections.Concurrent;
using System.Diagnostics;

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
            private static void SafeCallLogger(ILogger logger, Action<ILogger> action)
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

            private readonly ConcurrentQueue<Action<ILogger>> _work;
            private readonly Thread _thread;
            private readonly AutoResetEvent _signal;
            private bool _stop;

            public LoggerThread()
            {
                _work = new ConcurrentQueue<Action<ILogger>>();
                _thread = new Thread(Log) { IsBackground = true };
                _signal = new AutoResetEvent(false);
                _stop = false;
                _thread.Start();
            }

            private void Log()
            {
                while (!_stop)
                {
                    //Wait for signal
                    _signal.WaitOne();

                    //Process all pending logging actions
                    while (_work.TryDequeue(out var action))
                    {
                        //Dispatch to loggers
                        foreach (var logger in _Loggers)
                        {
                            SafeCallLogger(logger, action);
                        }
                    }
                }
            }

            public void AddWork(Action<ILogger> action)
            {
                // Assert that no attempt is made to log after the thread has been requested to stop
                Debug.Assert(!_stop);

                _work.Enqueue(action);
                _signal.Set();
            }

            public void Stop()
            {
                _stop = true;

                // Pulse the thread to end as soon as possible
                _signal.Set();

                // Wait for last actions to be processed
                _thread.Join();

                // Assert that no log were dropped
                Debug.Assert(_work.IsEmpty);
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
            LogException((Exception)args.ExceptionObject);
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
