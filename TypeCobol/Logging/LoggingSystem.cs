using System;
using System.Collections.Generic;
using System.Threading;

namespace TypeCobol.Logging
{
	public static class LoggingSystem
    {
        private class LoggerThread
        {
            private static readonly TimeSpan _Frequency = TimeSpan.FromSeconds(1);
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
                        Monitor.Wait(_syncLock, _Frequency);
                        while (_work.Count > 0)
                        {
                            var action = _work.Dequeue();
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

            public void AddWork(Action<ILogger> action)
            {
                _work.Enqueue(action);
            }

            public void Stop()
            {
                _stop = true;
                lock (_syncLock)
                {
                    Monitor.Pulse(_syncLock);
                }

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

        public static void RegisterLogger(ILogger logger)
        {
            _Loggers.Add(logger);
        }

        public static void LogMessage(LogLevel level, string message, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogMessage(level, message, contextData));
        }

        public static void LogException(Exception exception, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogException(exception, contextData));
        }

        public static void LogMetric(string name, double value, string unit = null, IDictionary<string, object> contextData = null)
        {
            _LoggerThread.AddWork(logger => logger.LogMetric(name, value, unit, contextData));
        }

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
