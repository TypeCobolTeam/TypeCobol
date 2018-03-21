using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.LanguageServer.Utilities
{
    using System;
    using System.Collections.Concurrent;
    using System.Threading;
    using System.Threading.Tasks;
    using System.Collections.Generic;

    /// <summary>
    /// Fifo task scheduler 
    /// Taken from: https://orderofcode.com/2016/05/06/fifo-order-task-scheduler/
    /// </summary>
    public class OrderTaskScheduler : System.Threading.Tasks.TaskScheduler, IDisposable
    {
        /// <summary> FIFO Queue of tasks to be executed </summary>
        private readonly ConcurrentQueue<Task> _tasks = new ConcurrentQueue<Task>();
        /// <summary> Thread to proceed task queue </summary>
        private readonly Thread _workingThread;
        /// <summary> Signal for the working thread that new tasks have been added to the queue </summary>
        private readonly AutoResetEvent _signalThread = new AutoResetEvent(false);
        /// <summary> Indicates when thread should stop </summary>
        private bool _stopThread = false;
        /// <summary> Flag: Has Dispose already been called? </summary>
        private bool _disposed = false;


        /// <summary>
        /// Constructor
        /// </summary>
        public OrderTaskScheduler(string name)
        {
            //Starts thread, it will wait for a task to proceed
            _workingThread = new Thread(() => ThreadBody(
                    new WeakReference(this),
                    _tasks,
                    ref _stopThread,
                    _signalThread))
            {
                Name = String.IsNullOrEmpty(name) ?
                                "OrderTaskScheduler-WorkingThread" : name
            };
            _workingThread.Start();
        }


        /// <summary>
        /// Queues a task to the scheduler
        /// </summary>
        /// <param name="task">The task to be queued.</param>
        protected sealed override void QueueTask(Task task)
        {
            if (_stopThread == true)
            {
                throw new InvalidOperationException("Scheduler can't handle new tasks, because it has been stopped.");
            }

            _tasks.Enqueue(task);
            _signalThread.Set();
        }

        /// <summary>
        /// Executes tasks from queue
        /// </summary>
        /// <param name="schedulerWeakRef"> weak reference to the scheduler, to allow GC to remove it</param>
        /// <param name="tasks"> queue of tasks </param>
        /// <param name="stopThreadSignal"> indicates when thread should stop </param>
        /// <param name="startProcessSignal"> indicates if scheduler has been disposed </param>
        private static void ThreadBody(
            WeakReference schedulerWeakRef,
            ConcurrentQueue<Task> tasks,
            ref bool stopThreadSignal,
            AutoResetEvent startProcessSignal)
        {
            while (true)
            {
                if (stopThreadSignal == true && tasks.IsEmpty)
                    return;

                startProcessSignal.WaitOne();

                Task item;
                while (tasks.TryDequeue(out item))
                {
                    // Executes the task we pulled out of the queue
                    object scheduler = schedulerWeakRef.Target;
                    if (scheduler != null)
                    {
                        ((OrderTaskScheduler)scheduler).TryExecuteTask(item);
                    }
                }

            }
        }

        /// <summary>
        /// Attempts to execute the specified task on the local thread.
        /// </summary>
        /// <param name="task">The task to be executed.</param>
        /// <param name="taskWasPreviouslyQueued"></param>
        /// <returns> Whether the task could be executed on the local thread.</returns>
        protected sealed override bool TryExecuteTaskInline(Task task, bool taskWasPreviouslyQueued)
        {
            //Allow inlining only for _workingThread
            if (Thread.CurrentThread.ManagedThreadId != _workingThread.ManagedThreadId)
            {
                return false;
            }

            // Try to run the task.
            return base.TryExecuteTask(task);
        }

        /// <summary>
        /// Gets the maximum concurrency level supported by this scheduler
        /// </summary>
        public sealed override int MaximumConcurrencyLevel { get { return 1; } }

        /// <summary>
        /// Gets an enumerable of the tasks currently scheduled on this scheduler.
        /// </summary>
        /// <returns>An enumerable of the tasks currently scheduled.</returns>
        protected sealed override IEnumerable<Task> GetScheduledTasks()
        {
            bool lockTaken = false;
            try
            {
                Monitor.TryEnter(_tasks, ref lockTaken);
                if (lockTaken)
                    return _tasks.ToArray();
                else
                    throw new NotSupportedException("Tasks queue can't be locked");
            }
            finally
            {
                if (lockTaken) Monitor.Exit(_tasks);
            }
        }

        #region IDisposable implementation
        // Public implementation of Dispose pattern
        public void Dispose()
        {
            Dispose(true);
            GC.SuppressFinalize(this);
        }

        // Protected implementation of Dispose pattern.
        protected virtual void Dispose(bool disposing)
        {
            if (_disposed)
                return;

            if (disposing)
            {
                //Stop local working Thread
                _stopThread = true;
                _signalThread.Set();
            }
            _disposed = true;
        }
        #endregion
    }

}
