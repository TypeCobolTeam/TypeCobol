using System;
using System.Threading;

namespace TypeCobol.Editor
{
    class Timer
    {
        public double ms { get; set; }
        public bool IsRunning { get; private set; }
        private Action action;
        private Thread t;
        private bool timeout = false;

        public Timer(Action action, double ms = 1000)
        {
            this.action = action;
            this.ms = ms;
            IsRunning = false;
        }

        private void sleep()
        {
            Thread.Sleep(TimeSpan.FromMilliseconds(ms));
            timeout = true;
            stop();
        }

        internal void start(bool reset = true)
        {
            if (!reset && IsRunning) return;
            stop(false);
            IsRunning = true;
            t = new Thread(sleep);
            t.Start();
        }

        public void stop(bool callAction = true)
        {
            if (!IsRunning) return;
            if (!timeout && t != null)
            {
                t.Abort();
                t = null;
            }
            timeout = false;
            IsRunning = false;
            if (callAction) action();
        }
    }
}
