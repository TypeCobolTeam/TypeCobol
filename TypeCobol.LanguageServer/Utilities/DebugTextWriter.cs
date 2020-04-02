using System;
using System.IO;
using System.Threading.Tasks;
using System.Diagnostics;

namespace TypeCobol.LanguageServer.Utilities
{
    /// <summary>
    /// This class is used only to redirect a Text witting to a Debug Output.
    /// So it is helpfull in debug mode.
    /// </summary>
    public class DebugTextWriter : TextWriter
    {
        /// <summary>
        /// Constructor
        /// </summary>
        public DebugTextWriter()
        {
        }

        public override IFormatProvider FormatProvider
        {
            get
            {
                return System.Console.Out.FormatProvider;
            }
        }
        public override void Flush()
        {
            Debug.Flush();            
        }
        public override Task FlushAsync()
        {            
            return new Task(() => Debug.Flush());
        }

        public override void Write(bool value)
        {
            Debug.Write(value);
        }
        public override void Write(char value)
        {
            Debug.Write(value);
        }
        public override void Write(char[] buffer)
        {
            Debug.Write(buffer);
        }
        public override void Write(decimal value)
        {
            Debug.Write(value);
        }
        public override void Write(double value)
        {
            Debug.Write(value);
        }
        public override void Write(float value)
        {
            Debug.Write(value);
        }
        public override void Write(int value)
        {
            Debug.Write(value);
        }
        public override void Write(long value)
        {
            Debug.Write(value);
        }
        public override void Write(object value)
        {
            Debug.Write(value);
        }
        public override void Write(string value)
        {
            Debug.Write(value);
        }
        public override void Write(uint value)
        {
            Debug.Write(value);
        }
        public override void Write(ulong value)
        {
            Debug.Write(value);
        }
        public override void Write(string format, object arg0)
        {
            Debug.Write(String.Format(format, arg0));
        }
        public override void Write(string format, params object[] arg)
        {
            Debug.Write(String.Format(format, arg));
        }
        public override void Write(char[] buffer, int index, int count)
        {
            Debug.Write(new String(buffer, index, count));
        }
        public override void Write(string format, object arg0, object arg1)
        {
            Debug.Write(String.Format(format, arg0, arg1));
        }
        public override void Write(string format, object arg0, object arg1, object arg2)
        {
            Debug.Write(String.Format(format, arg0, arg1, arg2));
        }
        public override Task WriteAsync(char value)
        {
            return new Task(() => Debug.Write(value));            
        }
        public new Task WriteAsync(char[] buffer)
        {
            return new Task(() => Debug.Write(buffer));            
        }
        public override Task WriteAsync(string value)
        {
            return new Task(() => Debug.Write(value));
        }
        public override Task WriteAsync(char[] buffer, int index, int count)
        {
            return new Task(() => Debug.Write(new String(buffer, index, count)));            
        }
        public override void WriteLine()
        {
            Debug.WriteLine(NewLine);
        }
        public override void WriteLine(bool value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(char value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(char[] buffer)
        {
            Debug.WriteLine(buffer);
        }
        public override void WriteLine(decimal value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(double value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(float value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(int value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(long value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(object value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(string value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(uint value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(ulong value)
        {
            Debug.WriteLine(value);
        }
        public override void WriteLine(string format, object arg0)
        {
            Debug.WriteLine(format, arg0);
        }
        public override void WriteLine(string format, params object[] arg)
        {
            Debug.WriteLine(format, arg);
        }
        public override void WriteLine(char[] buffer, int index, int count)
        {
            String msg = new String(buffer, index, count);
            Debug.WriteLine(msg);
        }
        public override void WriteLine(string format, object arg0, object arg1)
        {
            Debug.WriteLine(format, arg0, arg1);
        }
        public override void WriteLine(string format, object arg0, object arg1, object arg2)
        {
            Debug.WriteLine(format, arg0, arg1, arg2);
        }
        public override Task WriteLineAsync()
        {
            return new Task(() => Debug.Write(NewLine));
        }
        public override Task WriteLineAsync(char value)
        {
            return new Task(() => Debug.Write(value));
        }
        public override System.Text.Encoding Encoding
        {
            get
            {
                return System.Console.Out.Encoding;
            }
        }
    }
}
