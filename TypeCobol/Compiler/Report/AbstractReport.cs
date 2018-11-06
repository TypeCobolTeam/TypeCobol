using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Report
{
    /// <summary>
    /// And abstract report class.
    /// </summary>
    public abstract class AbstractReport : IReport
    {
        private string _filepath;

        public string Filepath
        {
            get { return _filepath; }

            set { _filepath = value; }
        }

        /// <summary>
        /// Emit the Report in the given TextWriter
        /// </summary>
        /// <param name="writer"></param>
        public abstract void Report(TextWriter writer);

        /// <summary>
        /// Emit the report in the given filepath
        /// </summary>
        /// <param name="filepath">The File path</param>
        /// <exception cref="Exception">Any exception that occured</exception>
        public virtual void Report()
        {
            if (string.IsNullOrEmpty(_filepath))
                throw new FileLoadException($"Cannot find file at path: {_filepath}");

            using (FileStream fs = new FileStream(_filepath, FileMode.Create))
            {
                Report(fs);
            }
        }

        /// <summary>
        /// Emit the report in a stream.
        /// </summary>
        /// <param name="stream">The stream in which to emit the report.</param>
        public virtual void Report(Stream stream)
        {
            System.IO.StreamWriter writer = new StreamWriter(stream);
            Report(writer);
            writer.Flush();
        }
    }
}
