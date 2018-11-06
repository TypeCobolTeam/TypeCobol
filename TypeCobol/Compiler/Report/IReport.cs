using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Report
{
    /// <summary>
    /// Interface for a emitting a report.
    /// </summary>
    public interface IReport
    {
        /// <summary>
        /// The file where the report should be emited
        /// </summary>
        string Filepath { get; set; }

        /// <summary>
        /// Emit the Report in the given TextWriter
        /// </summary>
        /// <param name="writer"></param>
        void Report(TextWriter writer);
    }
}
