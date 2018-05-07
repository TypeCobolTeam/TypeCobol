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
        /// Emit the Report in the given TextWriter
        /// </summary>
        /// <param name="writer"></param>
        void Report(TextWriter writer);
    }
}
