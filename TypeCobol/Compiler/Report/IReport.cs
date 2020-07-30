using System.IO;

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
        /// <param name="writer">Output TextWriter</param>
        /// <param name="unit">The compilation unit for which to create a Report</param>
        void Report(TextWriter writer, CompilationUnit unit = null);
    }
}
