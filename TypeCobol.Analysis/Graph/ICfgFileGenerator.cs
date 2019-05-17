using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// Interface for generator of a Control Flow Graph to a file format.
    /// </summary>
    public interface ICfgFileGenerator<N,D>
    {
        /// <summary>
        /// Generate the Control Flow Graph in the given TextWriter
        /// </summary>
        /// <param name="writer">The Writer in which to generate the graph</param>
        /// <param name="cfg">The Control Flow Graph to be Generated</param>
        void Generate(TextWriter writer, ControlFlowGraph<N, D> cfg);
    }
}
