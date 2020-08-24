using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// Interface of a CFG Graph transformer
    /// </summary>
    public interface ICfgTransform<N, D>
    {
        /// <summary>
        /// Apply a Transformation on a CFG graph and return the resulting CFG Graph.
        /// </summary>
        /// <param name="cfg">The CFG graph to be transformed</param>
        /// <returns>The transformed CFG Graph</returns>
        ControlFlowGraph<N, D> Transform(ControlFlowGraph<N, D> cfg);
    }
}
