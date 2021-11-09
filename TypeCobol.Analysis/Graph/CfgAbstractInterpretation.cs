using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// An abstract interpretation class
    /// </summary>
    /// <typeparam name="N"></typeparam>
    /// <typeparam name="D"></typeparam>
    public partial class CfgAbstractInterpretation<N, D>
    {
        /// <summary>
        /// Run an abstract interpretation on the given cfg graph.
        /// </summary>
        /// <param name="cfg">The CFG Graph</param>
        /// <param name="observers">A list of observers if any</param>
        /// <returns>Metrics collected during abstract interpretation</returns>
        public static Metrics Run(ControlFlowGraph<N, D> cfg, params IObserver[] observers)
        {
            return Run(cfg, 0, observers);
        }
        /// <summary>
        /// Run an abstract interpretation on the given cfg graph.
        /// </summary>
        /// <param name="cfg">The CFG Graph</param>
        /// <param name="cyclicExecThreshold">Number of maximum cyclic execution by block</param>
        /// <param name="observers">A list of observers if any</param>
        /// <returns>Metrics collected during abstract interpretation</returns>
        public static Metrics Run(ControlFlowGraph<N, D> cfg, int cyclicExecThreshold, params IObserver[] observers)
        {
            Environment env = new Environment(observers);
            return env.Run(cfg, cyclicExecThreshold);
        }
    }
}
