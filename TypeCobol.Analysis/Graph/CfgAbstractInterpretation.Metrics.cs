using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Graph
{
    public partial class CfgAbstractInterpretation<N,D>
    {
        /// <summary>
        /// Some metrics calculated during abstract Interpretations
        /// </summary>
        public class Metrics
        {
            /// <summary>
            /// Edge counter
            /// </summary>
            public int EdgeCount { get; internal set;}
            /// <summary>
            /// Node counter
            /// </summary>
            public int NodeCount { get; internal set; }
            /// <summary>
            /// Counter of the number of subranch accessed by control flow changing.
            /// </summary>
            public int ControlSubgraphCount { get; internal set; }

            public int HighCyclomaticComplexity => EdgeCount - NodeCount + 2;
            public int HighEssentialComplexityPath => HighCyclomaticComplexity - ControlSubgraphCount;
            public override string ToString()
            {
                return $"{{EdgeCount={EdgeCount}; NodeCount={NodeCount}; ControlSubgraphCount={ControlSubgraphCount}; HighCyclomaticComplexity={HighCyclomaticComplexity}; HighEssentialComplexityPath={HighEssentialComplexityPath}}}";
            }

        }
    }
}
