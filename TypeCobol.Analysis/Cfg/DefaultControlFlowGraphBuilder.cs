using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Analysis.Cfg
{
    /// <summary>
    /// Default Control Flow Graph Builder with any object has Data.
    /// </summary>
    public class DefaultControlFlowGraphBuilder : ControlFlowGraphBuilder<object>
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parentCfgBuilder">Parent Control Flow Builder for a nested program</param>
        public DefaultControlFlowGraphBuilder(ControlFlowGraphBuilder<object> parentCfgBuilder = null) : base(parentCfgBuilder)
        {

        }

        /// <summary>
        /// Create a Fresh Control Flow Graph Builder.
        /// </summary>
        /// <returns>The fresh Control Flow Graph Builder</returns>
        protected override ControlFlowGraphBuilder<object> CreateFreshControlFlowGraphBuilder(ControlFlowGraphBuilder<object> parentCfgBuilder = null)            
        {
            return new DefaultControlFlowGraphBuilder(parentCfgBuilder);
        }
    }
}
