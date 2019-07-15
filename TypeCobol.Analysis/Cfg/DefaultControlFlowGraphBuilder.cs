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
    public class DefaultControlFlowGraphBuilder<D> : ControlFlowGraphBuilder<D>  where D : new()
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parentCfgBuilder">Parent Control Flow Builder for a nested program</param>
        public DefaultControlFlowGraphBuilder(ControlFlowGraphBuilder<D> parentCfgBuilder = null) : base(parentCfgBuilder)
        {

        }

        /// <summary>
        /// Create a Fresh Control Flow Graph Builder.
        /// </summary>
        /// <returns>The fresh Control Flow Graph Builder</returns>
        protected override ControlFlowGraphBuilder<D> CreateFreshControlFlowGraphBuilder(ControlFlowGraphBuilder<D> parentCfgBuilder = null)            
        {
            return new DefaultControlFlowGraphBuilder<D>(parentCfgBuilder);
        }
    }
}
