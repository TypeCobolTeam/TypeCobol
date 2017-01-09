using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// An action which can Erase Node
    /// </summary>
    public interface IEraseAction
    {
        /// <summary>
        /// Get the List of Erased Nodes after the Action has been performed.
        /// </summary>
        IList<Node> ErasedNodes {get;}
    }
}
