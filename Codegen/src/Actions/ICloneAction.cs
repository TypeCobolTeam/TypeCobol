using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Interface of action which can render clones nodes.
    /// </summary>
    public interface ICloneAction
    {
        /// <summary>
        /// Get the List of Cloned Nodes after the Action has been performed.
        /// </summary>
        IList<Node> ClonedNodes { get; }
    }
}
