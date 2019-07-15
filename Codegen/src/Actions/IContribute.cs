using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Interface that all contribute action should implement
    /// </summary>
    public interface IContribute
    {
        /// <summary>
        /// Contribute method called by the Contribute action.
        /// </summary>
        /// <param name="parent">The parent of the new Generate Node to Create</param>
        /// <param name="pattern">The name of the pattern used</param>
        /// <param name="code">The code to apply</param>
        /// <param name="group">The Group ID</param>
        /// <param name="position">The Insertion position (index) as child in the Parent node</param>
        /// <param name="newline">Indicates if the action should be done on a new line</param>
        /// <returns>A Node if the contribution is to add a node in the parent node, null otherwise</returns>
        Node Contribute(Node parent, string pattern, string code, string group, int? position, bool newline);
    }
}
