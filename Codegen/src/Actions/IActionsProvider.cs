using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// The Interface of an Action Provider clas.
    /// </summary>
    public interface IActionsProvider
    {
        /// <summary>
        /// Metthod for getting actions associated toa given node.
        /// </summary>
        /// <param name="Self">The Given node</param>
        /// <param name="SelfContext">The Generator Context</param>
        /// <returns>The list of actions if any, null otherwise</returns>
        List<TypeCobol.Codegen.Actions.Action> GetActions(TypeCobol.Compiler.Nodes.Node @Self, TypeCobol.Codegen.GeneratorActions @SelfContext);
    }
}
