using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Comment Node Action
    /// </summary>
    public class Clone : EventArgs, Action, ICloneAction
    {
        public string Group { get; private set; }
        internal Node Node;
        private GeneratorActions GA;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="node">The target node to be cloned</param>
        public Clone(Node node, GeneratorActions generatorActions)
        {
            this.Node = node;
            this.GA = generatorActions;
        }

        /// <summary>
        /// Execute the action
        /// </summary>
        public IList<Action> Execute()
        {
            return DoClone(this.Node);
        }

        /// <summary>
        /// Collect all actions on the node
        /// </summary>
        /// <param name="node"></param>
        /// <param name="actions"></param>
        private void CollectActions(Node node, List<Action> actions)
        {
            actions.AddRange(GA.GetActions(node));
            foreach (var child in node.Children)
            {
                CollectActions(child, actions);
            }
        }

        /// <summary>
        /// Clone this node and all its children..
        /// </summary>
        /// <param name="node">The node to be cloned</param>
        private IList<Action> DoClone(Node node)
        {
            if (!node.IsFlagSet(Node.Flag.IsCloned))
            {
                Node cloned = (Node)node.Clone();
                //No parent for the cloned node.
                cloned.SetParent(null);
                //Do not comment a cloned node
                cloned.SetFlag(Node.Flag.IgnoreCommentAction, true);
                cloned.SetFlag(Node.Flag.IsCloned, true);
                ClonedNodes = new List<Node>() {cloned};
               
                List<Action> actions = new List<Action>();
                CollectActions(cloned, actions);
                return actions;
            }

            return null;
        }

        public IList<Node> ClonedNodes { get; private set; }
    }
}