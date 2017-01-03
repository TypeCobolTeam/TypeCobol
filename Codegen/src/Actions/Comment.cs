using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Comment Node Action
    /// </summary>
    public class Comment : EventArgs, Action, IEraseAction
    {
        public string Group { get; private set; }
        internal Node Node;
        /// <summary>
        /// Get the list of Erased Nodes
        /// </summary>
        public IList<Node> ErasedNodes
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="node">The target node to be commented</param>
        public Comment(Node node) 
        { 
            this.Node = node; 
        }

        /// <summary>
        /// Execute the action
        /// </summary>
        public void Execute() 
        { 
            comment(this.Node);
            List<Node> erasedNodes = new List<Node>();
            erasedNodes.Add(this.Node);
            this.Node.ListChildren(erasedNodes);
            erasedNodes.TrimExcess();
            ErasedNodes = erasedNodes;
        }
        /// <summary>
        /// Mark this node that it must be commented with all its chilbren.
        /// </summary>
        /// <param name="node"></param>
        private void comment(Node node)
        {
            node.Comment = true;
            foreach (var child in node.Children) 
                comment(child);
        }
    }
}