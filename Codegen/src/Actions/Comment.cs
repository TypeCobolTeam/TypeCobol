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

        private bool Commented
        {
            get;
            set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="node">The target node to be commented</param>
        /// <param name="bComment">true to comment, false to uncomment</param>
        public Comment(Node node, bool bComment = true) 
        { 
            this.Node = node;
            this.Commented = bComment;
        }

        /// <summary>
        /// Execute the action
        /// </summary>
        public IList<Action> Execute() 
        {
            if (!this.Node.IsFlagSet(Node.Flag.IgnoreCommentAction))
            {
                comment(this.Node);
                if (Commented)
                {
                    List<Node> erasedNodes = new List<Node>();
                    erasedNodes.Add(this.Node);
                    this.Node.ListChildren(erasedNodes);
                    erasedNodes.TrimExcess();
                    ErasedNodes = erasedNodes;
                }
            }

            return null;
        }
        /// <summary>
        /// Mark this node that it must be commented with all its chilbren.
        /// </summary>
        /// <param name="node"></param>
        private void comment(Node node)
        {
            node.Comment = Commented;
            foreach (var child in node.Children)
                comment(child);
        }
    }
}