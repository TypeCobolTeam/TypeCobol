using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Comment Node Action
    /// </summary>
    public class Comment : Action
    {
        public string Group { get; private set; }
        internal Node Node;

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