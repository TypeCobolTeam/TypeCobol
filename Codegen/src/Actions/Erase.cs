using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Action action to remove a Node from the generated code.
    /// The Removed node will be commented, all its children will be cleared.
    /// If the erase action contains words to be erased from the input, a new GenerateNode is built
    /// and its ouput is the input without the word to be erased:
    /// 
    /// ex: move unsafe  dateFreeFormat to maDate
    /// 
    /// will be generated with the word unsafe to be erased to:
    /// 
    /// *    move unsafe  dateFreeFormat to maDate                              
    /// move         dateFreeFormat to maDate                              
    /// 
    /// </summary>
    public class Erase : Action
    {
        public string Group { get; private set; }
        internal Node Node;
        private IEnumerable<string> Words;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="node">The node to be erased with input word</param>
        /// <param name="word">The word to erase </param>
        public Erase(Node node, string word)
        {
            this.Node = node;
            this.Words = new List<string> { word };
        }

        /// <summary>
        /// Perform the action.
        /// </summary>
        public void Execute()
        {
            var solver = new Skeletons.Templates.Eraser(Node.CodeElement.SourceText, this.Words);
            bool somethingToDo = solver.Run();
            if (!somethingToDo) return;

            // retrieve data
            int index = this.Node.Parent.IndexOf(this.Node);
            if (index > -1)
            {
                var nodegen = new GeneratedNode(solver);
                this.Node.Parent.Add(nodegen, index + 1);
            }
            // comment out original "line" (=~ non expanded node)
            this.Node.Comment = true;
            this.Node.Clear();

        }
    }
}
