using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    public class Erase : Action
    {
        public string Group { get; private set; }
        internal Node Node;
        private IEnumerable<string> Words;

        public Erase(Node node, string word)
        {
            this.Node = node;
            this.Words = new List<string> { word };
        }

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
