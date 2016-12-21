using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    public class Create : Action
    {
        public string Group { get; private set; }
        internal Node Parent;
        private Node Child;
        private int? position;

        public Create(Node parent, string template, Dictionary<string, object> variables, string group, string delimiter, int? position)
        {
            this.Parent = parent;
            if (group != null) this.Group = new TypeCobol.Codegen.Skeletons.Templates.RazorEngine().Replace(group, variables, delimiter);
            var solver = TypeCobol.Codegen.Skeletons.Templates.RazorEngine.Create(template, variables, delimiter);
            this.Child = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver);
            this.position = position;
        }

        public void Execute()
        {
            Parent.Add(Child, (position ?? -1));
        }
    }
}
