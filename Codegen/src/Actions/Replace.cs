using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    public class Replace : Action
    {
        public string Group { get; private set; }
        internal Node Old;
        private Node New;

        public Replace(Node node, string template, Dictionary<string, object> variables, string group, string delimiter)
        {
            this.Old = node;
            if (group != null) this.Group = new TypeCobol.Codegen.Skeletons.Templates.RazorEngine().Replace(group, variables, delimiter);
            var solver = TypeCobol.Codegen.Skeletons.Templates.RazorEngine.Create(template, variables, delimiter);
            this.New = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver);
        }

        public void Execute()
        {
            // transfer Old's children to New
            for (int i = Old.Children.Count - 1; i >= 0; --i)
            {
                var child = Old.Children[i];
                Old.Remove(child);
                New.Add(child, 0);
            }
            Old.Comment = true;
            var parent = Old.Parent;
            int index = parent.IndexOf(Old);
            parent.Add(New, index + 1);
        }
    }
}
