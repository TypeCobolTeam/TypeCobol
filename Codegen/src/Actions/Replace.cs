using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Action to replace a Node by a new Generated one. The Old node is comment but not its children.
    /// The clidren are copie din the new Generated Node.
    /// </summary>
    public class Replace : Action
    {
        public string Group { get; private set; }
        internal Node Old;
        private Node New;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="node">The old node</param>
        /// <param name="template">The template to apply</param>
        /// <param name="variables">The substitution Variable for the new GenerateNode based on the template</param>
        /// <param name="group"></param>
        /// <param name="delimiter">Aragument variable delimiter</param>
        public Replace(Node node, string template, Dictionary<string, object> variables, string group, string delimiter)
        {
            this.Old = node;
            //Substitute any group code
            if (group != null) this.Group = new TypeCobol.Codegen.Skeletons.Templates.RazorEngine().Replace(group, variables, delimiter);
            var solver = TypeCobol.Codegen.Skeletons.Templates.RazorEngine.Create(template, variables, delimiter);
            this.New = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver);
        }

        /// <summary>
        /// Perform the action
        /// </summary>
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
