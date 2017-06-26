using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Action to replace a Node by a new Generated one. The Old node is commented but not its children.
    /// The children are copied in the new Generated Node.
    /// </summary>
    public class Replace : EventArgs, Action
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
            //JCM Give to the replaced form the same Code element So that positions will be calculated correctly
            this.New = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver, Old.CodeElement);
        }

        /// <summary>
        /// Perform the action
        /// </summary>
        public void Execute()
        {
            //No need to replace an erased node.
            if (!Old.IsFlagSet(Node.Flag.GeneratorErasedNode) || Old.IsFlagSet(Node.Flag.PersistentNode))
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
}