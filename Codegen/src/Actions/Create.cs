using System;
using System.Collections.Generic;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Action to create a new Generate Node.
    /// </summary>
    public class Create : Action
    {
        public string Group { get; private set; }
        internal Node Parent;
        private Node Child;
        private int? position;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parent">The parent of the new Generate Node to Create</param>
        /// <param name="template">The new node generation Template</param>
        /// <param name="variables">The Substitution variables</param>
        /// <param name="group">The Group ID</param>
        /// <param name="delimiter">Substitution variable delimiter</param>
        /// <param name="position">The Insertion position (index) as child in the Parent node</param>
        public Create(Node parent, string template, Dictionary<string, object> variables, string group, string delimiter, int? position)
        {
            this.Parent = parent;
            if (group != null) this.Group = new TypeCobol.Codegen.Skeletons.Templates.RazorEngine().Replace(group, variables, delimiter);
            var solver = TypeCobol.Codegen.Skeletons.Templates.RazorEngine.Create(template, variables, delimiter);
            this.Child = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver);
            this.position = position;
        }

        /// <summary>
        /// Perform the creation action, the new GeneratedNode is added as child in the Parent node.
        /// </summary>
        public void Execute()
        {
            Parent.Add(Child, (position ?? -1));
        }
    }
}
