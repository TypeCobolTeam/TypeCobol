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
        /// True if this replace action is for a "replace SET <boolean> TO FALSE" pattern, false otherwise.
        /// </summary>
        private bool IsReplaceSetBool;
        private bool UseRazor;

        /// <summary>
        /// Determine if the given node, the given template and the set of template variables correspond to a "replace SET <boolean> TO FALSE"
        /// pattern
        /// </summary>
        /// <param name="node">The Node to check</param>
        /// <param name="template">The replacement template to check</param>
        /// <param name="variables">The set of variable templates</param>
        /// <returns>true if all parameters corresponds to a "replace SET <boolean> TO FALSE" pattern, false otherwise</returns>
        private static bool IsSetBoolVarTemplate(Node node, string template, Dictionary<string, object> variables)
        {
            return(node is TypeCobol.Compiler.Nodes.Set) && template.Equals("SET %receiver-false TO TRUE") && variables != null &&
                variables.ContainsKey("receiver");
        }

        /// <summary>
        /// Determines if the given node is a node that is a qualifier access node.
        /// </summary>
        /// <param name="node"></param>
        /// <returns></returns>
        private static bool IsQualifiedNodeReceiver(Node node)
        {
            //The node is a qualifier access node if all its children are of type: TypeCobol.Codegen.Actions.Qualifier.GenerateToken
            //That is to say, the Qualifier action has already detected the situation.
            if (node.Children.Count == 0)
                return false;
            foreach (Node child in node.Children)
                if (!(child is TypeCobol.Codegen.Actions.Qualifier.GenerateToken))
                    return false;
            return true;
        }

        /// <summary>
        /// Check for a customization of the replacement
        /// </summary>
        /// <param name="node">The old node</param>
        /// <param name="template">The template to apply</param>
        /// <param name="variables">The substitution Variable for the new GenerateNode based on the template</param>
        /// <param name="group"></param>
        /// <param name="delimiter">Aragument variable delimiter</param>
        /// <returns>The new template to apply for a customization, or the same template otherwise</returns>
        private string CheckCustomReplace(Node node, string template, Dictionary<string, object> variables, string group, string delimiter)
        {
            IsReplaceSetBool = IsSetBoolVarTemplate(node, template, variables) && IsQualifiedNodeReceiver(node);
            if (IsReplaceSetBool)
            {
              //Method : Optimization don't use Razor just create adequate TypeCobol.Codegen.Actions.Qualifier.GenerateToken.
              //In addition this method can allow the Code Generator to detect a column 72 overflow.
                int count = node.Children.Count;
                TypeCobol.Codegen.Actions.Qualifier.GenerateToken token = node.Children[0] as TypeCobol.Codegen.Actions.Qualifier.GenerateToken;
                //Add the -false
                token.ReplaceCode = token.ReplaceCode + "-false";
                //Create a Token to replase the "false" to TRUE ==> lookup for the last one;
                var consumedTokens = node.CodeElement.ConsumedTokens;
                count = consumedTokens.Count;
                for (int i = count - 1; i >= 0; i--)
                {
                    var consumed_token = consumedTokens[i];
                    if (consumed_token.TokenType == Compiler.Scanner.TokenType.FALSE)
                    {//Add a GenerateNode to replace false by TRUE
                        TypeCobol.Codegen.Actions.Qualifier.GenerateToken trueToken =
                            new TypeCobol.Codegen.Actions.Qualifier.GenerateToken(
                                new TypeCobol.Codegen.Actions.Qualifier.TokenCodeElement(consumed_token), "TRUE", null);
                        node.Add(trueToken);
                        break;
                    }
                }
                UseRazor = false;
            }
            return template;
        }
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
            UseRazor = true;
            template = CheckCustomReplace(node, template, variables, group, delimiter);
            //Substitute any group code
            if (UseRazor)
            {
                if (group != null) this.Group = new TypeCobol.Codegen.Skeletons.Templates.RazorEngine().Replace(group, variables, delimiter);
                var solver = TypeCobol.Codegen.Skeletons.Templates.RazorEngine.Create(template, variables, delimiter);
                //JCM Give to the replaced form the same Code element So that positions will be calculated correctly
                this.New = new GeneratedNode((TypeCobol.Codegen.Skeletons.Templates.RazorEngine)solver, Old.CodeElement);
            }
        }

        /// <summary>
        /// Perform the action
        /// </summary>
        public void Execute()
        {
            //No need to replace an erased node.
            if (!Old.IsFlagSet(Node.Flag.GeneratorErasedNode) || Old.IsFlagSet(Node.Flag.PersistentNode))
            {
                if (UseRazor)
                {
                    // transfer Old's children to New
                    for (int i = Old.Children.Count - 1; i >= 0; --i)
                    {
                        var child = Old.Children[i];
                        Old.Remove(child);
                        if (!IsReplaceSetBool)
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
}