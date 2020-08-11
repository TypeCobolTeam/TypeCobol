using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Codegen.Nodes;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Codegen.Actions
{
    /// <summary>
    /// Action to replace a Node by a new Generated one. The Old node is commented but not its children.
    /// The children are copied in the new Generated Node.
    /// </summary>
    public class ReplaceCode : EventArgs, Action
    {
        public string Group { get; private set; }
        internal Node Old;
        private Node New;
        /// <summary>
        /// True if this replace action is for a "replace SET <boolean> TO FALSE" pattern, false otherwise.
        /// </summary>
        private bool IsReplaceSetBool;
        /// <summary>
        /// True if this replace action is for a "replace SET <pointer> UP BY <integer>" pattern, false otherwise.
        /// </summary>
        private bool IsReplaceSetUpByPointer;
        private bool UseRazor;

        /// <summary>
        /// Determine if the given node, the given template and the set of template variables correspond to a "replace SET <boolean> TO FALSE"
        /// pattern
        /// </summary>
        /// <param name="node">The Node to check</param>
        /// <param name="pattern">The name of the pattern to check</param>
        /// <returns>true if all parameters corresponds to a "replace SET <boolean> TO FALSE" pattern, false otherwise</returns>
        private static bool IsSetBoolVarTemplate(Node node, string pattern)
        {
            return (node is TypeCobol.Compiler.Nodes.Set) && "BOOL.SET".Equals(pattern);
        }

        /// <summary>
        /// Determine if the given node, the given template and the set of template variables correspond to POINTER INCREMENT"
        /// pattern
        /// </summary>
        /// <param name="node">The Node to check</param>
        /// <param name="pattern">The name of the pattern to check</param>
        /// <returns>true if all parameters corresponds to the POINTER INCREMENT, false otherwise</returns>
        private static bool IsSetPointerInrementVarTemplate(Node node, string pattern)
        {
            return (node is TypeCobol.Compiler.Nodes.Set) && "POINTER.INCREMENT".Equals(pattern);
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
        /// <param name="pattern">The pattern name</param>
        /// <returns>The new template to apply for a customization, or the same template otherwise</returns>
        private void CheckCustomReplace(Node node, string pattern)
        {
            IsReplaceSetBool = IsSetBoolVarTemplate(node, pattern) && IsQualifiedNodeReceiver(node);
            IsReplaceSetUpByPointer = IsSetPointerInrementVarTemplate(node, pattern) && IsQualifiedNodeReceiver(node);

            if (IsReplaceSetBool)
            {
                //Method : Optimization don't use Razor just create adequate TypeCobol.Codegen.Actions.Qualifier.GenerateToken.
                //In addition this method can allow the Code Generator to detect a column 72 overflow.
                int count = node.Children.Count;
                Node previousNode = null;

                foreach (var token in node.Children.Cast<Qualifier.GenerateToken>())
                {
                    if (token.IsFlagSet(Node.Flag.NodeContainsIndex)) continue;

                    if (previousNode == null || token.CodeElement.ConsumedTokens[0].TokenType == TokenType.UserDefinedWord && previousNode.CodeElement.ConsumedTokens[0].TokenType != TokenType.QualifiedNameSeparator)
                    {
                        //Add the -false
                        token.ReplaceCode += "-false";
                    }
                    previousNode = token;
                }

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
            else if (IsReplaceSetUpByPointer)
            {
                // In case of pointer incrementation, the name qualification is already done
                node.RemoveAllChildren();
            }
        }
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="node">The old node</param>
        /// <param name="pattern">The pattern's name</param>
        /// <param name="code">The code to be applied</param>
        /// <param name="group"></param>        
        public ReplaceCode(Node node, string pattern, string code, string group)
        {
            this.Old = node;
            UseRazor = true;
            CheckCustomReplace(node, pattern);
            //Substitute any group code
            if (UseRazor)
            {
                if (group != null) this.Group = group;
                //JCM Give to the replaced form the same Code element So that positions will be calculated correctly
                this.New = new GeneratedNode2(code, false, Old.CodeElement);
            }            
        }

        /// <summary>
        /// Perform the action
        /// </summary>
        public IList<Action> Execute()
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

            return null;
        }
    }
}
