using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.Diagnostics
{
    #region NodeCheckers

    class WhenConditionNodeChecker
    {
        public static void OnNode(Node node)
        {
            if (node.ChildrenCount == 0)
            {
                System.Diagnostics.Debug.Assert(node.Parent.ChildrenCount >= 2);
                // -1 is the Then node
                // -2 is the When group
                var whenGroup = node.Parent.Children[node.Parent.ChildrenCount - 2];
                System.Diagnostics.Debug.Assert(whenGroup.ChildrenCount > 0);

                var whenNode = whenGroup.Children[whenGroup.ChildrenCount - 1];
                System.Diagnostics.Debug.Assert(whenNode.CodeElement != null);
                System.Diagnostics.Debug.Assert(whenNode.CodeElement.Type == CodeElementType.WhenCondition ||
                    whenNode.CodeElement.Type == CodeElementType.WhenSearchCondition);

                //Syntax error.
                DiagnosticUtils.AddError(whenNode, "Missing statement in \"when\" clause");
            }
        }
    }
    #endregion
}
