using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.Diagnostics
{
    #region NodeCheckers

    static class WhenConditionNodeChecker
    {
        public static void OnNode(WhenGroup whenGroup)
        {
            if(whenGroup is WhenGroup)
            {
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
