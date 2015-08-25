using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.Parser
{
    class StatementsBuilder
    {
        internal MoveStatement CreateMoveStatement(Generated.CobolCodeElementsParser.MoveStatementContext context)
        {
            var sending   = SyntaxElementBuilder.CreateIdentifierOrLiteral(context.identifierOrLiteral());
            var receiving = SyntaxElementBuilder.CreateIdentifiers(context.identifier());
            var statement = new MoveStatement(sending, receiving, context.corresponding() != null);
            if (context.corresponding() != null)
            {
                if (sending as Literal != null)
                    DiagnosticUtils.AddError(statement, "MOVE CORRESPONDING: illegal <literal> before TO", context.identifierOrLiteral());
                if (receiving != null && receiving.Count > 1)
                    DiagnosticUtils.AddError(statement, "MOVE CORRESPONDING: maximum 1 group item after TO", context.identifierOrLiteral());
            }
            foreach (var identifier in receiving)
            {
                var function = identifier as FunctionReference;
                if (function != null)
                {
                    var rulestack = new TypeCobol.Compiler.AntlrUtils.RuleStackBuilder().GetRuleStack(context);
                    DiagnosticUtils.AddError(statement, "MOVE: illegal <intrinsic function> after TO", function.Symbol.NameToken, rulestack);
                }
            }
            return statement;
        }

        internal ReturnStatement CreateReturnStatement(Generated.CobolCodeElementsParser.ReturnStatementContext context)
        {
            if (context == null) return null;
            var filename = SyntaxElementBuilder.CreateFileName(context.fileName());
            var identifier = SyntaxElementBuilder.CreateIdentifier(context.identifier());
            var statement = new ReturnStatement(filename, identifier);
            return statement;
        }
    }
}
