using System;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    public static class SavepointStatementChecker
    {
        public static void OnCodeElement(SavepointStatement savepointStatement,
            CodeElementsParser.SavepointStatementContext context)
        {
            if (savepointStatement.Name != null)
            {
                var savePointName = savepointStatement.Name.ToString();
                if (savePointName.StartsWith("SYS", StringComparison.OrdinalIgnoreCase))
                {
                    DiagnosticUtils.AddError(savepointStatement,
                        "Invalid savepoint-name, it must not begin with 'SYS'.",
                        context);
                }
            }
        }
    }

    internal static class ExecuteImmediateStatementChecker
    {
        public static void OnCodeElement(ExecuteImmediateStatement executeImmediateStatement, CodeElementsParser.ExecuteImmediateStatementContext context)
        {
            var expressionType = executeImmediateStatement.Expression?.ExpressionType;
            switch (expressionType)
            {
                case SqlExpressionType.Variable:
                    var variable = (SqlVariable) executeImmediateStatement.Expression;
                    if (variable.Type != VariableType.HostVariable)
                    {
                        DiagnosticUtils.AddError(executeImmediateStatement,
                            "The sql variable must be a host variable in EXECUTE IMMEDIATE statement",
                            context);
                    }
                    else
                    {
                        if (((HostVariable) variable).IndicatorReference != null)
                        {
                            DiagnosticUtils.AddError(executeImmediateStatement,
                                "An indicator variable must not be specified with a host variable in EXECUTE IMMEDIATE statement",
                                context);
                        }
                    }

                    break;

                case SqlExpressionType.Constant:
                    var constant = (SqlConstant)executeImmediateStatement.Expression;
                    if (constant.Type != SqlConstantType.CharacterString)
                    {
                        DiagnosticUtils.AddError(executeImmediateStatement,
                            "Invalid literal found after EXECUTE IMMEDIATE, variable or string-expression was expected.",
                            context);
                    }
                    break;
                case null:
                    break;
                default:
                DiagnosticUtils.AddError(executeImmediateStatement,
                    "Invalid expression found after EXECUTE IMMEDIATE, variable or string-expression was expected.",
                    context);
                break;

            }
        }

    }
}
