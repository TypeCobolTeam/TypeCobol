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

    public static class ExecuteImmediateStatementChecker
    {
        public static void OnCodeElement(ExecuteImmediateStatement executeImmediateStatement, CodeElementsParser.ExecuteImmediateStatementContext context)
        {
            var test = executeImmediateStatement.Expression is HostVariable;
            if (!test) return;
            var hostVariable = (HostVariable) executeImmediateStatement.Expression;
            if (hostVariable.IndicatorReference != null)
            {
                DiagnosticUtils.AddError(executeImmediateStatement,
                    "The host variable mustn't have any indicator variable",
                    context);
            }
        }
    }
}
