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
        public static void OnCodeElement(ExecuteImmediateStatement executeImmediateStatement,
            CodeElementsParser.ExecuteImmediateStatementContext context)
        {
            if (executeImmediateStatement.StatementVariable != null)
            {
                //Check that no indicator is present
                var variable = executeImmediateStatement.StatementVariable;
                if (variable.Type == VariableType.HostVariable && ((HostVariable)variable).IndicatorReference != null)
                {
                    DiagnosticUtils.AddError(executeImmediateStatement,
                        "An indicator variable must not be specified with a host variable in EXECUTE IMMEDIATE statement",
                        context);
                }
            }
        }
    }
}
