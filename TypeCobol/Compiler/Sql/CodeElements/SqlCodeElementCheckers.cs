using System;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    internal static class SavepointStatementChecker
    {
        public static void OnCodeElement(SavepointStatement savepointStatement, CodeElementsParser.SavepointStatementContext context)
        {
            var savePointName = savepointStatement.Name.ToString();
            if (savePointName.StartsWith("SYS", StringComparison.OrdinalIgnoreCase))
            {
                DiagnosticUtils.AddError(savepointStatement, "Invalid savepoint-name, it must not begin with 'SYS'.",
                    context);
            }
        }
    }
}
