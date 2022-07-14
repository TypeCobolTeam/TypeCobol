using System;
using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    internal static class SavepointStatementChecker
    {
        public static void OnCodeElement(SavepointStatement savepointStatement, CodeElementsParser.SavepointStatementContext context)
        {
            if (savepointStatement.Name != null)
            {
                var savePointName = savepointStatement.Name.ToString();
                if (savePointName.StartsWith("SYS", StringComparison.OrdinalIgnoreCase))
                {
                    DiagnosticUtils.AddError(savepointStatement,
                        "Invalid savepoint-name, it must not begin with 'SYS'.", context);
                }
            }
        }
    }

    internal static class IntegrateAlterSequenceStatementChecker
    {
        public static void OnCodeElement(AlterSequenceStatement alterSequenceStatement, List<ParserRuleContextWithDiagnostics> duplicatedClauses,
            CodeElementsParser.AlterSequenceStatementContext context)
        {
            foreach (var clauseType in duplicatedClauses)
            {
                DiagnosticUtils.AddError(alterSequenceStatement, clauseType + " can not be specified more than once",
                    context);
            }

            if (alterSequenceStatement != null && alterSequenceStatement.MinValue == null &&
                alterSequenceStatement.MaxValue == null &&
                alterSequenceStatement.Ordered == null && alterSequenceStatement.Restart == null &&
                alterSequenceStatement.IncrementValue == null && alterSequenceStatement.Cycle == null &&
                alterSequenceStatement.CacheSize == null)
            {
                DiagnosticUtils.AddError(alterSequenceStatement, "At least one option must be specified", context);
            }
        }
    }
}
