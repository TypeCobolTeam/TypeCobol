using System;
using System.Collections.Generic;
using Antlr4.Runtime.Tree;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    internal static class SavepointStatementChecker
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
                        "Invalid savepoint-name, it must not begin with 'SYS'.", context);
                }
            }
        }
    }

    internal static class AlterSequenceStatementChecker
    {
        public static void OnCodeElement(AlterSequenceStatement alterSequenceStatement,
            List<IParseTree> duplicatedClauses, bool emptyClauseSet,
            CodeElementsParser.AlterSequenceStatementContext context)
        {
            foreach (var clauseType in duplicatedClauses)
            {
                DiagnosticUtils.AddError(alterSequenceStatement,
                    clauseType.GetChild(0).GetText() + " can not be specified more than once",
                    context);
            }

            if (emptyClauseSet)
            {
                DiagnosticUtils.AddError(alterSequenceStatement, "At least one option must be specified", context);
            }
        }
    }
}
