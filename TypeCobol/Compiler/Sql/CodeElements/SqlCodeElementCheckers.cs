using System.Collections.Generic;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    public static class IntegrateAlterSequenceStatementChecker
    {
        public static void OnCodeElement(AlterSequenceStatement alterSequenceStatement, List<SqlCodeElementBuilder.AlterSequenceClauseTypes> duplicatedClauses,
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
                alterSequenceStatement.NoCache == false)
            {
                DiagnosticUtils.AddError(alterSequenceStatement, "At least one option must be specified", context);
            }
        }
    }
}