using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;
using TypeCobol.Compiler.Sql.CodeElements.Statements;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    class SqlCodeElementCheckers
    {
        public static void OnSetAssignmentStatementChecker(CodeElement codeElement,
            CodeElementsParser.SetAssignmentStatementContext context)
        {
            var statement = (SetAssignmentStatement) codeElement;
            foreach (var assignment in statement.Assignments)
            {
                if (assignment.Values != null && assignment.Targets != null &&
                    assignment.Values.Count != assignment.Targets.Count)
                {
                    DiagnosticUtils.AddError(statement,
                        "The number of source value specifications (expression, NULL, or DEFAULT) on the right side of the equal sign must match the number of target specifications on the left side of the statement.",
                        context);
                }
            }
        }
    }
}
