using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Sql.CodeElements
{
    class SqlCodeElementCheckers
    {
        public static void OnSavepointStatementChecker(CodeElement codeElement, CodeElementsParser.SavepointStatementContext context)
        {
            if (context.savepoint_name == null) return;
            var savePointName = context.savepoint_name.Text;
            if (savePointName.Length == 0 || savePointName.Length <= 3) return;
            var text = savePointName.Substring(0, 3);
            if (text.ToUpper() == "SYS")
            {
                DiagnosticUtils.AddError(codeElement, "Invalid savepoint-name, it must not begin with 'SYS'.",
                    context);
            }

        }
    }
}
