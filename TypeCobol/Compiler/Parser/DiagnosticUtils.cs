using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Parser
{
    internal static class DiagnosticUtils
    {
        internal static void AddError(CodeElement e, string message, Antlr4.Runtime.RuleContext context)
        {
            string rulestack = new RuleStackBuilder().GetRuleStack(context);
            var diagnostic = new ParserDiagnostic(message, ParseTreeUtils.GetFirstToken(context), rulestack);
            e.Diagnostics.Add(diagnostic);
        }
    }
}
