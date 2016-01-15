using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Parser
{
    internal static class DiagnosticUtils
    {
        internal static void AddError(CodeElement e, string message, Antlr4.Runtime.RuleContext context)
        {
            string rulestack = new RuleStackBuilder().GetRuleStack(context);
            AddError(e, message, ParseTreeUtils.GetFirstToken(context), rulestack);
        }
        internal static void AddError(CodeElement e, string message)
        {
            var diagnostic = new ParserDiagnostic(message, e.StartIndex+1, e.StopIndex+1, e.ConsumedTokens[0].Line, null);
            e.Diagnostics.Add(diagnostic);
        }

        internal static void AddError(CodeElement e, string message, Scanner.Token token, string rulestack)
        {
            var diagnostic = new ParserDiagnostic(message, token, rulestack);
            e.Diagnostics.Add(diagnostic);
        }

        internal static void AddWarning(CodeElement e, string message, Antlr4.Runtime.RuleContext context)
        {
            var diagnostic = new ParserDiagnostic(message, ParseTreeUtils.GetFirstToken(context), new RuleStackBuilder().GetRuleStack(context), Diagnostics.MessageCode.SyntaxWarningInParser);
            e.Diagnostics.Add(diagnostic);
        }
    }
}
