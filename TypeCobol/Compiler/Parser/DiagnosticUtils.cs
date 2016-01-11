using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Parser
{
    internal static class DiagnosticUtils
    {
        internal static void AddError(CodeElement e, string message, Antlr4.Runtime.RuleContext context)
        {
            string rulestack = new RuleStackBuilder().GetRuleStack(context);
            /*
            var tokens = ParseTreeUtils.GetTokensList(context);
            ParserDiagnostic diagnostic = null;
            if (tokens.Count > 0) {
                var first = tokens[0];
                var last = tokens[tokens.Count-1];
                diagnostic = new ParserDiagnostic(message, first.StartIndex+1, last.StopIndex+1, first.Line, rulestack);
            } else {
                diagnostic = new ParserDiagnostic(message, null, rulestack);
            }
            e.Diagnostics.Add(diagnostic);
            */
            AddError(e, message, ParseTreeUtils.GetFirstToken(context), rulestack);
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
