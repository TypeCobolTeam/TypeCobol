using System.Collections.Generic;
using Analytics;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.Parser
{
	internal static class DiagnosticUtils
	{
		internal static void AddError(CodeElement e, string message, Antlr4.Runtime.RuleContext context, MessageCode code = MessageCode.SyntaxErrorInParser) {
			AddError(e, message, ParseTreeUtils.GetFirstToken(context), RuleStackBuilder.GetRuleStack(context), code);
		}
		internal static void AddError(CodeElement e, string message, MessageCode code = MessageCode.SyntaxErrorInParser) {
            if (e.Diagnostics == null) e.Diagnostics = new List<Diagnostic>();
            var parserDiag = new ParserDiagnostic(message, e.StartIndex + 1, e.StopIndex + 1, e.ConsumedTokens[0].Line, null, code);
            e.Diagnostics.Add(parserDiag);    
        }
		internal static void AddError(CodeElement e, string message, Scanner.Token token, string rulestack = null, MessageCode code = MessageCode.SyntaxErrorInParser) {
            if (e.Diagnostics == null) e.Diagnostics = new List<Diagnostic>();
            var parserDiag = new ParserDiagnostic(message, token, rulestack, code);
            e.Diagnostics.Add(parserDiag);
        }
	}
}
