using System.Collections.Generic;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.Parser
{
	internal static class DiagnosticUtils
	{
		internal static void AddError(CodeElement e, string message, Antlr4.Runtime.RuleContext context, MessageCode code = MessageCode.SyntaxErrorInParser) {
			AddError(e, message, ParseTreeUtils.GetFirstToken(context), new RuleStackBuilder().GetRuleStack(context), code);
		}
		internal static void AddError(CodeElement e, string message, MessageCode code = MessageCode.SyntaxErrorInParser) {
            if (e.Diagnostics == null) e.Diagnostics = new List<Diagnostic>();
			e.Diagnostics.Add(new ParserDiagnostic(message, e.StartIndex+1, e.StopIndex+1, e.ConsumedTokens[0].Line, null, code));
		}
		internal static void AddError(CodeElement e, string message, Scanner.Token token, string rulestack = null, MessageCode code = MessageCode.SyntaxErrorInParser) {
            if (e.Diagnostics == null) e.Diagnostics = new List<Diagnostic>();
            e.Diagnostics.Add(new ParserDiagnostic(message, token, rulestack, code));
		}
	}
}
