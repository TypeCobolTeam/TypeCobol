using System;
using System.Collections.Generic;
using Analytics;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;

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


        #region Node Diagnostic Generator

        public static void AddError(Node node, string message, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null)
	    {
            var diagnostic = new Diagnostic(code, node.CodeElement.StartIndex+1, node.CodeElement.StopIndex+1, node.CodeElement.Line, message, exception);
            node.AddDiagnostic(diagnostic);
        }

        internal static void AddError(Node node, string message, Antlr4.Runtime.RuleContext context, MessageCode code = MessageCode.SyntaxErrorInParser)
        {
            AddError(node, message, ParseTreeUtils.GetFirstToken(context), RuleStackBuilder.GetRuleStack(context), code);
        }

        internal static void AddError(Node node, string message, CodeElement context, MessageCode code = MessageCode.SyntaxErrorInParser)
        {   //CUP PORT -- TODO Get the rule concerned --> Uses a context that can provide such information
            AddError(node, message, context.ConsumedTokens != null ? context.ConsumedTokens[0] : null, /*RuleStackBuilder.GetRuleStack(context)*/ null, code);
        }

        internal static void AddError(Node node, string message, Scanner.Token token, string rulestack = null, MessageCode code = MessageCode.SyntaxErrorInParser)
        {
            var diagnostic = new ParserDiagnostic(message, token, rulestack, code);
            node.AddDiagnostic(diagnostic);
        }

        #endregion
    }
}
