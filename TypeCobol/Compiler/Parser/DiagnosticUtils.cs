using System;
using System.Collections.Generic;
using Antlr4.Runtime;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Scanner;

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
		internal static void AddError(CodeElement e, string message, IToken token, string rulestack = null, MessageCode code = MessageCode.SyntaxErrorInParser) {
            if (e.Diagnostics == null) e.Diagnostics = new List<Diagnostic>();
		    var parserDiag = new ParserDiagnostic(message, token, rulestack, code);
            e.Diagnostics.Add(parserDiag);
        }
        internal static void AddErrorWithNoRuleStack(CodeElement e, string message, Antlr4.Runtime.RuleContext context, MessageCode code = MessageCode.SyntaxErrorInParser)
	    {
	        Token token = ParseTreeUtils.GetFirstToken(context);
            if (e.Diagnostics == null) e.Diagnostics = new List<Diagnostic>();
	        var parserDiag = new ParserDiagnostic(message, token.StartIndex + 1, token.StopIndex + 1, token.Line, null, code);
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

	    internal static void AddError(Node node, string message, SymbolReference symbol, MessageCode code = MessageCode.SyntaxErrorInParser)
	    {
	        var diagnostic = new ParserDiagnostic(message, symbol.NameLiteral.Token, null, code);
	        node.AddDiagnostic(diagnostic);
	    }

	    internal static void AddError(Node node, string message, DataDefinitionEntry data, MessageCode code = MessageCode.SyntaxErrorInParser)
	    {
	        ParserDiagnostic diagnostic;

	        if (data?.DataName != null)
	        {
	            diagnostic = new ParserDiagnostic(message, data?.DataName != null ? data.DataName.NameLiteral.Token : data.ConsumedTokens[0], null, code);
                node.AddDiagnostic(diagnostic);
            }
	        else
	        {
	            AddError(node, message, code);
            }
	    }

        #endregion
    }

    public static class DiagnosticPositionHelper
    {
        public static Diagnostic.Position Position(this IToken token)
        {
            if (token == null) return Diagnostic.Position.Default;

            if (token is Token scannerToken) return Position(scannerToken);

            if (token is CodeElement codeElement) return Position(codeElement);

            throw new NotSupportedException($"Unsupported IToken implementation '{token.GetType().FullName}'.");
        }

        public static Diagnostic.Position Position(this Token token)
        {
            if (token == null) return Diagnostic.Position.Default;

            var copyDirective = token is Preprocessor.ImportedToken importedToken ? importedToken.CopyDirective : null;
            return new Diagnostic.Position(token.Line, token.Column, token.EndColumn, copyDirective);
        }

        public static Diagnostic.Position Position(this CodeElement codeElement)
        {
            if (codeElement == null) return Diagnostic.Position.Default;

            var copyDirective = codeElement.IsInsideCopy() ? codeElement.FirstCopyDirective : null;
            return new Diagnostic.Position(codeElement.Line, codeElement.StartIndex + 1, codeElement.StopIndex + 1, copyDirective);
        }
    }
}
