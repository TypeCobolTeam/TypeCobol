using System;
using System.Collections.Generic;
using System.Linq;
using Antlr4.Runtime;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
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
            var parserDiag = new ParserDiagnostic(message, e.Position(), null, code);
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
	        var parserDiag = new ParserDiagnostic(message, token.Position(), null, code);
	        e.Diagnostics.Add(parserDiag);
        }

        #region Node Diagnostic Generator

        public static void AddError(Node node, string message, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null)
	    {
            var diagnostic = new Diagnostic(code, node.CodeElement.Position(), message, exception);
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

    /// <summary>
    /// Helper class to build Position object required to instantiate new diagnostics.
    /// </summary>
    public static class DiagnosticPositionHelper
    {
        /// <summary>
        /// Position from an IToken.
        /// </summary>
        /// <param name="token">IToken instance, must be of type <see cref="Token"/> or <see cref="CodeElement"/>.</param>
        /// <returns>Position of this IToken, or Default if supplied token is null.</returns>
        public static Diagnostic.Position Position(this IToken token)
        {
            if (token == null) return Diagnostic.Position.Default;

            if (token is Token scannerToken) return Position(scannerToken);

            if (token is CodeElement codeElement) return Position(codeElement);

            throw new NotSupportedException($"Unsupported IToken implementation '{token.GetType().FullName}'.");
        }

        /// <summary>
        /// Position from a Token.
        /// </summary>
        /// <param name="token">Token instance.</param>
        /// <returns>Position of this Token, or Default if supplied token is null.</returns>
        public static Diagnostic.Position Position(this Token token)
        {
            if (token == null) return Diagnostic.Position.Default;

            var copyDirective = token is Preprocessor.ImportedToken importedToken ? importedToken.CopyDirective : null;
            return new Diagnostic.Position(token.Line, token.Column, token.EndColumn, copyDirective);
        }

        /// <summary>
        /// Position from a CodeElement.
        /// </summary>
        /// <param name="codeElement">CodeElement instance.</param>
        /// <returns>Position of this CodeElement, or Default if supplied CE is null.</returns>
        public static Diagnostic.Position Position(this CodeElement codeElement)
        {
            if (codeElement == null) return Diagnostic.Position.Default;

            CopyDirective copyDirective;
            if (codeElement.IsAcrossSourceFile())
            {
                //using first token as reference if the CE is split across program and copy
                copyDirective = codeElement.ConsumedTokens.FirstOrDefault() is Preprocessor.ImportedToken importedToken
                    ? importedToken.CopyDirective
                    : null;
            }
            else if (codeElement.IsInsideCopy())
            {
                //inside copy
                copyDirective = codeElement.FirstCopyDirective;
            }
            else
            {
                //inside program
                copyDirective = null;
            }

            return new Diagnostic.Position(codeElement.Line, codeElement.StartIndex + 1, codeElement.StopIndex + 1, copyDirective);
        }
    }
}
