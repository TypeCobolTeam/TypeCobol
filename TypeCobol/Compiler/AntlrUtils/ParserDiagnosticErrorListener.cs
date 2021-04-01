using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Register all errors encountered by the Antlr parser on all parse nodes of type ParserRuleContextWithDiagnostics 
    /// (insert the following option in the associated grammar : contextSuperClass=TypeCobol.Compiler.AntlrUtils.ParserRuleContextWithDiagnostics)
    /// </summary>
    public class ParserDiagnosticErrorListener : BaseErrorListener 
    {
        /// <summary>
        /// Register a ParserDiagnostic for each syntax error encountered by the parser
        /// </summary>
        public override void SyntaxError(IRecognizer recognizer, IToken offendingSymbol, int line, int charPositionInLine, string msg, RecognitionException e)
        {
            // Build a string representing the current grammar rules being recognized
            StringBuilder ruleStack = new StringBuilder();
            IList<String> stack = ((Antlr4.Runtime.Parser)recognizer).GetRuleInvocationStack();
            bool isFirst = true;
            foreach (string ruleInvocation in stack.Reverse())
            {
                // Hide the root rule which is useful for error recovery and perf, but does not exist in the pure Cobol grammar
                if (ruleInvocation == "cobolCodeElements") continue; 

                if(isFirst) { isFirst = false;  }
                else
                {
                    ruleStack.Append('>');
                }
                ruleStack.Append(ruleInvocation);
            }

            // Create a new diagnostic object
            ParserDiagnostic diagnostic = new ParserDiagnostic(msg, offendingSymbol, ruleStack.ToString());

            // Attach this diagnostic to the current parse tree rule
            var parser = (Antlr4.Runtime.Parser)recognizer;
            if (parser != null && parser.Context is ParserRuleContextWithDiagnostics)
            {
                var currentRuleContext = (ParserRuleContextWithDiagnostics)parser.Context;
                currentRuleContext.AttachDiagnostic(diagnostic);
            }
        }
    }

    /// <summary>
    /// Enhanced error message containing additional information about 
    /// the origin of the syntax error in the grammar : offending symbol, rule stack
    /// </summary>
    public class ParserDiagnostic : Diagnostic
    {
		public ParserDiagnostic(string message, IToken offendingSymbol, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null) :
			base(code, offendingSymbol.Position(), message, exception)
		{
			OffendingSymbol = offendingSymbol;
			this.ruleStack = ruleStack;
        }

        public ParserDiagnostic(string message, Position position, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null)
            : base(code, position, message, exception)
        {
            this.ruleStack = ruleStack;
        }

        /// <summary>
        /// First token which did not match the current rule in the grammar
        /// </summary>
        public IToken OffendingSymbol { get; private set; }

        /// <summary>Grammar rules which were being recognized when an incorrect token occured.</summary>
        private readonly string ruleStack;

        public string ToStringWithRuleStack() {
            var str = new StringBuilder();
            str.Append(base.ToString());
            if (ruleStack!=null) str.Append(" RuleStack="+ruleStack+", ");
            if (OffendingSymbol!=null) {
                str.Append(" OffendingSymbol=").Append(OffendingSymbol);
                var  importedToken = OffendingSymbol as ImportedToken;
                if (importedToken != null) {
                    str.Append(" in ").Append(importedToken.CopyDirective);
                }
            } 
            return str.ToString();
        }
    }
}
