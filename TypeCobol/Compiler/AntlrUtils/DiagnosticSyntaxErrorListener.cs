using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Register all errors encountered by the Antlr parser in a list of Diagnostic objects
    /// </summary>
    public class DiagnosticSyntaxErrorListener : BaseErrorListener 
    {
        /// <summary>
        /// List of errors found by parsing the program
        /// </summary>
        public IList<ParserDiagnostic> Diagnostics { get; private set; }

        public DiagnosticSyntaxErrorListener()
        {
            Diagnostics = new List<ParserDiagnostic>();
        }

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
                if(isFirst) { isFirst = false;  }
                else
                {
                    ruleStack.Append('>');
                }
                ruleStack.Append(ruleInvocation);
            }

            // Register a new diagnostic
            ParserDiagnostic diagnostic = new ParserDiagnostic(msg, offendingSymbol, ruleStack.ToString());
            Diagnostics.Add(diagnostic);
        }
    }

    /// <summary>
    /// Enhanced error message containing additional information about 
    /// the origin of the syntax error in the grammar : offending symbol, rule stack
    /// </summary>
    public class ParserDiagnostic : Diagnostic
    {
		public ParserDiagnostic(string message, IToken offendingSymbol, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser) :
			base(code, offendingSymbol == null ? -1 : offendingSymbol.Column, offendingSymbol == null ? -1 : (offendingSymbol.StopIndex < 0 ? -1 : (offendingSymbol.StopIndex+1)), message)
		{
			OffendingSymbol = offendingSymbol;
			this.ruleStack = ruleStack;
		}

        public ParserDiagnostic(string message, int start, int stop, int line, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser)
            : base(code, start, stop, message) {
            this.line = line;
            this.ruleStack = ruleStack;
        }

        /// <summary>
        /// First token which did not match the current rule in the grammar
        /// </summary>
        public IToken OffendingSymbol { get; private set; }

        /// <summary>Grammar rules which were being recognized when an incorrect token occured.</summary>
        private string ruleStack;
        /// <summary>Line at wich the error occured.</summary>
        private int line = -1;

        public string ToStringWithRuleStack() {
            int lineindex = line;
            if (lineindex < 0 && OffendingSymbol != null) {
                lineindex = OffendingSymbol.Line;
            }
            if (lineindex < 0 && OffendingSymbol != null) {
                CodeElement e = OffendingSymbol as CodeElement;
                if (e != null && e.ConsumedTokens.Count > 0) lineindex = e.ConsumedTokens[0].Line;
            }
            // TO DO - IMPORTANT : this is the INITIAL line number, and not the CURRENT line number
            // This is enough to pass all unit tests, but will return false informations in real usage !
            // for real line number, use a Snapshot
            var str = new StringBuilder();
            str.Append(base.ToString()).Append(" (");
            if (ruleStack!=null) str.Append("RuleStack="+ruleStack+", ");
            if (OffendingSymbol!=null) {
                str.Append("OffendingSymbol=").Append(OffendingSymbol);
                var  importedToken = OffendingSymbol as ImportedToken;
                if (importedToken != null) {
                    str.Append(" in ").Append(importedToken.CopyDirective);
                }
            } else {
                str.Append("[").Append(ColumnStart).Append(">").Append(ColumnEnd).Append("]");
            }
            str.Append(" on line ").Append(lineindex);
            str.Append(")");
            return str.ToString();
        }
    }
}
