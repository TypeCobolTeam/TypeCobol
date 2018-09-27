using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Preprocessor;

namespace TypeCobol.Compiler.CupCommon
{
    /// <summary>
    /// Enhanced error message containing additional information about 
    /// the origin of the syntax error in the grammar : offending symbol, rule stack
    /// </summary>
    public class CupParserDiagnostic : Diagnostic
    {
        public CupParserDiagnostic(string message, IToken offendingSymbol, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null) :
			base(code, offendingSymbol == null ? -1 : offendingSymbol.Column, offendingSymbol == null ? -1 : (offendingSymbol.StopIndex < 0 ? -1 : (offendingSymbol.StopIndex+1)), offendingSymbol == null ? -1 : offendingSymbol.Line, message, exception)
		{
            OffendingSymbol = offendingSymbol;
            this.ruleStack = ruleStack;

            // TO DO - IMPORTANT : this is the INITIAL line number, and not the CURRENT line number
            // This is enough to pass all unit tests, but will return false informations in real usage !
            // for real line number, use a Snapshot
            if (Line < 0 && OffendingSymbol != null)
            {
                CodeElement e = OffendingSymbol as CodeElement;
                if (e != null && e.ConsumedTokens.Count > 0) Line = e.ConsumedTokens[0].Line;
            }
        }

        public CupParserDiagnostic(string message, int start, int stop, int line, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null)
            : base(code, start, stop, line, message, exception) {
            this.ruleStack = ruleStack;
        }

        /// <summary>
        /// First token which did not match the current rule in the grammar
        /// </summary>
        public IToken OffendingSymbol { get; private set; }

        /// <summary>Grammar rules which were being recognized when an incorrect token occured.</summary>
        private readonly string ruleStack;

        public string ToStringWithRuleStack()
        {
            var str = new StringBuilder();
            str.Append(base.ToString());
            if (ruleStack != null) str.Append(" RuleStack=" + ruleStack + ", ");
            if (OffendingSymbol != null)
            {
                str.Append(" OffendingSymbol=").Append(OffendingSymbol);
                var importedToken = OffendingSymbol as ImportedToken;
                if (importedToken != null)
                {
                    str.Append(" in ").Append(importedToken.CopyDirective);
                }
            }
            return str.ToString();
        }
    }
}
