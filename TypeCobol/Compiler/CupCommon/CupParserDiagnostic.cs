using System;
using System.Text;
using Antlr4.Runtime;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;

namespace TypeCobol.Compiler.CupCommon
{
    /// <summary>
    /// Enhanced error message containing additional information about 
    /// the origin of the syntax error in the grammar : offending symbol, rule stack
    /// </summary>
    public class CupParserDiagnostic : Diagnostic
    {
        public CupParserDiagnostic(string message, IToken offendingSymbol, string ruleStack, MessageCode code = MessageCode.SyntaxErrorInParser, Exception exception = null)
            : base(code, offendingSymbol.Position(), message, exception)
        {
            OffendingSymbol = offendingSymbol;
            this.ruleStack = ruleStack;
        }

        private CupParserDiagnostic(CupParserDiagnostic other)
            : base(other)
        {
            OffendingSymbol = other.OffendingSymbol;
            this.ruleStack = other.ruleStack;
        }

        /// <summary>
        /// First token which did not match the current rule in the grammar
        /// </summary>
        public IToken OffendingSymbol { get; }

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

        protected override Diagnostic Duplicate() => new CupParserDiagnostic(this);
    }
}
