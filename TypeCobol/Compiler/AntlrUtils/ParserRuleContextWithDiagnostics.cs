using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.AntlrUtils
{
    /// <summary>
    /// Extends the default parse tree node with a property used to collect all 
    /// types of problems encountered when trying to parse the associated rule.
    /// </summary>
    public class ParserRuleContextWithDiagnostics : ParserRuleContext
    {
        public ParserRuleContextWithDiagnostics()
        {
        }
        
        public override void CopyFrom(Antlr4.Runtime.ParserRuleContext ctx)
        {
            base.CopyFrom(ctx);
            var diagnostics = ctx as ParserRuleContextWithDiagnostics;
            if(diagnostics != null)
            {
                this._diagnostics = diagnostics._diagnostics;
            }
        }
        
        public ParserRuleContextWithDiagnostics(Antlr4.Runtime.ParserRuleContext parent, int invokingStateNumber)
            : base(parent, invokingStateNumber)
        {
        }

        private IList<Diagnostic> _diagnostics;

        public IList<Diagnostic> Diagnostics
        {
            get { return _diagnostics; }
        }

        public void AttachDiagnostic(Diagnostic diagnostic)
        {
            if (_diagnostics == null) _diagnostics = new List<Diagnostic>();
            _diagnostics.Add(diagnostic);
        }
    }
}
