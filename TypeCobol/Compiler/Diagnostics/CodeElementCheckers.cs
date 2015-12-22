using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Diagnostics
{
    class StartStatementChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(StartStatement), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext context) {
            var s = e as StartStatement;
            var c = context as CobolCodeElementsParser.StartStatementContext;
            if (c.relationalOperator() != null)
                if (s.Operator != '=' && s.Operator != '>' && s.Operator != '≥')
                    DiagnosticUtils.AddError(s, "START: Illegal operator "+s.Operator, c.relationalOperator());
        }
    }

    class StopStatementChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(StopStatement), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext context) {
            var s = e as StopStatement;
            var c = context as CobolCodeElementsParser.StopStatementContext;
            if (c.literal() != null)
                if (s.Literal != null && s.Literal.All)
                    DiagnosticUtils.AddError(s, "STOP: Illegal ALL", c.literal());
        }
    }
}
