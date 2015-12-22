using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Parser.Generated;

namespace TypeCobol.Compiler.Diagnostics
{
    class DataDescriptionChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(DataDescriptionEntry), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext c) {
            var data = e as DataDescriptionEntry;
            var context = c as CobolCodeElementsParser.DataDescriptionEntryContext;

            var picture   = GetContext(data, context.pictureClause());
            var blank     = GetContext(data, context.blankWhenZeroClause());
            var external  = GetContext(data, context.externalClause());
            var global    = GetContext(data, context.globalClause());
            var justified = GetContext(data, context.justifiedClause());
            var sync      = GetContext(data, context.synchronizedClause());
            var group     = GetContext(data, context.groupUsageClause());
            var usage     = GetContext(data, context.usageClause());
            var sign      = GetContext(data, context.signClause());
            var occurs    = GetContext(data, context.occursClause());
            var value     = GetContext(data, context.valueClause());

            if (data.DataName == null) {
                if ((data.LevelNumber == 77 || data.LevelNumber == 88) && !data.IsFiller)
                    DiagnosticUtils.AddError(data, "Data name must be specified for level-66 or level-88 items", context.levelNumber());
                if (data.IsExternal)
                    DiagnosticUtils.AddError(data, "Data name must be specified for any entry containing the EXTERNAL clause", external);
                if (data.IsGlobal)
                    DiagnosticUtils.AddError(data, "Data name must be specified for any entry containing the GLOBAL clause", global);
            }
        }

        public static T GetContext<T>(CodeElement e, T[] contexts, bool checkErrors = true) where T: Antlr4.Runtime.ParserRuleContext {
            if (contexts == null) return null;
            if (contexts.Length < 1) return null;
            if (checkErrors) {
                for (int c = 1; c < contexts.Length; c++)
                    DiagnosticUtils.AddError(e, "Only one such clause allowed", contexts[c]);
            }
            return contexts[0];
        }
    }

    class AddStatementChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(AddStatement), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext context) {
            var s = e as AddStatement;
            var c = context as CobolCodeElementsParser.AddStatementFormat2Context;
            if (c == null) return; //we only check format 2
            if (c.GIVING() == null)
                DiagnosticUtils.AddError(s, "Required: <identifier> after TO", c.identifierOrNumericLiteralTmp());
        }
    }

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
