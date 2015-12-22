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
        public void OnCodeElement(CodeElement e, ParserRuleContext c) {
            var statement = e as AddStatement;
            var context = c as CobolCodeElementsParser.AddStatementFormat2Context;
            if (context == null) return; //we only check format 2
            if (context.GIVING() == null)
                DiagnosticUtils.AddError(statement, "Required: <identifier> after TO", context.identifierOrNumericLiteralTmp());
        }
    }

    class CancelStatementChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(CancelStatement), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext ctxt) {
            var statement = e as CancelStatement;
            var context = ctxt as CobolCodeElementsParser.CancelStatementContext;

            foreach (var item in statement.Items)
            {
                var literal = item as TypeCobol.Compiler.CodeElements.Expressions.Literal;
                if (literal != null && (literal.Value is double || literal.Value is long)) {
                    // we should link this error to the specific context.identifierOrLiteral[i] context
                    // corresponding to statement.Items[i], but since refactor in #157 it's not trivial anymore
                    DiagnosticUtils.AddError(statement, "CANCEL: <literal> must be alphanumeric", context);
                }
            }
        }
    }

    class SetStatementChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(TypeCobol.Compiler.CodeElements.Statement.SetStatementForIndex), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext c) {
            var sa = e as TypeCobol.Compiler.CodeElements.Statement.SetStatementForAssignation;
            if (sa != null) {
                var ca = c as CobolCodeElementsParser.SetStatementForAssignationContext;
                for (int i = 0; i < ca.setStatementForAssignationReceiving().Length; i++) {
                    if (i >= sa.ReceivingFields.Count) {
                        var ctxt = ca.setStatementForAssignationReceiving()[i];
                        DiagnosticUtils.AddError(sa, "Set: Receiving fields missing or type unknown before TO", ctxt);
                    }
                }
                if (sa.SendingField == null) {
                    DiagnosticUtils.AddError(sa, "Set: Sending field missing or type unknown after TO", ca.setStatementForAssignationSending());
                }
            }
            var si = e as TypeCobol.Compiler.CodeElements.Statement.SetStatementForIndex;
            if (si != null) {
                if (si.SendingField == null) {
                    var ci = c as CobolCodeElementsParser.SetStatementForIndexesContext;
                    DiagnosticUtils.AddError(si, "Set xxx up/down by xxx: Sending field missing or type unknown", ci);
                }
            }
        }
    }

    class StartStatementChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(StartStatement), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext c) {
            var statement = e as StartStatement;
            var context = c as CobolCodeElementsParser.StartStatementContext;
            if (context.relationalOperator() != null)
                if (statement.Operator != '=' && statement.Operator != '>' && statement.Operator != '≥')
                    DiagnosticUtils.AddError(statement, "START: Illegal operator "+statement.Operator, context.relationalOperator());
        }
    }

    class StopStatementChecker: CodeElementListener
    {
        public IList<Type> GetCodeElements() {
            return new List<Type>() { typeof(StopStatement), };
        }
        public void OnCodeElement(CodeElement e, ParserRuleContext c) {
            var statement = e as StopStatement;
            var context = c as CobolCodeElementsParser.StopStatementContext;
            if (context.literal() != null)
                if (statement.Literal != null && statement.Literal.All)
                    DiagnosticUtils.AddError(statement, "STOP: Illegal ALL", context.literal());
        }
    }
}
