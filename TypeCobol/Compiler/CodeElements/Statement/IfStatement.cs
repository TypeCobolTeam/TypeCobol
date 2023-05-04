using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p341: IF statement
    /// The IF statement evaluates a condition and provides for alternative actions in the
    /// object program, depending on the evaluation. 
    /// </summary>
    public class IfStatement : StatementElement {
        public IfStatement() : base(CodeElementType.IfStatement, StatementType.IfStatement) { }
        public ConditionalExpression Condition { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, Condition);
        }
    }


    
    /// <summary>
    /// If ELSE statement-2 is specified, statement-2 is executed. If statement-2 contains a
    /// procedure-branching or conditional statement, control is transferred, according
    /// to the rules for that statement. If statement-2 does not contain a
    /// procedure-branching or conditional statement, control is passed to the next
    /// executable statement after the corresponding END-IF or separator period.
    /// </summary>
    public class ElseCondition : StatementElement {
        public ElseCondition() : base(CodeElementType.ElseCondition, StatementType.ElseCondition) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }



    /// <summary>
    /// NEXT SENTENCE in IF statements:
    /// p341:
    /// The NEXT SENTENCE phrase transfers control to an implicit CONTINUE
    /// statement immediately following the next separator period.
    ///
    /// When NEXT SENTENCE is specified with END-IF, control does not pass to
    /// the statement following the END-IF. Instead, control passes to the
    /// statement after the closest following period.
    ///
    /// p342:
    /// Transferring control
    /// The topic describes the actions to take when conditions tested is true or false.
    ///
    /// If the condition tested is true, (...)
    /// If NEXT SENTENCE is specified, control passes to an implicit CONTINUE
    /// statement immediately preceding the next separator period.
    ///
    /// If the condition tested is false, (...)
    /// If ELSE NEXT SENTENCE is specified, control passes to an implicit CONTINUE
    /// STATEMENT immediately preceding the next separator period.
    ///
    /// NEXT SENTENCE in SEARCH statements:
    /// p409:
    /// NEXT SENTENCE transfers control to the first statement following the closest
    /// separator period.
    ///
    /// When NEXT SENTENCE is specified with END-SEARCH, control does not pass to
    /// the statement following the END-SEARCH. Instead, control passes to the statement
    /// after the closest following period.
    ///
    /// For the format-2 SEARCH ALL statement, neither imperative-statement-2 nor NEXT
    /// SENTENCE is required. Without them, the SEARCH statement sets the index to
    /// the value in the table that matched the condition.
    ///
    /// The function of the NEXT SENTENCE phrase is the same for a serial search and a
    /// binary search.
    /// </summary>
    public class NextSentenceStatement : StatementElement {
        public NextSentenceStatement() : base(CodeElementType.NextSentenceStatement, StatementType.NextSentenceStatement) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }



    public class IfStatementEnd: CodeElementEnd {
        public IfStatementEnd() : base(CodeElementType.IfStatementEnd) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
