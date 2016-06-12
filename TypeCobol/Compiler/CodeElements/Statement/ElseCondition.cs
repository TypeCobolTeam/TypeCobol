using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// If ELSE statement-2 is specified, statement-2 is executed. If statement-2 contains a
    /// procedure-branching or conditional statement, control is transferred, according
    /// to the rules for that statement. If statement-2 does not contain a
    /// procedure-branching or conditional statement, control is passed to the next
    /// executable statement after the corresponding END-IF or separator period.
    /// </summary>
    public class ElseCondition : StatementElement
    {
        public ElseCondition() : base(CodeElementType.ElseCondition, StatementType.ElseCondition)
        { }
    }
}
