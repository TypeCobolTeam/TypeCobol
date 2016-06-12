using System;

namespace TypeCobol.Compiler.CodeElements
{
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
    public class NextSentenceStatement : StatementElement
    {
        public NextSentenceStatement() : base(CodeElementType.NextSentenceStatement, StatementType.NextSentenceStatement) { }
    }
}
