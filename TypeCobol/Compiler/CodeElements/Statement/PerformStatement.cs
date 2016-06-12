using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p384: PERFORM statement
    /// imperative-statement-1
    /// The statements to be executed for an in-line PERFORM
    /// An in-line PERFORM must be delimited by the END-PERFORM phrase.
    /// </summary>
    public class PerformStatement : StatementElement
    {
        public PerformStatement() : base(CodeElementType.PerformStatement, StatementType.PerformStatement)
        { }
    }
}
