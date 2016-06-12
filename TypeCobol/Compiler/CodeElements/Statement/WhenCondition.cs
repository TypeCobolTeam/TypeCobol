using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Conditional expression case for the EVALUATE and SEARCH statements.
    /// </summary>
    public class WhenCondition : StatementElement
    {
        public WhenCondition() : base(CodeElementType.WhenCondition, StatementType.WhenCondition)
        { }
    }
}
