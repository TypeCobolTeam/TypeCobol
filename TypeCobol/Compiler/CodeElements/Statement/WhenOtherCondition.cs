using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Conditional fallback case for the EVALUATE and SEARCH statements.
    /// </summary>
    public class WhenOtherCondition : StatementElement
    {
        public WhenOtherCondition() : base(CodeElementType.WhenOtherCondition, StatementType.WhenOtherCondition)
        { }
    }
}
