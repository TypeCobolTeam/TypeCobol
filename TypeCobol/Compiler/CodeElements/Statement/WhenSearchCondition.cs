using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Conditional expression case for the SEARCH statement.
    /// </summary>
    public class WhenSearchCondition : StatementElement
    {
        public WhenSearchCondition() : base(CodeElementType.WhenSearchCondition, StatementType.WhenSearchCondition)
        { }
    }

    public class WhenSelectionObject
    {

    }
}
