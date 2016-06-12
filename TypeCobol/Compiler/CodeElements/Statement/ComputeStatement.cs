using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p317:
    /// The COMPUTE statement assigns the value of an arithmetic expression to one or
    /// more data items.
    ///
    /// With the COMPUTE statement, arithmetic operations can be combined without the
    /// restrictions on receiving data items imposed by the rules for the ADD, SUBTRACT,
    /// MULTIPLY, and DIVIDE statements.
    ///
    /// When arithmetic operations are combined, the COMPUTE statement can be more
    /// efficient than the separate arithmetic statements written in a series.
    /// </summary>
    public class ComputeStatement : StatementElement
    {
        public ComputeStatement() : base(CodeElementType.ComputeStatement, StatementType.ComputeStatement)
        { }
    }
}
