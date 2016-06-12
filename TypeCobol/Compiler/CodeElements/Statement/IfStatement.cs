using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p341: IF statement
    /// The IF statement evaluates a condition and provides for alternative actions in the
    /// object program, depending on the evaluation. 
    /// </summary>
    public class IfStatement : StatementElement
    {
        public IfStatement() : base(CodeElementType.IfStatement, StatementType.IfStatement)
        { }

        public ConditionalExpression condition = null;
    }
}
