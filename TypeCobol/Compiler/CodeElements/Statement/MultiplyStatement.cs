using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p376:
    /// The MULTIPLY statement multiplies numeric items and sets the values of data
    /// items equal to the results.
    /// </summary>
    public abstract class MultiplyStatement : StatementElement
    {
        public MultiplyStatement(StatementType statementType) : base(CodeElementType.MultiplyStatement, statementType) { }
    }

    /// <summary>
    /// p376: Format 1: MULTIPLY statement
    /// In format 1, the value of identifier-1 or literal-1 is multiplied by the value of
    /// identifier-2; the product is then placed in identifier-2. For each successive occurrence
    /// of identifier-2, the multiplication takes place in the left-to-right order in which
    /// identifier-2 is specified.
    /// </summary>
    public class MultiplySimpleStatement : MultiplyStatement
    {
        public MultiplySimpleStatement() : base(StatementType.MultiplySimpleStatement)
        { }
    }

    /// <summary>
    /// p377: Format 2: MULTIPLY statement with GIVING phrase
    /// In format 2, the value of identifier-1 or literal-1 is multiplied by the value of
    /// identifier-2 or literal-2. The product is then stored in the data items referenced by
    /// identifier-3.
    /// </summary>
    public class MultiplyGivingStatement : MultiplyStatement
    {
        public MultiplyGivingStatement() : base(StatementType.MultiplyGivingStatement)
        { }
    }
}
