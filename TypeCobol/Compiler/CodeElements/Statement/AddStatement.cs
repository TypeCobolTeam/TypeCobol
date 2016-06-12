using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p298:
    /// The ADD statement sums two or more numeric operands and stores the result.
    /// </summary>
    public abstract class AddStatement : StatementElement
    {
        public AddStatement(StatementType statementType) : base(CodeElementType.AddStatement, statementType) { }

    }

    /// <summary>
    ///  p298: Format 1: ADD statement
    /// All identifiers or literals that precede the keyword TO are added together, and this sum is added to and stored in identifier-2. 
    /// This process is repeated for each successive occurrence of identifier-2 in the left-to-right order in which identifier-2 is specified.
    /// </summary>
    public class AddSimpleStatement : AddStatement
    {
        public AddSimpleStatement() : base(StatementType.AddSimpleStatement)
        { }

        /// <summary>
        /// p285:
        /// When an arithmetic statement has multiple results, execution conceptually
        /// proceeds as follows:
        /// 1. The statement performs all arithmetic operations to find the result to be placed
        /// in the receiving items, and stores that result in a temporary location.
        /// </summary>
        public ArithmeticExpression IntermediateResult { get; set; }

        /// <summary>
        /// 2. A sequence of statements transfers or combines the value of this temporary
        /// result with each single receiving field. The statements are considered to be
        /// written in the same left-to-right order in which the multiple results are listed.
        /// </summary>
        public Tuple<ReceivingStorageArea, ArithmeticExpression>[] Affectations { get; set; }    
    }

    /// <summary>
    /// p299: Format 2: ADD statement with GIVING phrase
    /// The values of the operands that precede the word GIVING are added together, and the sum is stored as the new value of each data item referenced by identifier-3.
    /// </summary>
    public class AddGivingStatement : AddStatement
    {
        public AddGivingStatement() : base(StatementType.AddGivingStatement)
        { }
    }

    /// <summary>
    /// p299: Format 3: ADD statement with CORRESPONDING phrase
    /// Elementary data items within identifier-1 are added to and stored in the corresponding elementary items within identifier-2.
    /// </summary>
    public class AddCorrespondingStatement : AddStatement
    {
        public AddCorrespondingStatement() : base(StatementType.AddCorrespondingStatement)
        { }
    }
}
