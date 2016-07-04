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

        public NumericVariable[] VariablesAddedTogether { get; set; }

        public RoundedResult[] SendingAndReceivingStorageAreas { get; set; }    
    }

    public class RoundedResult
    {
        public ReceivingStorageArea ReceivingStorageArea { get; set; }

        public SyntaxProperty<bool> IsRounded { get; set; }
    }

    /// <summary>
    /// p299: Format 2: ADD statement with GIVING phrase
    /// The values of the operands that precede the word GIVING are added together, and the sum is stored as the new value of each data item referenced by identifier-3.
    /// </summary>
    public class AddGivingStatement : AddStatement
    {
        public AddGivingStatement() : base(StatementType.AddGivingStatement)
        { }

        public NumericVariable[] VariablesAddedTogether { get; set; }

        public NumericVariable ToOperand { get; set; }

        public RoundedResult[] ReceivingStorageAreas { get; set; }
    }

    /// <summary>
    /// p299: Format 3: ADD statement with CORRESPONDING phrase
    /// Elementary data items within identifier-1 are added to and stored in the corresponding elementary items within identifier-2.
    /// </summary>
    public class AddCorrespondingStatement : AddStatement
    {
        public AddCorrespondingStatement() : base(StatementType.AddCorrespondingStatement)
        { }

        public StorageArea GroupItem { get; set; }

        public StorageArea ToGroupItem { get; set; }

        public SyntaxProperty<bool> IsRounded { get; set; }
    }
}
