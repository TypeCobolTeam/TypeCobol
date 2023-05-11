namespace TypeCobol.Compiler.CodeElements {

    using System.Collections.Generic;

/// <summary>
/// p438:
/// The SUBTRACT statement subtracts one numeric item, or the sum of two or more
/// numeric items, from one or more numeric items, and stores the result.
/// </summary>
public abstract class SubtractStatement: AbstractArithmeticStatement {
    protected SubtractStatement(StatementType statementType): base(CodeElementType.SubtractStatement, statementType) { }
    public abstract override Dictionary<StorageArea,List<ArithmeticExpression>> Affectations { get; }
}

/// <summary>
/// p438: SUBTRACT statement
/// The SUBTRACT statement subtracts one numeric item, or the sum of two or more
/// numeric items, from one or more numeric items, and stores the result.
/// </summary>
public class SubtractSimpleStatement: SubtractStatement {
    public SubtractSimpleStatement(): base(StatementType.SubtractSimpleStatement) { }

    public NumericVariable[] VariablesTogether { get; set; }
    public RoundedResult[] SendingAndReceivingStorageAreas { get; set; }

    public override Dictionary<StorageArea, List<ArithmeticExpression>> Affectations {
        get {
            var map = new Dictionary<StorageArea, List<ArithmeticExpression>>();
            ArithmeticExpression left = null;
            foreach (NumericVariable varTogether in VariablesTogether) {
                var right = new NumericVariableOperand(varTogether);
                if (left == null) left = right;
                else left = ArithmeticOperator.Plus.CreateOperation(left, right);
            }
            foreach(var receiver in SendingAndReceivingStorageAreas) {
                var rarea = receiver.ReceivingStorageArea.StorageArea;
                if (rarea != null && !map.ContainsKey(rarea)) map[rarea] = new List<ArithmeticExpression>();
                var right = new NumericVariableOperand(new NumericVariable(rarea));
                var operation = ArithmeticOperator.Minus.CreateOperation(left, right);
                if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
                if (rarea != null) map[rarea].Add(operation);
            }
            return map;
        }
    }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, VariablesTogether, SendingAndReceivingStorageAreas);
        }
    }

/// <summary>
/// p439: Format 2: SUBTRACT statement with GIVING phrase
/// All identifiers or literals preceding the keyword FROM are added together and
/// their sum is subtracted from identifier-2 or literal-2. The result of the subtraction is
/// stored as the new value of each data item referenced by identifier-3.
/// </summary>
public class SubtractGivingStatement: SubtractStatement {
    public SubtractGivingStatement(): base(StatementType.SubtractGivingStatement) { }

    public NumericVariable[] VariablesTogether { get; set; }
    public NumericVariable Operand { get; set; }
    public RoundedResult[] ReceivingStorageAreas { get; set; }

    public override Dictionary<StorageArea, List<ArithmeticExpression>> Affectations {
        get {
            var map = new Dictionary<StorageArea, List<ArithmeticExpression>>();
            ArithmeticExpression left = null;
            foreach (NumericVariable varTogether in VariablesTogether) {
                var right = new NumericVariableOperand(varTogether);
                if (left == null) left = right;
                else left = ArithmeticOperator.Minus.CreateOperation(left, right);
            }
            foreach(var receiver in ReceivingStorageAreas) {
                var rarea = receiver.ReceivingStorageArea.StorageArea;
            
                if (rarea != null && !map.ContainsKey(rarea)) map[rarea] = new List<ArithmeticExpression>();
                var operation = left;
                if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
                if (rarea != null) map[rarea].Add(operation);
            }
            return map;
        }
    }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, Operand)
                   && this.ContinueVisitToChildren(astVisitor, VariablesTogether, ReceivingStorageAreas);
        }
    }

/// <summary>
/// p439: Format 3: SUBTRACT statement with CORRESPONDING phrase
/// Elementary data items within identifier-1 are subtracted from, and the results are
/// stored in, the corresponding elementary data items within identifier-2.
/// </summary>
public class SubtractCorrespondingStatement: SubtractStatement {
    public SubtractCorrespondingStatement(): base(StatementType.SubtractCorrespondingStatement) { }

    public StorageArea GroupItem { get; set; }
    public StorageArea SendingAndReceivingGroupItem { get; set; }
    public SyntaxProperty<bool> Rounded { get; set; }
    public bool IsRounded { get { return Rounded != null && Rounded.Value; } }

    public override Dictionary<StorageArea,List<ArithmeticExpression>> Affectations {
        get {
            var map = new Dictionary<StorageArea, List<ArithmeticExpression>>();
                if (SendingAndReceivingGroupItem != null)
                {
                    map[SendingAndReceivingGroupItem] = new List<ArithmeticExpression>();
                    var operation = new ArithmeticOperation(
                            new NumericVariableOperand(new NumericVariable(GroupItem)),
                            new SyntaxProperty<ArithmeticOperator>(ArithmeticOperator.Minus, null),
                            new NumericVariableOperand(new NumericVariable(SendingAndReceivingGroupItem))
                        );
                    if (IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
                    map[SendingAndReceivingGroupItem].Add(operation);
                }
            return map;
        }
    }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, GroupItem, SendingAndReceivingGroupItem, Rounded);
        }
    }

}
