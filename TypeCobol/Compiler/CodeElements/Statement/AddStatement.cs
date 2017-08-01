namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;

/// <summary>p298: The ADD statement sums two or more numeric operands and stores the result.</summary>
public abstract class AddStatement: AbstractArithmeticStatement {
    protected AddStatement(StatementType statementType) : base(CodeElementType.AddStatement, statementType) { }
	public abstract override Dictionary<string,List<ArithmeticExpression>> Affectations { get; }
}

/// <summary>
///  p298: Format 1: ADD statement
/// All identifiers or literals that precede the keyword TO are added together, and this sum is added to and stored in identifier-2.
/// This process is repeated for each successive occurrence of identifier-2 in the left-to-right order in which identifier-2 is specified.
/// </summary>
public class AddSimpleStatement: AddStatement {
	public AddSimpleStatement(): base(StatementType.AddSimpleStatement) { }

	public NumericVariable[] VariablesTogether { get; set; }
	public RoundedResult[] SendingAndReceivingStorageAreas { get; set; }

	public override Dictionary<string,List<ArithmeticExpression>> Affectations {
		get {
			var map = new Dictionary<string,List<ArithmeticExpression>>();
			ArithmeticExpression left = null;
                if (VariablesTogether != null)
                {
                    foreach (NumericVariable varTogether in VariablesTogether)
                    {
                        var right = new NumericVariableOperand(varTogether);
                        if (left == null) left = right;
                        else left = ArithmeticOperator.Plus.CreateOperation(left, right);
                    }
                }
                if(SendingAndReceivingStorageAreas != null)
                {
                    foreach (var receiver in SendingAndReceivingStorageAreas)
                    {
                        var rarea = receiver.ReceivingStorageArea.StorageArea;
                        string key = rarea.ToString();
                        if (!map.ContainsKey(key)) map[key] = new List<ArithmeticExpression>();
                        var right = new NumericVariableOperand(new NumericVariable(rarea));
                        var operation = ArithmeticOperator.Plus.CreateOperation(left, right);
                        if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
                        map[key].Add(operation);
                    }
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
/// p299: Format 2: ADD statement with GIVING phrase
/// The values of the operands that precede the word GIVING are added together,
/// and the sum is stored as the new value of each data item referenced by identifier-3.
/// </summary>
public class AddGivingStatement: AddStatement {
	public AddGivingStatement(): base(StatementType.AddGivingStatement) { }

	public NumericVariable[] VariablesTogether { get; set; }
	public NumericVariable Operand { get; set; }
	public RoundedResult[] ReceivingStorageAreas { get; set; }

	public override Dictionary<string,List<ArithmeticExpression>> Affectations {
		get {
			var map = new Dictionary<string,List<ArithmeticExpression>>();
			ArithmeticExpression left = null;
			foreach (NumericVariable varTogether in VariablesTogether) {
			    var right = new NumericVariableOperand(varTogether);
			    if (left == null) left = right;
			    else left = ArithmeticOperator.Plus.CreateOperation(left, right);
			}
			foreach(var receiver in ReceivingStorageAreas) {
				var rarea = receiver.ReceivingStorageArea.StorageArea;
				string key = rarea.ToString();
				if (!map.ContainsKey(key)) map[key] = new List<ArithmeticExpression>();
				var operation = left;
				if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
				map[key].Add(operation);
			}
			return map;
		}
	}

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, VariablesTogether, ReceivingStorageAreas)
                   && this.ContinueVisitToChildren(astVisitor, Operand);
        }
    }

/// <summary>
/// p299: Format 3: ADD statement with CORRESPONDING phrase
/// Elementary data items within identifier-1 are added to and stored in the corresponding elementary items within identifier-2.
/// </summary>
public class AddCorrespondingStatement: AddStatement {
	public AddCorrespondingStatement(): base(StatementType.AddCorrespondingStatement) { }

	public StorageArea GroupItem { get; set; }
	public StorageArea SendingAndReceivingGroupItem { get; set; }
	public SyntaxProperty<bool> Rounded { get; set; }
	public bool IsRounded { get { return Rounded != null && Rounded.Value; } }

	public override Dictionary<string,List<ArithmeticExpression>> Affectations {
		get {
			var map = new Dictionary<string,List<ArithmeticExpression>>();
			string key = SendingAndReceivingGroupItem.ToString();
			map[key] = new List<ArithmeticExpression>();
			var operation = new ArithmeticOperation(
					new NumericVariableOperand(new NumericVariable(GroupItem)),
					new SyntaxProperty<ArithmeticOperator>(ArithmeticOperator.Plus, null),
					new NumericVariableOperand(new NumericVariable(SendingAndReceivingGroupItem))
				);
			if (IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
			map[key].Add(operation);
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
