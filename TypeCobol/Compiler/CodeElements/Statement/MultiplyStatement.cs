namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;

/// <summary>p376: The MULTIPLY statement multiplies numeric items and sets the values of data items equal to the results.</summary>
public abstract class MultiplyStatement: AbstractArithmeticStatement {
    protected MultiplyStatement(StatementType statementType): base(CodeElementType.MultiplyStatement, statementType) { }

	public NumericVariable Operand { get; set; }
	public abstract override Dictionary<string,List<ArithmeticExpression>> Affectations { get; }
}

/// <summary>
/// p376: Format 1: MULTIPLY statement
/// In format 1, the value of identifier-1 or literal-1 is multiplied by the value of
/// identifier-2; the product is then placed in identifier-2. For each successive occurrence
/// of identifier-2, the multiplication takes place in the left-to-right order in which
/// identifier-2 is specified.
/// </summary>
public class MultiplySimpleStatement: MultiplyStatement {
	public MultiplySimpleStatement(): base(StatementType.MultiplySimpleStatement) { }

	public RoundedResult[] SendingAndReceivingStorageAreas { get; set; }

	public override Dictionary<string,List<ArithmeticExpression>> Affectations {
		get {
			var map = new Dictionary<string,List<ArithmeticExpression>>();
			ArithmeticExpression left = new NumericVariableOperand(Operand);
			foreach(var receiver in SendingAndReceivingStorageAreas) {
				var rarea = receiver.ReceivingStorageArea.StorageArea;
				string key = rarea.ToString();
				if (!map.ContainsKey(key)) map[key] = new List<ArithmeticExpression>();
				var right = new NumericVariableOperand(new NumericVariable(rarea));
				var operation = ArithmeticOperator.Multiply.CreateOperation(left, right);
				if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
				map[key].Add(operation);
			}
			return map;
		}
	}
}

/// <summary>
/// p377: Format 2: MULTIPLY statement with GIVING phrase
/// In format 2, the value of identifier-1 or literal-1 is multiplied by the value of
/// identifier-2 or literal-2. The product is then stored in the data items referenced by
/// identifier-3.
/// </summary>
public class MultiplyGivingStatement: MultiplyStatement {
	public MultiplyGivingStatement(): base(StatementType.MultiplyGivingStatement) { }

	public NumericVariable ByOperand { get; set; }
	public RoundedResult[] ReceivingStorageAreas { get; set; }

	public override Dictionary<string,List<ArithmeticExpression>> Affectations {
		get {
			var map = new Dictionary<string,List<ArithmeticExpression>>();
			ArithmeticExpression left  = new NumericVariableOperand(Operand);
			ArithmeticExpression right = new NumericVariableOperand(ByOperand);
			left = ArithmeticOperator.Multiply.CreateOperation(left, right);
			foreach(var receiver in ReceivingStorageAreas) {
				var rarea = receiver.ReceivingStorageArea.StorageArea;
				string key = rarea.ToString();
				if (!map.ContainsKey(key)) map[key] = new List<ArithmeticExpression>();
				right = new NumericVariableOperand(new NumericVariable(rarea));
				var operation = left;
				if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
				map[key].Add(operation);
			}
			return map;
		}
	}
}

}
