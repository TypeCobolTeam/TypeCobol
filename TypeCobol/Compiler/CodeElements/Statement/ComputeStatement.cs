namespace TypeCobol.Compiler.CodeElements {

	using System.Collections.Generic;

/// <summary>
/// p317:
/// The COMPUTE statement assigns the value of an arithmetic expression to one or more data items.
///
/// With the COMPUTE statement, arithmetic operations can be combined without the restrictions on
/// receiving data items imposed by the rules for the ADD, SUBTRACT, MULTIPLY, and DIVIDE statements.
///
/// When arithmetic operations are combined, the COMPUTE statement can be more
/// efficient than the separate arithmetic statements written in a series.
/// </summary>
public class ComputeStatement: StatementElement, ArithmeticStatement {
	public ComputeStatement(): base(CodeElementType.ComputeStatement, StatementType.ComputeStatement) { }

	public RoundedResult[] ReceivingStorageAreas { get; set; }
	public ArithmeticExpression ArithmeticExpression { get; set; }

	public Dictionary<string,List<ArithmeticExpression>> Affectations {
		get {
			var map = new Dictionary<string,List<ArithmeticExpression>>();
			foreach(var receiver in ReceivingStorageAreas) {
				var rarea = receiver.ReceivingStorageArea.StorageArea;
				string key = rarea.ToString();
				if (!map.ContainsKey(key)) map[key] = new List<ArithmeticExpression>();
				var operation = ArithmeticExpression;
				if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
				map[key].Add(operation);
			}
			return map;
		}
	}
}



public class RoundedResult {
	public ReceivingStorageArea ReceivingStorageArea { get; set; }
	public SyntaxProperty<bool> Rounded { get; set; }
	public bool IsRounded { get { return Rounded != null && Rounded.Value; } }

	public override string ToString() {
		var str = new System.Text.StringBuilder();
		if (ReceivingStorageArea == null) str.Append('?');
		else str.Append(ReceivingStorageArea.StorageArea);
		if (IsRounded) str.Append(" °");
		return str.ToString();
	}
}

public interface ArithmeticStatement {
	Dictionary<string,List<ArithmeticExpression>> Affectations { get; }
}

}
