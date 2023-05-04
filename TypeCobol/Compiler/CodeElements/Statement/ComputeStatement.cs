namespace TypeCobol.Compiler.CodeElements {

    using System.Collections.Generic;
    using TypeCobol.Compiler.CodeElements.Expressions;

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
public class ComputeStatement: AbstractArithmeticStatement {
    public ComputeStatement(): base(CodeElementType.ComputeStatement, StatementType.ComputeStatement) { }

    public RoundedResult[] ReceivingStorageAreas { get; set; }
    public ArithmeticExpression ArithmeticExpression { get; set; }

    public override Dictionary<StorageArea,List<ArithmeticExpression>> Affectations {
        get {
			var map = new Dictionary<StorageArea, List<ArithmeticExpression>>();
			foreach(var receiver in ReceivingStorageAreas) {
                var rarea = receiver.ReceivingStorageArea.StorageArea;
                if (rarea != null && !map.ContainsKey(rarea)) map[rarea] = new List<ArithmeticExpression>();
                var operation = ArithmeticExpression;
                if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
                if (rarea != null) map[rarea].Add(operation);
            }
			return map;
		}
    }
        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) ReceivingStorageAreas)
                   && this.ContinueVisitToChildren(astVisitor, ArithmeticExpression);
        }
    }



public class RoundedResult : IVisitable {
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

    public bool AcceptASTVisitor(IASTVisitor astVisitor) {
        return astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, ReceivingStorageArea, Rounded);
    }
}

public interface ArithmeticStatement {
    Dictionary<StorageArea,List<ArithmeticExpression>> Affectations { get; }
}

public abstract class AbstractArithmeticStatement: StatementElement, ArithmeticStatement, VariableWriter {
    protected AbstractArithmeticStatement(CodeElementType ce, StatementType statement): base(ce, statement) { }
    
    public abstract Dictionary<StorageArea, List<ArithmeticExpression>> Affectations { get; }

    public IDictionary<StorageArea, object> Variables { get { return VariablesWritten; } }
    private IDictionary<StorageArea, object> variablesWritten;
    public  IDictionary<StorageArea,object> VariablesWritten {
        get {
			if (variablesWritten != null) return variablesWritten;
			variablesWritten = new Dictionary<StorageArea, object>();

			foreach(var affectation in Affectations)
                variablesWritten.Add(affectation.Key, affectation.Value);
			return variablesWritten;
		}
    }
    public bool IsUnsafe { get { return false; } }
}

}
