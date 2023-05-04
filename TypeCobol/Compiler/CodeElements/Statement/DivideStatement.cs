namespace TypeCobol.Compiler.CodeElements {
    using System.Collections.Generic;

/// <summary>
/// p325:
/// The DIVIDE statement divides one numeric data item into or by others and sets
/// the values of data items equal to the quotient and remainder.
/// </summary>
public abstract class DivideStatement: AbstractArithmeticStatement {
    protected DivideStatement(StatementType statementType): base(CodeElementType.DivideStatement, statementType) { }
    public abstract override Dictionary<StorageArea,List<ArithmeticExpression>> Affectations { get; }
}

/// <summary>
/// In format 1, the value of identifier-1 or literal-1 is divided into the value of identifier-2, and the quotient is then stored in identifier-2. 
/// For each successive occurrence of identifier-2, the division takes place in the left-to-right order in which identifier-2 is specified.
/// </summary>
public class DivideSimpleStatement: DivideStatement {
    public DivideSimpleStatement(): base(StatementType.DivideSimpleStatement) { }

    public NumericVariable Divisor { get; set; }
    public RoundedResult[] SendingAndReceivingStorageAreas { get; set; }

    public override Dictionary<StorageArea, List<ArithmeticExpression>> Affectations {
        get {
			var map = new Dictionary<StorageArea, List<ArithmeticExpression>>();
			ArithmeticExpression right = new NumericVariableOperand(Divisor);
			foreach(var receiver in SendingAndReceivingStorageAreas) {
                var rarea = receiver.ReceivingStorageArea.StorageArea;
                if (rarea != null && !map.ContainsKey(rarea)) map[rarea] = new List<ArithmeticExpression>();
                var left = new NumericVariableOperand(new NumericVariable(rarea));
                var operation = ArithmeticOperator.Divide.CreateOperation(left, right);
                if (receiver.IsRounded) operation = ArithmeticOperator.Round.CreateOperation(operation);
                if (rarea != null) map[rarea].Add(operation);
            }
			return map;
		}
    }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)SendingAndReceivingStorageAreas)
                   && this.ContinueVisitToChildren(astVisitor, Divisor);
        }
    }

/// <summary>
/// Format 2: DIVIDE statement with INTO and GIVING phrases
/// In format 2, the value of identifier-1 or literal-1 is divided into the value of identifier-2 or literal-2.
/// The value of the quotient is stored in each data item referenced by identifier-3.
///
/// Format 3: DIVIDE statement with BY and GIVING phrase
/// In format 3, the value of identifier-1 or literal-1 is divided by the value of identifier-2 or literal-2.
/// The value of the quotient is stored in each data item referenced by identifier-3.
/// </summary>
public class DivideGivingStatement: DivideStatement {
    public DivideGivingStatement(): base(StatementType.DivideGivingStatement) { }

    public NumericVariable Dividend { get; set; }
    /// <summary>Divisor</summary>
    public NumericVariable Divisor { get; set; }
    /// <summary>Quotients</summary>
    public RoundedResult[] ReceivingStorageAreas { get; set; }

    public override Dictionary<StorageArea, List<ArithmeticExpression>> Affectations {
        get {
			var map = new Dictionary<StorageArea, List<ArithmeticExpression>>();
			ArithmeticExpression left  = new NumericVariableOperand(Dividend);
			ArithmeticExpression right = new NumericVariableOperand(Divisor);
			left = ArithmeticOperator.Divide.CreateOperation(left, right);
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
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)ReceivingStorageAreas)
                   && this.ContinueVisitToChildren(astVisitor, Dividend, Divisor);
        }
    }

/// <summary>
/// Format 4: DIVIDE statement with INTO and REMAINDER phrases
/// In format 4, the value of identifier-1 or literal-1 is divided into identifier-2 or literal-2.
/// The value of the quotient is stored in identifier-3, and the value of the remainder is stored in identifier-4.
///
/// Format 5: DIVIDE statement with BY and REMAINDER phrase
/// In format 5, the value of identifier-1 or literal-1 is divided by identifier-2 or literal-2.
/// The value of the quotient is stored in identifier-3, and the value of the remainder is stored in identifier-4.
/// </summary>
public class DivideRemainderStatement: DivideStatement {
    public DivideRemainderStatement(): base(StatementType.DivideRemainderStatement) { }

    public NumericVariable Divisor { get; set; }
    public NumericVariable Dividend { get; set; }
    public RoundedResult Quotient { get; set; }
    public ReceivingStorageArea Remainder { get; set; }

    public override Dictionary<StorageArea, List<ArithmeticExpression>> Affectations {
        get {
			var map = new Dictionary<StorageArea,List<ArithmeticExpression>>();
			ArithmeticExpression left  = new NumericVariableOperand(Dividend);
			ArithmeticExpression right = new NumericVariableOperand(Divisor);
			ArithmeticExpression operation;

			operation = ArithmeticOperator.Divide.CreateOperation(left, right);
			var rarea = Quotient.ReceivingStorageArea.StorageArea;
		    if (rarea != null)
            {
                map[rarea] = new List<ArithmeticExpression>();
                if (Quotient.IsRounded)
                    map[rarea].Add(ArithmeticOperator.Round.CreateOperation(operation));
                else map[rarea].Add(operation);

                operation = ArithmeticOperator.Remainder.CreateOperation(left, right);
                rarea = Remainder.StorageArea;
            }
               
		    if (rarea != null)
                map[rarea] = new List<ArithmeticExpression> {operation};

		    return map;
		}
    }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, Divisor, Dividend, Quotient, Remainder);
        }
    }

}
