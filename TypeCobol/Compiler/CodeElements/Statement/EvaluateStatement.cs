using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
	/// <summary>
	/// p331: EVALUATE statement
	/// The EVALUATE statement provides a shorthand notation for a series of nested IF statements.
	/// The EVALUATE statement can evaluate multiple conditions.
	/// The subsequent action depends on the results of these evaluations.
	/// </summary>
	public class EvaluateStatement : StatementElement {
		public EvaluateStatement() : base(CodeElementType.EvaluateStatement, StatementType.EvaluateStatement) { }

		/// <summary>
		/// Operands before the WHEN phrase
		/// Are interpreted in one of two ways, depending on how they are specified:
		/// - Individually, they are called selection subjects.
		/// - Collectively, they are called a set of selection subjects.
		/// </summary>
		public SelectionSubject[] SelectionSubjects { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) SelectionSubjects);
        }
    }

	public class SelectionSubject : IVisitable {
		public VariableOrExpression AlphanumericComparisonVariable { get; set; }
		public BooleanValueOrExpression BooleanComparisonVariable { get; set; }
	    public bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return this.ContinueVisitToChildren(astVisitor, AlphanumericComparisonVariable, BooleanComparisonVariable);
        }
	}

    /// <summary>
	/// Default conditional expression case for the EVALUATE statement.
	/// </summary>
	public class WhenOtherCondition : StatementElement {
		public WhenOtherCondition() : base(CodeElementType.WhenOtherCondition, StatementType.WhenOtherCondition) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
