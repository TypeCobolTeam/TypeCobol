using System;
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
		public EvaluateSelectionSubject[] SelectionSubjects { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) SelectionSubjects);
        }
    }

	public class EvaluateSelectionSubject : IVisitable {
		public VariableOrExpression AlphanumericComparisonVariable { get; set; }
		public BooleanValueOrExpression BooleanComparisonVariable { get; set; }
	    public bool AcceptASTVisitor(IASTVisitor astVisitor) {
	        return this.ContinueVisitToChildren(astVisitor, AlphanumericComparisonVariable, BooleanComparisonVariable);
        }
	}

	/// <summary>
	/// Conditional expression case for the EVALUATE statement.
	/// </summary>
	public class WhenCondition : StatementElement {
		public WhenCondition() : base(CodeElementType.WhenCondition, StatementType.WhenCondition) { }

		/// <summary>
		/// Operands in the WHEN phrase
		/// Are interpreted in one of two ways, depending on how they are specified:
		/// - Individually, they are called selection objects
		/// - Collectively, they are called a set of selection objects.
		/// </summary>
		public EvaluateSelectionObject[] SelectionObjects { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>) SelectionObjects);
        }
    }

	public class EvaluateSelectionObject : IVisitable  {

		public SyntaxProperty<bool> IsAny { get; set; }

		public BooleanValueOrExpression BooleanComparisonVariable { get; set; }

		public SyntaxProperty<bool> InvertAlphanumericComparison { get; set; }

		public SyntaxProperty<bool> IsAlphanumericExpressionRange { get; set; }

		public VariableOrExpression AlphanumericComparisonVariable { get; set; }

		public VariableOrExpression AlphanumericComparisonVariable2 { get; set; }

        public bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return this.ContinueVisitToChildren(astVisitor, IsAny, BooleanComparisonVariable,
                InvertAlphanumericComparison, IsAlphanumericExpressionRange, AlphanumericComparisonVariable,
                   AlphanumericComparisonVariable2);
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
