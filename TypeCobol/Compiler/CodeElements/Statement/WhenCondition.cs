using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Conditional expression case for the EVALUATE and SEARCH statements.
    /// </summary>
    public class WhenCondition : StatementElement
    {
        public WhenCondition() : base(CodeElementType.WhenCondition, StatementType.WhenCondition)
        {
        }

        /// <summary>
        /// Operands in the WHEN phrase
        /// Are interpreted in one of two ways, depending on how they are specified:
        /// - Individually, they are called selection objects
        /// - Collectively, they are called a set of selection objects.
        /// </summary>
        public SelectionObject[] SelectionObjects { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)SelectionObjects);
        }
    }

    public class SelectionObject : IVisitable
    {
        public SyntaxProperty<bool> IsAny { get; set; }

        public BooleanValueOrExpression BooleanComparisonVariable { get; set; }

        public SyntaxProperty<bool> InvertAlphanumericComparison { get; set; }

        public SyntaxProperty<bool> IsAlphanumericExpressionRange { get; set; }

        public VariableOrExpression AlphanumericComparisonVariable { get; set; }

        public VariableOrExpression AlphanumericComparisonVariable2 { get; set; }

        public bool AcceptASTVisitor(IASTVisitor astVisitor)
        {
            return this.ContinueVisitToChildren(astVisitor, IsAny, BooleanComparisonVariable,
                InvertAlphanumericComparison, IsAlphanumericExpressionRange, AlphanumericComparisonVariable,
                AlphanumericComparisonVariable2);
        }
    }
}
