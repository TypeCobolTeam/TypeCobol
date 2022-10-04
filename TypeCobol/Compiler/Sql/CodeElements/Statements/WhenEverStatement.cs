using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public enum ExceptionConditionType
    {
        NotFound,
        SqlError,
        SqlWarning
    }
    public enum BranchingType
    {
        Continue,
        Goto
    }

    public class WhenEverStatement : SqlStatementElement
    {
        public SymbolReference TargetSectionOrParagraph { get; }
        public SyntaxProperty<ExceptionConditionType> ExceptionCondition { get; }
        public SyntaxProperty<BranchingType> BranchingType { get; }

        public WhenEverStatement(SyntaxProperty<ExceptionConditionType> exceptionCondition, SyntaxProperty<BranchingType> branchingType, SymbolReference targetSectionOrParagraph) : base(CodeElementType.WhenEverStatement, StatementType.WhenEverStatement)
        {
            ExceptionCondition = exceptionCondition;
            BranchingType = branchingType;
            TargetSectionOrParagraph = targetSectionOrParagraph;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, ExceptionCondition,
                                                         BranchingType, TargetSectionOrParagraph);
        }
    }
}
