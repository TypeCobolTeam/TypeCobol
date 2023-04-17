
namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// EXIT SECTION statement
    /// The EXIT SECTION statement specifies the end of a SECTION.
    /// </summary>
    public class ExitSectionStatement : StatementElement
    {
        public ExitSectionStatement() : base(CodeElementType.ExitSectionStatement, StatementType.ExitSectionStatement)
        { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
