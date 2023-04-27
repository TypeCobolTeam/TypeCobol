
namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// EXIT PARAGRAPH statement
    /// The EXIT PARAGRAPH statement specifies the end of an invoked PARAGRAPH.
    /// </summary>
    public class ExitParagraphStatement : StatementElement
    {
        public ExitParagraphStatement() : base(CodeElementType.ExitParagraphStatement, StatementType.ExitParagraphStatement)
        { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this);
        }
    }
}
