using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL SELECT Statement Code Element.
    /// </summary>
    public class SelectStatement : SqlStatementElement
    {
        public FullSelect FullSelect { get; }

        public SelectStatement(FullSelect fullSelect) : base(CodeElementType.SelectStatement, StatementType.SelectStatement)
        {
            this.FullSelect = fullSelect;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null && FullSelect != null &&
                                                     FullSelect.AcceptVisitor(astVisitor.SqlVisitor);
        }
    }
} 
