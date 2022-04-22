using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL Truncate Statement Code Element.
    /// </summary>
    public class TruncateStatement : SqlStatementElement
    {
        public TruncateClause TruncateClause { get; }

        public TruncateStatement(TruncateClause truncateClause) : base(CodeElementType.TruncateStatement, StatementType.TruncateStatement)
        {
            TruncateClause = truncateClause;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TruncateClause);
        }
    }
}
