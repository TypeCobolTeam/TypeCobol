using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{

    public enum LockMode
    {
        Shared,
        Exclusive
    }

    public class LockTableStatement : SqlStatementElement
    {
        public TableViewCorrelationName Table { get; }

        [CanBeNull] public SqlConstant PartitionId { get; }

        public SyntaxProperty<LockMode> Mode { get; }

        public LockTableStatement(TableViewCorrelationName table, SqlConstant partitionId,
            SyntaxProperty<LockMode> mode) : base(CodeElementType.LockTableStatement, StatementType.LockTableStatement)
        {
            Table = table;
            PartitionId = partitionId;
            Mode = mode;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, Mode)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(PartitionId, Table);
        }
    }
}