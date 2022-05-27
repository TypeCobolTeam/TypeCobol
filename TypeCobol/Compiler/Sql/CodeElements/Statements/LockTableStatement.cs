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

        public SyntaxProperty<bool> IsPartitionInteger { get; }

        public LockTableStatement(TableViewCorrelationName table, SqlConstant partitionId,
            SyntaxProperty<LockMode> mode, SyntaxProperty<bool> isPartitionInteger) : base(
            CodeElementType.LockTableStatement, StatementType.LockTableStatement)
        {
            Table = table;
            PartitionId = partitionId;
            Mode = mode;
            IsPartitionInteger = isPartitionInteger;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, Mode,
                                                         IsPartitionInteger)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(PartitionId,Table);
        }
    }
}