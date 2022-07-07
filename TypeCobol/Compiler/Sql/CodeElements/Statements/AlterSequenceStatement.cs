using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    public class AlterSequenceStatement : SqlStatementElement
    {
        public TableViewCorrelationName SequenceName { get; }
        public SyntaxProperty<bool> Restart { get; }
        public SqlConstant RestartValue { get; }
        public SqlConstant IncrementValue { get; }
        public SqlConstant MinValue { get; }
        public SqlConstant MaxValue { get; }
        public SyntaxProperty<bool> Cycle { get; }
        public SqlConstant CacheSize { get; }
        public bool NoCache => CacheSize == null;
        public SyntaxProperty<bool> Ordered { get; }

        public AlterSequenceStatement(TableViewCorrelationName sequenceName, SyntaxProperty<bool> restart,
            SqlConstant restartValue, SqlConstant incrementValue, SqlConstant minValue, SqlConstant maxValue,
            SyntaxProperty<bool> cycle, SqlConstant cacheSize, SyntaxProperty<bool> ordered) : base(
            CodeElementType.AlterSequenceStatement, StatementType.AlterSequenceStatement)
        {
            SequenceName = sequenceName;
            Restart = restart;
            RestartValue = restartValue;
            IncrementValue = incrementValue;
            MinValue = minValue;
            MaxValue = maxValue;
            Cycle = cycle;
            CacheSize = cacheSize;
            Ordered = ordered;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, Restart,
                                                         Cycle, Ordered)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(SequenceName, RestartValue,
                                                         IncrementValue, MinValue, MaxValue, CacheSize);
        }
    }
}