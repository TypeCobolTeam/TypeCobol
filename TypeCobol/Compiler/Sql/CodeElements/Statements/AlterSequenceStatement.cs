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
        public SyntaxProperty<bool> HasMinValue { get; }
        public SqlConstant MinValue { get; }
        public SyntaxProperty<bool> HasMaxValue { get; }
        public SqlConstant MaxValue { get; }
        public SyntaxProperty<bool> Cycle { get; }
        public SyntaxProperty<bool> HasCache { get; }
        public SqlConstant CacheSize { get; }
        public SyntaxProperty<bool> Ordered { get; }

        public AlterSequenceStatement(TableViewCorrelationName sequenceName, SyntaxProperty<bool> restart,
            SqlConstant restartValue, SqlConstant incrementValue, SyntaxProperty<bool> hasMinValue,
            SqlConstant minValue, SyntaxProperty<bool> hasMaxValue, SqlConstant maxValue, SyntaxProperty<bool> cycle,
            SyntaxProperty<bool> hasCache, SqlConstant cacheSize, SyntaxProperty<bool> ordered) : base(
            CodeElementType.AlterSequenceStatement, StatementType.AlterSequenceStatement)
        {
            SequenceName = sequenceName;
            Restart = restart;
            RestartValue = restartValue;
            IncrementValue = incrementValue;
            HasMinValue = hasMinValue;
            MinValue = minValue;
            HasMaxValue = hasMaxValue;
            MaxValue = maxValue;
            Cycle = cycle;
            HasCache = hasCache;
            CacheSize = cacheSize;
            Ordered = ordered;
        }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && this.ContinueVisitToChildren(astVisitor, Restart, HasMinValue,
                                                         HasMaxValue, Cycle, HasCache, Ordered)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(SequenceName, RestartValue,
                                                         IncrementValue, MinValue, MaxValue, CacheSize);
        }
    }
}
