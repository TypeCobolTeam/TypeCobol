using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class Assignment : SqlObject
    {
        public Assignment(IList<TargetVariable> targets, IList<SourceValue> values)
        {
            Targets = targets;
            Values = values;
        }

        public IList<TargetVariable> Targets { get; }
        public IList<SourceValue> Values { get; }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Targets), Targets, indentLevel);
            DumpProperty(output, nameof(Values), Values, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(Targets, Values);
        }
    }

    public class TargetVariable : SqlObject
    {
        public SqlVariable SqlVariable { get; }

        public TargetVariable(SqlVariable sqlVariable)
        {
            SqlVariable = sqlVariable;
        }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(SqlVariable), SqlVariable, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(SqlVariable);
        }
    }

    public class SourceValue : SqlObject
    {
        public SqlExpression Expression { get; }
        public SyntaxProperty<bool> IsNull { get; }
        public SyntaxProperty<bool> IsDefault { get; }

        public SourceValue(SqlExpression expression, SyntaxProperty<bool> isNull, SyntaxProperty<bool> isDefault)
        {
            Expression = expression;
            IsNull = isNull;
            IsDefault = isDefault;
        }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Expression), Expression, indentLevel);
            DumpProperty(output, nameof(IsNull), IsNull, indentLevel);
            DumpProperty(output, nameof(IsDefault), IsDefault, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(Expression);
        }
    }
}