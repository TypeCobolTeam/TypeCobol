using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public enum VariableType
    {
        HostVariable
    }

    public abstract class SqlVariable : SqlExpression
    {
        public override SqlExpressionType ExpressionType => SqlExpressionType.Variable;
        public abstract VariableType VariableType { get; }
        public SymbolReference MainReference { get; }

        protected SqlVariable(SymbolReference mainReference)
        {
            MainReference = mainReference;
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(MainReference), MainReference, indentLevel);
        }
    }

    public class HostVariable : SqlVariable
    {
        public SymbolReference IndicatorReference { get; }

        public HostVariable(SymbolReference mainReference, SymbolReference indicatorReference) : base(mainReference)
        {
            IndicatorReference = indicatorReference;
        }
        public override VariableType VariableType => VariableType.HostVariable;

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            base.DumpContent(output, indentLevel);
            DumpProperty(output, nameof(IndicatorReference), IndicatorReference, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return base.VisitSqlObject(visitor) && visitor.Visit(this);
        }
    }
}
