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
        public abstract VariableType Type { get; }

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

        public override VariableType Type => VariableType.HostVariable;

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(IndicatorReference), IndicatorReference, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}
