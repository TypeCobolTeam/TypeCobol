using System.IO;

namespace TypeCobol.Compiler.Sql.Model
{
    public class StringExpression : SqlObject
    {
        //TODO so far only constants are supported.
        public SqlConstant Literal { get; }

        public StringExpression(SqlConstant literal)
        {
            Literal = literal;
        }

        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(Literal), Literal, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(Literal);
        }
    }
}
