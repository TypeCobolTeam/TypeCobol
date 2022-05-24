using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public enum SqlSymbolType
    {
        ColumnName,
        Variable
    }

    public class SqlSymbol : SqlObject
    {
        private readonly SqlStorageArea _name;
        private SqlSymbolType _type;
        public SqlSymbol(SqlStorageArea name , SqlSymbolType type)
        {
            _name=name;
            _type = type;
        }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(_name.SymbolReference), _name.SymbolReference, indentLevel);
        }

        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}
