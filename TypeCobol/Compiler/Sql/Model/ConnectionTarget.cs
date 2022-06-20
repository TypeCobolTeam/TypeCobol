using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public class ConnectionTarget : SqlObject
    {
        public ConnectionTarget(SyntaxProperty<string> locationNameLiteral, HostVariable locationNameVariable)
        {
            LocationNameLiteral = locationNameLiteral;
            LocationNameVariable = locationNameVariable;
        }

        public SyntaxProperty<string> LocationNameLiteral { get; }
        public HostVariable LocationNameVariable { get; }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(LocationNameLiteral), LocationNameLiteral, indentLevel);
            DumpProperty(output, nameof(LocationNameVariable), LocationNameVariable, indentLevel);
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(LocationNameVariable);
        }
    }

    public class ConnectionAuthorization : SqlObject
    {
        public ConnectionAuthorization(HostVariable userName, HostVariable password)
        {
            UserName = userName;
            Password = password;
        }

        public HostVariable UserName { get; }
        public HostVariable Password { get; }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(UserName), UserName, indentLevel);
            DumpProperty(output, nameof(Password), Password, indentLevel );
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this) && visitor.ContinueVisit(UserName,Password);
        }
    }
}
