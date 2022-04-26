using System.IO;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Sql.Model
{
    public enum StorageManagementOption
    {
        DropStorage,
        ReuseStorage
    }

    public enum DeleteTriggersHandlingOption
    {
        IgnoreDeleteTriggers,
        RestrictWhenDeleteTriggers
    }

    public class StorageManagementClause : SqlObject
    {
        public StorageManagementClause(SyntaxProperty<StorageManagementOption> storageManagement)
        {
            StorageManagement = storageManagement;
        }

        public SyntaxProperty<StorageManagementOption> StorageManagement { get; }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(StorageManagement), StorageManagement, indentLevel);
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }

    public class DeleteTriggersHandlingClause : SqlObject
    {
        public DeleteTriggersHandlingClause(SyntaxProperty<DeleteTriggersHandlingOption> deleteTriggersHandling)
        {
            DeleteTriggersHandling = deleteTriggersHandling;
        }

        public SyntaxProperty<DeleteTriggersHandlingOption> DeleteTriggersHandling { get; }
        protected override void DumpContent(TextWriter output, int indentLevel)
        {
            DumpProperty(output, nameof(DeleteTriggersHandling), DeleteTriggersHandling, indentLevel);
        }
        protected override bool VisitSqlObject(ISqlVisitor visitor)
        {
            return visitor.Visit(this);
        }
    }
}