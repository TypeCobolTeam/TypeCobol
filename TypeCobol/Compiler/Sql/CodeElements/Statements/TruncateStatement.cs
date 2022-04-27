using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL Truncate Statement Code Element.
    /// </summary>
    public class TruncateStatement : SqlStatementElement
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
        public TableViewCorrelationName TableName { get; }
        public SyntaxProperty<StorageManagementOption> StorageManagement { get; }
        public SyntaxProperty<DeleteTriggersHandlingOption> DeleteTriggersHandling { get; }
        public SyntaxProperty<bool> IsImmediate { get; }

        public TruncateStatement(TableViewCorrelationName tableName, SyntaxProperty<StorageManagementOption> storageManagement, SyntaxProperty<DeleteTriggersHandlingOption> deleteTriggersHandling, SyntaxProperty<bool> isImmediate) : base(CodeElementType.TruncateStatement, StatementType.TruncateStatement)
        {
            this.TableName = tableName;
            this.StorageManagement = storageManagement;
            this.DeleteTriggersHandling = deleteTriggersHandling;
            IsImmediate = isImmediate;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(TableName);
        }
    }
}
