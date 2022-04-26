using System.IO;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Sql.Model;

namespace TypeCobol.Compiler.Sql.CodeElements.Statements
{
    /// <summary>
    /// SQL Truncate Statement Code Element.
    /// </summary>
    public class TruncateStatement : SqlStatementElement
    {
        public StorageArea TableName { get; }
        public StorageManagementClause StorageManagementClause { get; }
        public DeleteTriggersHandlingClause DeleteTriggersHandlingClause { get; }
        public SyntaxProperty<bool> IsImmediate { get; }

        public TruncateStatement(StorageArea tableName, StorageManagementClause storageManagementClause, DeleteTriggersHandlingClause deleteTriggersHandlingClause, SyntaxProperty<bool> isImmediate) : base(CodeElementType.TruncateStatement, StatementType.TruncateStatement)
        {
            this.TableName = tableName;
            this.StorageManagementClause = storageManagementClause;
            this.DeleteTriggersHandlingClause = deleteTriggersHandlingClause;
            IsImmediate = isImmediate;
        }
        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                                                     && astVisitor.SqlVisitor != null
                                                     && astVisitor.SqlVisitor.ContinueVisit(StorageManagementClause)
                                                     && astVisitor.SqlVisitor.ContinueVisit(
                                                         DeleteTriggersHandlingClause);
        }
    }
}
