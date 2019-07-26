using System.Linq;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The FREE statement releases dynamic storage that was previously obtained with
    /// an ALLOCATE statement.
    /// </summary>
    public class FreeStatement : StatementElement
    {
        public FreeStatement()
            : base(CodeElementType.FreeStatement, StatementType.FreeStatement)
        {

        }

        /// <summary>
        /// Target storage areas to be freed.
        /// data-name-1
        /// Must be defined as USAGE IS POINTER.
        /// Can be qualified or subscripted.
        /// The FREE statement is processed as follows:
        /// - If the pointer referenced by data-name-1 identifies the start of storage that is
        /// currently allocated by an ALLOCATE statement, that storage is released and the
        /// pointer referenced by data-name-1 is set to NULL, the length of the released
        /// storage is the length of the storage obtained by the ALLOCATE statement, and
        /// the contents of any data items located within the released storage area become
        /// undefined.
        /// - If the pointer referenced by data-name-1 contains the predefined address NULL
        /// or the address of storage that is not acquired by the ALLOCATE statement, no
        /// storage will be freed. The pointer data-name-1 will be kept unchanged and the
        /// behavior is undefined.
        /// If more than one data-name-1 is specified in a FREE statement, the result of
        /// executing this FREE statement is the same as if a separate FREE statement had
        /// been written for each data-name-1 in the same order as specified in the FREE
        /// statement.
        /// </summary>
        [ItemNotNull]
        public ReceivingStorageArea[] TargetStorageAreas { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor)
                && astVisitor.Visit(this)
                && this.ContinueVisitToChildren(astVisitor, this.TargetStorageAreas.Cast<IVisitable>());
        }
    }
}
