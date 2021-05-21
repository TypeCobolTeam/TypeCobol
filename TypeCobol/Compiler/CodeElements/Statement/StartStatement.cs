using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p429:
    /// The START statement provides a means of positioning within an indexed or
    /// relative file for subsequent sequential record retrieval.
    ///
    /// When the START statement is executed, the associated indexed or relative file must
    /// be open in either INPUT or I-O mode.
    /// </summary>
    public class StartStatement : StatementElement
    {
        public StartStatement() : base(CodeElementType.StartStatement, StatementType.StartStatement)
        { }

        /// <summary>
        /// p429:
        /// file-name-1
        /// Must name a file with sequential or dynamic access. file-name-1 must be
        /// defined in an FD entry in the DATA DIVISION and must not name a sort
        /// file.
        /// </summary>
        public SymbolReference FileName { get; set; }

        /// <summary>
        /// When the START statement is executed, a comparison is made between the current
        /// value in the key data-name and the corresponding key field in the file's index.
        /// </summary>
        public RelationalOperator RelationalOperator { get; set; }

        /// <summary>
        /// p429:
        /// KEY phrase
        /// When the KEY phrase is specified, the file position indicator is positioned at the
        /// logical record in the file whose key field satisfies the comparison.
        /// When the KEY phrase is not specified, KEY IS EQUAL (to the prime record key) is
        /// implied.
        ///
        /// data-name-1
        /// Can be qualified; it cannot be subscripted.
        /// 
        /// If the FILE STATUS clause is specified in the file-control entry, the associated file
        /// status key is updated when the START statement is executed (See “File status key”
        /// on page 287).
        /// </summary>
        public Variable KeyValue { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, FileName, RelationalOperator, KeyValue);
        }
    }
}