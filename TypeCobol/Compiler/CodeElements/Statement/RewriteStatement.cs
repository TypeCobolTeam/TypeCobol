using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p405:
    /// The REWRITE statement logically replaces an existing record in a direct-access file.
    /// When the REWRITE statement is executed, the associated direct-access file must be
    /// open in I-O mode.
    ///
    /// The REWRITE statement is not supported for line-sequential files.
    /// </summary>
    public class RewriteStatement : StatementElement
    {
        public RewriteStatement() : base(CodeElementType.RewriteStatement, StatementType.RewriteStatement)
        { }

        /// <summary>
        /// p405:
        /// Must be the name of a logical record in a DATA DIVISION FD entry. The
        /// record-name can be qualified.
        /// </summary>
        public SymbolReference RecordName { get; set; }

        /// <summary>
        /// p405:
        /// FROM phrase
        /// The result of the execution of the REWRITE statement with the FROM
        /// identifier-1 phrase is equivalent to the execution of the following statements
        /// in the order specified.
        /// MOVE identifier-1 TO record-name-1.
        /// REWRITE record-name-1
        /// The MOVE is performed according to the rules for the MOVE statement
        /// without the CORRESPONDING phrase.
        ///
        /// identifier-1 can reference one of the following items:
        /// * A record description for another previously opened file
        /// * An alphanumeric or national function
        /// * A data item defined in the WORKING-STORAGE SECTION, the
        /// LOCAL-STORAGE SECTION, or the LINKAGE SECTION
        /// identifier-1 must be a valid sending item with record-name-1 as the receiving
        /// item in accordance with the rules of the MOVE statement.
        /// identifier-1 and record-name-1 must not refer to the same storage area.
        /// After the REWRITE statement is executed, the information is still available
        /// in identifier-1 (“INTO and FROM phrases” on page 291 under "Common
        /// processing facilities").
        /// </summary>
        public Variable FromVariable { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, RecordName, FromVariable);
        }
    }
}
