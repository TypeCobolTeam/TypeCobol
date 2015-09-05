using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p401:
    /// The RELEASE statement transfers records from an input/output area to the initial
    /// phase of a sorting operation.
    ///
    /// The RELEASE statement can be used only within the range of an INPUT
    /// PROCEDURE associated with a SORT statement.
    ///
    /// Within an INPUT PROCEDURE, at least one RELEASE statement must be
    /// specified.
    /// </summary>
    public class ReleaseStatement : CodeElement
    {
        /// <summary>
        /// p401:
        /// When the RELEASE statement is executed, the current contents of record-name-1 are
        /// placed in the sort file. This makes the record available to the initial phase of the
        /// sorting operation.
        ///
        /// record-name-1
        /// Must specify the name of a logical record in a sort-merge file description
        /// entry (SD). record-name-1 can be qualified.
        /// </summary>
        public QualifiedDataName RecordName;

        /// <summary>
        /// p401:
        /// FROM phrase
        /// The result of the execution of the RELEASE statement with the FROM
        /// identifier-1 phrase is equivalent to the execution of the following statements
        /// in the order specified.
        /// MOVE identifier-1 to record-name-1.
        /// RELEASE record-name-1.
        /// The MOVE is performed according to the rules for the MOVE statement
        /// without the CORRESPONDING phrase.
        ///
        /// identifier-1
        /// identifier-1 must reference one of the following items:
        /// * An entry in the WORKING-STORAGE SECTION, the LOCAL-STORAGE
        /// SECTION, or the LINKAGE SECTION
        /// * A record description for another previously opened file
        /// * An alphanumeric or national function.
        ///
        /// identifier-1 must be a valid sending item with record-name-1 as the receiving
        /// item in accordance with the rules of the MOVE statement.
        ///
        /// identifier-1 and record-name-1 must not refer to the same storage area.
        ///
        /// After the RELEASE statement is executed, the information is still available
        /// in identifier-1. (See “INTO and FROM phrases” on page 291 under
        /// "Common processing facilities".)
        /// </summary>
        public Identifier From;

        public ReleaseStatement() : base(CodeElementType.ReleaseStatement) { }
    }
}
