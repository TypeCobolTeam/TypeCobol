using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p394:
    /// For sequential access, the READ statement makes the next logical record from a file
    /// available to the object program. For random access, the READ statement makes a
    /// specified record from a direct-access file available to the object program.
    /// When the READ statement is executed, the associated file must be open in INPUT
    /// or I-O mode.
    /// </summary>
    public class ReadStatement : CodeElement
    {
        /// <summary>
        /// p394:
        /// Must be defined in a DATA DIVISION FD entry.
        /// </summary>
        FileName FileName;
        /// <summary>
        /// p395:
        /// The KEY IS phrase can be specified only for indexed files. data-name-1 must
        /// identify a record key associated with file-name-1. data-name-1 can be qualified; it
        /// cannot be subscripted.
        /// </summary>
        QualifiedName Key;
        /// <summary>
        /// p395:
        /// identifier-1 is the receiving field.
        /// identifier-1 must be a valid receiving field for the selected sending record
        /// description entry in accordance with the rules of the MOVE statement.
        /// The record areas associated with file-name-1 and identifier-1 must not be the
        /// same storage area.
        ///
        /// When there is only one record description associated with file-name-1 or all
        /// the records and the data item referenced by identifier-1 describe an
        /// elementary alphanumeric item or an alphanumeric group item, the result
        /// of the execution of a READ statement with the INTO phrase is equivalent
        /// to the application of the following rules in the order specified:
        /// * The execution of the same READ statement without the INTO phrase.
        /// * The current record is moved from the record area to the area specified
        /// by identifier-1 according to the rules for the MOVE statement without the
        /// CORRESPONDING phrase. The size of the current record is determined
        /// by rules specified for the RECORD clause. If the file description entry
        /// contains a RECORD IS VARYING clause, the implied move is a group
        /// move. The implied MOVE statement does not occur if the execution of
        /// the READ statement was unsuccessful. Any subscripting or reference
        /// modification associated with identifier-1 is evaluated after the record has
        /// been read and immediately before it is moved to the data item. The
        /// record is available in both the record area and the data item referenced
        /// by identifier-1.
        /// </summary>
        Identifier Into;
        /// <summary>
        /// p394:
        /// NEXT RECORD
        /// Reads the next record in the logical sequence of records. NEXT is optional
        /// when the access mode is sequential, and has no effect on READ statement
        /// execution.
        /// You must specify the NEXT RECORD phrase to retrieve records
        /// sequentially from files in dynamic access mode.
        /// </summary>
        bool IsNext;
        /// <summary>
        /// p394:
        /// NEXT RECORD
        /// Reads the next record in the logical sequence of records. NEXT is optional
        /// when the access mode is sequential, and has no effect on READ statement
        /// execution.
        /// You must specify the NEXT RECORD phrase to retrieve records
        /// sequentially from files in dynamic access mode.
        /// </summary>
        bool IsRecord;

        public ReadStatement(FileName filename, Identifier into, QualifiedName key, bool next, bool record)
            : base(CodeElementType.ReadStatement)
        {
            this.FileName = filename;
            this.Into = into;
            this.Key = key;
            this.IsNext = next;
            this.IsRecord = record;
        }
    }
}
