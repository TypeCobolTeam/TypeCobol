using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p403:
    /// The RETURN statement transfers records from the final phase of a sorting or
    /// merging operation to an OUTPUT PROCEDURE.
    /// The RETURN statement can be used only within the range of an OUTPUT
    /// PROCEDURE associated with a SORT or MERGE statement.
    ///
    /// Within an OUTPUT PROCEDURE, at least one RETURN statement must be specified.
    ///
    /// When the RETURN statement is executed, the next record from file-name-1 is made
    /// available for processing by the OUTPUT PROCEDURE.
    /// </summary>
    public class ReturnStatement : StatementElement
    {
        public ReturnStatement() : base(CodeElementType.ReturnStatement, StatementType.ReturnStatement)
        { }

        /// <summary>
        /// p403:
        /// Must be described in a DATA DIVISION SD entry.
        /// If more than one record description is associated with file-name-1, those
        /// records automatically share the same storage; that is, the area is implicitly
        /// redefined. After RETURN statement execution, only the contents of the
        /// current record are available. If any data items lie beyond the length of the
        /// current record, their contents are undefined.
        /// </summary>
        public SymbolReference FileName { get; set; }

        /// <summary>
        /// When there is only one record description associated with file-name-1 or all
        /// the records and the data item referenced by identifier-1 describe an
        /// elementary alphanumeric item or an alphanumeric group item, the result
        /// of the execution of a RETURN statement with the INTO phrase is
        /// equivalent to the application of the following rules in the order specified:
        /// * The execution of the same RETURN statement without the INTO phrase.
        /// * The current record is moved from the record area to the area specified
        /// by identifier-1 according to the rules for the MOVE statement without the
        /// CORRESPONDING phrase. The size of the current record is determined
        /// by rules specified for the RECORD clause. If the file description entry
        /// contains a RECORD IS VARYING clause, the implied move is a group
        /// move. The implied MOVE statement does not occur if the execution of
        /// the RETURN statement was unsuccessful. Any subscripting or reference
        /// modification associated with identifier-1 is evaluated after the record has
        /// been read and immediately before it is moved to the data item. The
        /// record is available in both the record area and the data item referenced
        /// by identifier-1.
        ///
        /// When there are multiple record descriptions associated with file-name-1 and
        /// they do not all describe an alphanumeric group item or elementary
        /// alphanumeric item, the following rules apply:
        /// 1. If the file referenced by file-name-1 contains variable-length records, a
        /// group move takes place.
        /// 2. If the file referenced by file-name-1 contains fixed-length records, a
        /// move takes place according to the rules for a MOVE statement using,
        /// as a sending field description, the record that specifies the largest
        /// number of character positions. If more than one such record exists, the
        /// sending field record selected will be the one among those records that
        /// appears first under the description of file-name-1.
        /// identifier-1 must be a valid receiving field for the selected sending record
        /// description entry in accordance with the rules of the MOVE statement.
        /// The record areas associated with file-name-1 and identifier-1 must not be the same
        /// storage area.
        /// </summary>
        public ReceivingStorageArea IntoStorageArea { get; set; }
    }
}
