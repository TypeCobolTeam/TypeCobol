namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p320:
    /// The DELETE statement removes a record from an indexed or relative file. For
    /// indexed files, the key can then be reused for record addition. For relative files, the
    /// space is then available for a new record with the same RELATIVE KEY value.
    /// When the DELETE statement is executed, the associated file must be open in I-O
    /// mode.
    ///
    /// p320:
    /// After successful execution of a DELETE statement, the record is removed from the
    /// file and can no longer be accessed.
    ///
    /// Execution of the DELETE statement does not affect the contents of the record area
    /// associated with file-name-1 or the content of the data item referenced by the
    /// data-name specified in the DEPENDING ON phrase of the RECORD clause
    /// associated with file-name-1.
    ///
    /// If the FILE STATUS clause is specified in the file-control entry, the associated file
    /// status key is updated when the DELETE statement is executed.
    ///
    /// The file position indicator is not affected by execution of the DELETE statement.
    /// </summary>
    public class DeleteStatement : CodeElement
    {
        /// <summary>
        /// p320:
        /// file-name-1
        /// Must be defined in an FD entry in the DATA DIVISION and must be the
        /// name of an indexed or relative file.
        /// </summary>
        public SymbolReference<FileName> FileName = null;

        public DeleteStatement() : base(CodeElementType.DeleteStatement) { }
    }
}
