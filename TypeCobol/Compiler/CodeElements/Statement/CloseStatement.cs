using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p313:
    /// The CLOSE statement terminates the processing of volumes and files.
    ///
    /// p314:
    /// A CLOSE statement can be executed only for a file in an open mode. After
    /// successful execution of a CLOSE statement (without the REEL/UNIT phrase if
    /// using format 1):
    /// * The record area associated with the file-name is no longer available.
    /// Unsuccessful execution of a CLOSE statement leaves availability of the record
    /// data undefined.
    /// * An OPEN statement for the file must be executed before any other input/output
    /// statement can be executed for the file and before data is moved to a record
    /// description entry associated with the file.
    /// </summary>
    public class CloseStatement : StatementElement
    {
        public CloseStatement() : base(CodeElementType.CloseStatement, StatementType.CloseStatement)
        { }

        /// <summary>
        /// p313:
        /// Designates the file upon which the CLOSE statement is to operate. If more
        /// than one file-name is specified, the files need not have the same
        /// organization or access. [Each filename] must not be a sort or merge file.
        /// </summary>
        public CloseFileInstruction[] CloseFileInstructions { get; set; }
        
    }

    public class CloseFileInstruction
    {
        /// <summary>
        /// p313:
        /// Designates the file upon which the CLOSE statement is to operate. If more
        /// than one file-name is specified, the files need not have the same
        /// organization or access. [FileName] must not be a sort or merge file.
        /// </summary>
        public SymbolReference FileName { get; set; }

        /// <summary>
        /// p314:
        /// REEL and UNIT
        /// You can specify these phrases only for QSAM multivolume or single
        /// volume files. The terms REEL and UNIT are interchangeable.
        /// p313:
        /// The REEL, UNIT, and NO REWIND phrases are not valid for VSAM files.
        /// </summary>
        public SyntaxProperty<bool> IsReelUnit { get; set; }

        /// <summary>
        /// p314:
        /// WITH NO REWIND and FOR REMOVAL
        /// These phrases apply only to QSAM tape files. If they are specified for
        /// storage devices to which they do not apply, the close operation is
        /// successful and a status key value is set to indicate the file was on a
        /// non-reel medium.
        /// </summary>
        public SyntaxProperty<bool> IsForRemoval { get; set; }

        /// <summary>
        /// p314:
        /// WITH NO REWIND and FOR REMOVAL
        /// These phrases apply only to QSAM tape files. If they are specified for
        /// storage devices to which they do not apply, the close operation is
        /// successful and a status key value is set to indicate the file was on a
        /// non-reel medium.
        /// </summary>
        public SyntaxProperty<bool> IsWithNoRewind { get; set; }

        /// <summary>
        /// issue #62: LOCK is notified when you don't want the file can be opened again during the execution of program.
        /// </summary>
        public SyntaxProperty<bool> IsWithLock { get; set; }
    }
}
