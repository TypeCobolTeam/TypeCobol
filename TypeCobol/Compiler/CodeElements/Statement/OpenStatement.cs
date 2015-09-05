using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p380:
    /// The phrases INPUT, OUTPUT, I-O, and EXTEND specify the mode to be used for
    /// opening the file. At least one of the phrases INPUT, OUTPUT, I-O, or EXTEND
    /// must be specified with the OPEN keyword. The INPUT, OUTPUT, I-O, and
    /// EXTEND phrases can appear in any order.
    ///
    /// INPUT
    /// Permits input operations.
    ///
    /// OUTPUT
    /// Permits output operations. This phrase can be specified when the file is
    /// being created.
    /// Do not specify OUTPUT for files that:
    /// * Contain records. The file will be replaced by new data.
    /// If the OUTPUT phrase is specified for a file that already contains
    /// records, the data set must be defined as reusable and cannot have an
    /// alternate index. The records in the file will be replaced by the new data
    /// and any ALTERNATE RECORD KEY clause in the SELECT statement
    /// will be ignored.
    /// * Are defined with a DD dummy card. Unpredictable results can occur.
    ///
    /// I-O
    /// Permits both input and output operations. The I-O phrase can be specified
    /// only for files assigned to direct access devices.
    /// The I-O phrase is not valid for line-sequential files.
    ///
    /// EXTEND
    /// Permits output operations that append to or create a file.
    /// The EXTEND phrase is allowed for sequential access files only if the new
    /// data is written in ascending sequence. The EXTEND phrase is allowed for
    /// files that specify the LINAGE clause.
    /// For QSAM files, do not specify the EXTEND phrase for a multiple file reel.
    /// If you want to append to a file, but are unsure if the file exists, use the
    /// SELECT OPTIONAL clause before opening the file in EXTEND mode. The
    /// file will be created or appended to, depending on whether the file exists.
    /// </summary>
    public enum OpenMode { INPUT, OUTPUT, IO, EXTEND, NONE };

    /// <summary>
    /// p379:
    /// The OPEN statement initiates the processing of files. It also checks or writes labels, or both.
    /// </summary>
    public class OpenStatement : CodeElement
    {
        /// <summary>
        /// p380:
        /// Designate [FileNames] upon which the OPEN statement is to operate. If more
        /// than one file is specified, the files need not have the same organization or
        /// access mode.
        /// </summary>
        Dictionary<OpenMode, IList<OpenFileName>> FileNames;

        public OpenStatement(Dictionary<OpenMode, IList<OpenFileName>> filenames)
            : base(CodeElementType.OpenStatement)
        {
            this.FileNames = (filenames != null ? filenames : new Dictionary<OpenMode, IList<OpenFileName>>());
        }
    }

    /// <summary>
    /// p380:
    /// Designate a file upon which the OPEN statement is to operate. If more
    /// than one file is specified, the files need not have the same organization or
    /// access mode.
    /// </summary>
    public class OpenFileName
    {
        /// <summary>
        /// p380:
        /// Each file-name must be defined in an FD entry in the DATA
        /// DIVISION and must not name a sort or merge file. The FD entry must be
        /// equivalent to the information supplied when the file was defined.
        /// </summary>
        SymbolReference<FileName> FileName;
        /// <summary>
        /// REVERSED
        /// Valid only for sequential single-reel files. REVERSED is not valid for
        /// VSAM files.
        /// If the concept of reels has no meaning for the storage medium (for
        /// example, a direct access device), the REVERSED and NO REWIND phrases
        /// do not apply.
        /// </summary>
        bool IsNoRewind;
        /// <summary>
        /// NO REWIND
        /// Valid only for sequential single-reel files. It is not valid for VSAM files.
        /// </summary>
        bool IsReversed;

        public OpenFileName(SymbolReference<FileName> filename, bool norewind = false, bool reversed = false)
        {
            this.FileName = filename;
            this.IsNoRewind = norewind;
            this.IsReversed = reversed;
        }
    }
}
