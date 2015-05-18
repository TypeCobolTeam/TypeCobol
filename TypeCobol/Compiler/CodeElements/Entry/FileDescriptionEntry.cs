using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Provides information about the physical structure and identification of a file 
    /// </summary>
    public class FileDescriptionEntry : CodeElement
    {
        public FileDescriptionEntry() : base(CodeElementType.FileDescriptionEntry)
        { }

        /// <summary>
        /// A level indicator, with its descriptive entry, identifies each file in a program. 
        /// Level indicators represent the highest level of any data hierarchy with which they are associated. 
        /// FD is the file description level indicator and SD is the sort-merge file description level indicator.
        /// </summary>
        public FileDescriptionType Type { get; set; }

        /// <summary>
        /// A file-name must conform to the rules for a COBOL user-defined name, must contain at least one alphabetic character, 
        /// and must be unique within this program.
        /// </summary>
        public FileName FileName { get; set; }

        /// <summary>
        /// The EXTERNAL clause specifies that a file connector is external, and permits
        /// communication between two programs by the sharing of files.
        /// A file connector is external if the storage associated with that file is associated with
        /// the run unit rather than with any particular program within the run unit. An
        /// external file can be referenced by any program in the run unit that describes the
        /// file. References to an external file from different programs that use separate
        /// descriptions of the file are always to the same file. In a run unit, there is only one
        /// representative of an external file.
        /// </summary>
        public bool IsExternal { get; set; }

        /// <summary>
        /// The GLOBAL clause specifies that the file connector named by a file-name is a
        /// global name. A global file-name is available to the program that declares it and to
        /// every program that is contained directly or indirectly in that program.
        /// Two programs in a run unit can reference global file connectors in the following
        /// circumstances:
        /// - An external file connector can be referenced from any program that describes
        ///   that file connector.
        /// - If a program is contained within another program, both programs can refer to a
        ///   global file connector by referring to an associated global file-name either in the
        ///   containing program or in any program that directly or indirectly contains the
        ///   containing program.
        /// </summary>
        public bool IsGlobal { get; set; }

        /// <summary>
        /// The BLOCK CONTAINS clause specifies the size of the physical records.
        /// If the records in the file are not blocked, the BLOCK CONTAINS clause can be
        /// omitted. When it is omitted, the compiler assumes that records are not blocked.
        /// When integer-1 and integer-2 are both
        /// specified, they represent the minimum and maximum number of
        /// bytes in the physical record, respectively.
        /// </summary>
        public int MinBlockSize { get; set; }

        /// <summary>
        /// When integer-1 and integer-2 are both
        /// specified, they represent the minimum and maximum number of
        /// bytes in the physical record, respectively.
        /// </summary>
        public int MaxBlockSize { get; set; }

        /// <summary>
        /// CHARACTERS
        /// Specifies the number of bytes required to store the physical record,
        /// no matter what USAGE the data items have within the data record.
        /// RECORDS
        /// Specifies the number of logical records contained in each physical
        /// record.
        /// </summary>
        public BlockSizeUnit BlockSizeUnit { get; set; }

        /// <summary>
        /// When the RECORD clause is used, the record size must be specified as the number
        /// of bytes needed to store the record internally, regardless of the USAGE of the data
        /// items contained within the record.
        /// When the RECORD clause is omitted, the compiler determines the record lengths
        /// from the record descriptions. When one of the entries within a record description
        /// contains an OCCURS DEPENDING ON clause, the compiler uses the maximum
        /// value of the variable-length item to calculate the number of bytes needed to store
        /// the record internally.
        /// Fixed-length records are obtained when all 01 record description entry lengths are
        /// the same.
        /// integer-4 specifies the size of the smallest data record.
        /// </summary>
        public int MinRecordSize { get; set; }

        /// <summary>
        /// When the RECORD clause is used, the record size must be specified as the number
        /// of bytes needed to store the record internally, regardless of the USAGE of the data
        /// items contained within the record.
        /// integer-5 specifies the size of the largest data record.
        /// </summary>
        public int MaxRecordSize { get; set; }

        /// <summary>
        /// If data-name-1 is specified:
        /// - data-name-1 must be an elementary unsigned integer.
        /// - The number of bytes in the record must be placed into the data item referenced
        ///   by data-name-1 before any RELEASE, REWRITE, or WRITE statement is executed
        ///   for the file.
        /// - The execution of a DELETE, RELEASE, REWRITE, START, or WRITE statement
        ///   or the unsuccessful execution of a READ or RETURN statement does not alter
        ///   the content of the data item referenced by data-name-1.
        /// - After the successful execution of a READ or RETURN statement for the file, the
        ///   contents of the data item referenced by data-name-1 indicate the number of bytes
        ///   in the record just read.
        /// During the execution of a RELEASE, REWRITE, or WRITE statement, the number
        /// of bytes in the record is determined by the following conditions:
        /// - If data-name-1 is specified, by the content of the data item referenced by
        ///   data-name-1
        /// - If data-name-1 is not specified and the record does not contain a variable
        ///   occurrence data item, by the number of bytes positions in the record
        /// - If data-name-1 is not specified and the record contains a variable occurrence data
        ///   item, by the sum of the fixed position and that portion of the table described by
        ///   the number of occurrences at the time of execution of the output statement
        /// During the execution of a READ ... INTO or RETURN ... INTO statement, the
        /// number of bytes in the current record that participate as the sending data items in
        /// the implicit MOVE statement is determined by the following conditions:
        /// - If data-name-1 is specified, by the content of the data item referenced by
        ///   data-name-1
        /// - If data-name-1 is not specified, by the value that would have been moved into
        // /  the data item referenced by data-name-1 had data-name-1 been specified
        /// </summary>
        public SymbolReference<DataName> RecordSizeDependingOn { get; set; }

        /// <summary>
        /// The LABEL RECORDS clause documents the presence or absence of labels.
        /// For sequential, relative, or indexed files, and for sort/merge SDs, the LABEL
        /// RECORDS clause is syntax checked, but has no effect on the execution of the
        /// program.
        /// STANDARD
        /// Labels conforming to system specifications exist for this file.
        /// STANDARD is permitted for mass storage devices and tape devices.
        /// OMITTED
        /// No labels exist for this file.
        /// OMITTED is permitted for tape devices.
        /// data-name-2
        /// User labels are present in addition to standard labels. 
        /// </summary>
        public LabelRecordType LabelRecordType { get; set; }

        /// <summary>
        ///  User labels are present in addition to standard labels. 
        ///  data-name-2 specifies the name of a user label record.
        ///  data-name-2 must appear as the subject of a record description entry associated with the file.
        /// </summary>
        public SymbolReference<DataName> LabelRecord { get; set; }

        // The VALUE OF clause describes an item in the label records associated with the file.
        // The VALUE OF clause is syntax checked, but has no effect on the execution of the program.
        // ===> because the the LABEL RECORDS clause is obsolete, we ignore the VALUE OF clause in this code model

        /// <summary>
        /// The DATA RECORDS clause is syntax checked but serves only as documentation
        /// for the names of data records associated with the file.
        /// data-name-4
        /// The names of record description entries associated with the file.
        /// The data-name need not have an associated 01 level number record description
        /// with the same name.
        /// </summary>
        public IList<SymbolReference<DataName>> DataRecords { get; set; }

        /// <summary>
        /// The LINAGE clause specifies the number of lines that can be written or spaced on this logical page.
        /// The area of the page that these lines represent is called the page body.
        /// The value must be greater than zero.
        /// The LINAGE clause is effective for sequential files opened as OUTPUT or EXTEND.
        /// A LINAGE clause under an SD is syntax checked, but has no effect on the execution of the program.
        /// The logical page size specified in the LINAGE clause is the sum of all values
        /// specified in each phrase except the FOOTING phrase.
        /// </summary>
        public LiteralOrSymbolReference<int, DataName> LogicalPageBodyLineCount { get; set; }

        /// <summary>
        /// WITH FOOTING AT
        /// integer-9 or the value of the data item in data-name-6 specifies the first line
        /// number of the footing area within the page body. The footing line number
        /// must be greater than zero, and not greater than the last line of the page
        /// body. The footing area extends between those two lines.
        /// </summary>
        public LiteralOrSymbolReference<int, DataName> LogicalPageFootingLineNumber { get; set; }

        /// <summary>
        /// LINES AT TOP
        /// integer-10 or the value of the data item in data-name-7 specifies the number
        /// of lines in the top margin of the logical page. The value can be zero.
        /// </summary>
        public LiteralOrSymbolReference<int, DataName> LogicalPageTopMarginLineCount { get; set; }

        /// <summary>
        /// LINES AT BOTTOM
        /// integer-11 or the value of the data item in data-name-8 specifies the number
        /// of lines in the bottom margin of the logical page. The value can be zero.
        /// </summary>
        public LiteralOrSymbolReference<int, DataName> LogicalPageBottomMarginLineCount { get; set; }

        /// <summary>
        /// The RECORDING MODE clause specifies the format of the physical records in a QSAM file. 
        /// Permitted values for RECORDING MODE are:
        /// * Recording mode F (fixed)
        /// * Recording mode V (variable)
        /// * Recording mode U (fixed or variable)
        /// * Recording mode S (spanned)
        /// The clause is ignored for a VSAM file.
        /// </summary>
        public QSAMRecordingMode QSAMRecordingMode { get; set; }

        /// <summary>
        /// The CODE-SET clause specifies the character code used to represent data on a
        /// magnetic tape file. When the CODE-SET clause is specified, an alphabet-name
        /// identifies the character code convention used to represent data on the input-output
        /// device.
        /// alphabet-name must be defined in the SPECIAL-NAMES paragraph as
        /// STANDARD-1 (for ASCII-encoded files), STANDARD-2 (for ISO 7-bit encoded
        /// files), EBCDIC (for EBCDIC-encoded files), or NATIVE. When NATIVE is specified,
        /// the CODE-SET clause is syntax checked but has no effect on the execution of the
        /// program.
        /// The CODE-SET clause also specifies the algorithm for converting the character
        /// codes on the input-output medium from and to the internal EBCDIC character set.
        /// When the CODE-SET clause is specified for a file, all data in the file must have
        /// USAGE DISPLAY; and if signed numeric data is present, it must be described with
        /// the SIGN IS SEPARATE clause.
        /// When the CODE-SET clause is omitted, the EBCDIC character set is assumed for
        /// the file.
        /// The CODE-SET clause is valid only for magnetic tape files.
        /// The CODE-SET clause is syntax checked but has no effect on the execution of the
        /// program when specified under an SD.
        /// </summary>
        public SymbolReference<AlphabetName> CodeSet { get; set; }
    }

    /// <summary>
    /// A level indicator, with its descriptive entry, identifies each file in a program. 
    /// Level indicators represent the highest level of any data hierarchy with which they are associated.  
    /// </summary>
    public enum FileDescriptionType
    {
        /// <summary>
        /// FD is the file description level indicator
        /// </summary>
        File,
        /// <summary>
        /// SD is the sort-merge file description level indicator
        /// </summary>
        SortMergeFile
    }

    /// <summary>
    /// CHARACTERS
    /// Specifies the number of bytes required to store the physical record,
    /// no matter what USAGE the data items have within the data record.
    /// RECORDS
    /// Specifies the number of logical records contained in each physical
    /// record.
    /// </summary>
    public enum BlockSizeUnit
    {
        Characters,
        Records
    }

    /// <summary>
    /// The LABEL RECORDS clause documents the presence or absence of labels.
    /// STANDARD
    /// Labels conforming to system specifications exist for this file.
    /// STANDARD is permitted for mass storage devices and tape devices.
    /// OMITTED
    /// No labels exist for this file.
    /// OMITTED is permitted for tape devices.
    /// data-name-2
    /// User labels are present in addition to standard labels. 
    /// </summary>
    public enum LabelRecordType
    {
        StandardLabels,
        UserLabels,
        Omitted
    }

    /// <summary>
    /// Permitted values for RECORDING MODE are:
    /// * Recording mode F (fixed)
    /// All the records in a file are the same length and each is wholly contained
    /// within one block. Blocks can contain more than one record, and there is
    /// usually a fixed number of records for each block. In this mode, there are
    /// no record-length or block-descriptor fields.
    ///* Recording mode V (variable)
    /// The records can be either fixed-length or variable-length, and each must be
    /// wholly contained within one block. Blocks can contain more than one
    /// record. Each data record includes a record-length field and each block
    /// includes a block-descriptor field. These fields are not described in the
    /// DATA DIVISION. They are each 4 bytes long and provision is
    /// automatically made for them. These fields are not available to you.
    /// * Recording mode U (fixed or variable)
    /// The records can be either fixed-length or variable-length. However, there is
    /// only one record for each block. There are no record-length or
    /// block-descriptor fields.
    /// You cannot use RECORDING MODE U if you are using the BLOCK
    /// CONTAINS clause.
    /// * Recording mode S (spanned)
    /// The records can be either fixed-length or variable-length, and can be larger
    /// than a block. If a record is larger than the remaining space in a block, a
    /// segment of the record is written to fill the block. The remainder of the
    /// record is stored in the next block (or blocks, if required). Only complete
    /// records are made available to you. Each segment of a record in a block,
    /// even if it is the entire record, includes a segment-descriptor field, and each
    /// block includes a block-descriptor field. These fields are not described in the
    /// DATA DIVISION; provision is automatically made for them. These fields
    /// are not available to you.
    /// When recording mode S is used, the BLOCK CONTAINS CHARACTERS clause
    /// must be used. Recording mode S is not allowed for ASCII files.
    /// </summary>
    public enum QSAMRecordingMode
    {
        /// <summary>
        /// The compiler determines the recording mode to be F if the largest level-01
        /// record associated with the file is not greater than the block size specified in
        /// the BLOCK CONTAINS clause, and you do one of the following things:
        /// - Use the RECORD CONTAINS integer clause. (For more information, see
        /// the Enterprise COBOL Migration Guide.)
        /// - Omit the RECORD clause and make sure that all level-01 records
        /// associated with the file are the same size and none contains an OCCURS
        /// DEPENDING ON clause.
        /// </summary>
        F,
        /// <summary>
        /// The compiler determines the recording mode to be V if the largest level-01
        /// record associated with the file is not greater than the block size specified in
        /// the BLOCK CONTAINS clause, and you do one of the following things:
        /// - Use the RECORD IS VARYING clause.
        /// - Omit the RECORD clause and make sure that all level-01 records
        ///   associated with the file are not the same size or some contain an
        ///   OCCURS DEPENDING ON clause.
        /// - Use the RECORD CONTAINS integer-1 TO integer-2 clause, with integer-1
        ///   the minimum length and integer-2 the maximum length of the level-01
        ///   records associated with the file. The two integers must be different, with
        ///   values matching minimum and maximum length of either different
        ///   length records or records with an OCCURS DEPENDING ON clause. 
        /// </summary>
        V,
        /// <summary>
        /// The compiler determines the recording mode to be S if the maximum block
        /// size is smaller than the largest record size.
        /// </summary>
        S,
        /// <summary>
        /// Recording mode U is never obtained by default. The RECORDING MODE
        /// U clause must be explicitly specified to get recording mode U.
        /// </summary>
        U
    }
}
