using System.Collections.Generic;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Provides information about the physical structure and identification of a file 
    /// </summary>
    public class FileDescriptionEntry : DataDefinitionEntry
    {
        public FileDescriptionEntry() : base(CodeElementType.FileDescriptionEntry)
        { }

        /// <summary>
        /// A level indicator, with its descriptive entry, identifies each file in a program. 
        /// Level indicators represent the highest level of any data hierarchy with which they are associated. 
        /// FD is the file description level indicator and SD is the sort-merge file description level indicator.
        /// </summary>
        public SyntaxProperty<FileDescriptionType> LevelIndicator { get; set; }

        /// <summary>
        /// A file-name must conform to the rules for a COBOL user-defined name, must contain at least one alphabetic character, 
        /// and must be unique within this program.
        /// </summary>
        public SymbolReference FileName { get; set; }

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
        public SyntaxProperty<bool> External { get; set; }

        public bool IsExternal => External != null && External.Value;

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
        public SyntaxProperty<bool> Global { get; set; }

        public bool IsGlobal => Global != null && Global.Value;

        /// <summary>
        /// The BLOCK CONTAINS clause specifies the size of the physical records.
        /// If the records in the file are not blocked, the BLOCK CONTAINS clause can be
        /// omitted. When it is omitted, the compiler assumes that records are not blocked.
        /// When integer-1 and integer-2 are both
        /// specified, they represent the minimum and maximum number of
        /// bytes in the physical record, respectively.
        /// </summary>
        public IntegerValue MinBlockSize { get; set; }

        /// <summary>
        /// When integer-1 and integer-2 are both
        /// specified, they represent the minimum and maximum number of
        /// bytes in the physical record, respectively.
        /// </summary>
        public IntegerValue MaxBlockSize { get; set; }

        /// <summary>
        /// CHARACTERS
        /// Specifies the number of bytes required to store the physical record,
        /// no matter what USAGE the data items have within the data record.
        /// RECORDS
        /// Specifies the number of logical records contained in each physical
        /// record.
        /// </summary>
        public SyntaxProperty<BlockSizeUnit> BlockSizeUnit { get; set; }

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
        public IntegerValue MinRecordSize { get; set; }

        /// <summary>
        /// When the RECORD clause is used, the record size must be specified as the number
        /// of bytes needed to store the record internally, regardless of the USAGE of the data
        /// items contained within the record.
        /// integer-5 specifies the size of the largest data record.
        /// </summary>
        public IntegerValue MaxRecordSize { get; set; }

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
        ///  the data item referenced by data-name-1 had data-name-1 been specified
        /// </summary>
        public SymbolReference RecordSizeDependingOn { get; set; }

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
        public SyntaxProperty<LabelRecordType> LabelRecordType { get; set; }

        /// <summary>
        ///  User labels are present in addition to standard labels. 
        ///  data-name-2 specifies the name of a user label record.
        ///  data-name-2 must appear as the subject of a record description entry associated with the file.
        /// </summary>
        public SymbolReference[] LabelRecords { get; set; }

        /// <summary>
        /// The VALUE OF clause describes an item in the label records associated with the file.
        /// The VALUE OF clause is syntax checked, but has no effect on the execution of the program.
        /// </summary>
        public IDictionary<SymbolReference, Variable> ValueOfLabelRecords { get; set; }

        /// <summary>
        /// The DATA RECORDS clause is syntax checked but serves only as documentation
        /// for the names of data records associated with the file.
        /// data-name-4
        /// The names of record description entries associated with the file.
        /// The data-name need not have an associated 01 level number record description
        /// with the same name.
        /// </summary>
        public SymbolReference[] DataRecords { get; set; }

        /// <summary>
        /// The LINAGE clause specifies the number of lines that can be written or spaced on this logical page.
        /// The area of the page that these lines represent is called the page body.
        /// The value must be greater than zero.
        /// The LINAGE clause is effective for sequential files opened as OUTPUT or EXTEND.
        /// A LINAGE clause under an SD is syntax checked, but has no effect on the execution of the program.
        /// The logical page size specified in the LINAGE clause is the sum of all values
        /// specified in each phrase except the FOOTING phrase.
        /// </summary>
        public IntegerVariable LogicalPageBodyLineCount { get; set; }

        /// <summary>
        /// WITH FOOTING AT
        /// integer-9 or the value of the data item in data-name-6 specifies the first line
        /// number of the footing area within the page body. The footing line number
        /// must be greater than zero, and not greater than the last line of the page
        /// body. The footing area extends between those two lines.
        /// </summary>
        public IntegerVariable LogicalPageFootingLineNumber { get; set; }

        /// <summary>
        /// LINES AT TOP
        /// integer-10 or the value of the data item in data-name-7 specifies the number
        /// of lines in the top margin of the logical page. The value can be zero.
        /// </summary>
        public IntegerVariable LogicalPageTopMarginLineCount { get; set; }

        /// <summary>
        /// LINES AT BOTTOM
        /// integer-11 or the value of the data item in data-name-8 specifies the number
        /// of lines in the bottom margin of the logical page. The value can be zero.
        /// </summary>
        public IntegerVariable LogicalPageBottomMarginLineCount { get; set; }

        /// <summary>
        /// The RECORDING MODE clause specifies the format of the physical records in a QSAM file. 
        /// The clause is ignored for a VSAM file.
        /// </summary>
        [CanBeNull]
        public EnumeratedValue RecordingMode { get; set; }

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
        public SymbolReference CodeSet { get; set; }
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
}
