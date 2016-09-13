using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The I-O-CONTROL paragraph of the input-output section specifies 
    /// when checkpoints are to be taken and 
    /// the storage areas to be shared by different files. 
    /// </summary>
    public class IOControlEntry : CodeElement
    {
        public IOControlEntry() : base(CodeElementType.IOControlEntry)
        { }
    }

    /// <summary>
    /// The RERUN clause specifies that checkpoint records are to be taken.    
    /// SORT/MERGE considerations: 
    /// When the RERUN clause is specified in the I-O-CONTROL paragraph, checkpoint records are written at logical intervals determined by the sort/merge program during execution of each SORT or MERGE statement in the program. 
    /// When the RERUN clause is omitted, checkpoint records are not written. 
    /// There can be only one SORT/MERGE I-O-CONTROL paragraph in a program, and it cannot be specified in contained programs. It will have a global effect on all SORT and MERGE statements in the program unit. 
    /// </summary>
    public class RerunClause : IOControlEntry
    {
        /// <summary>
        /// assignment-name-1 The external data set for the checkpoint file. It must not be the same assignment-name as that specified in any ASSIGN clause throughout the entire program, including contained and containing programs. 
        /// For QSAM files, assignment-name-1 has the format: label-? S-? name 
        /// The QSAM file must reside on a tape or direct access device.
        /// </summary>
        public SymbolReference<AssignmentName> OnExternalDataSet { get; set; }

        /// <summary>
        /// file-name-1 Must be a sequentially organized file. 
        /// VSAM and QSAM considerations: 
        /// The file named in the RERUN clause must be a file defined in the same program as the I-O-CONTROL paragraph, even if the file is defined as GLOBAL. 
        /// </summary>
        public SymbolReference<FileName> OnFileName { get; set; }

        /// <summary>
        /// EVERY integer-1 RECORDS 
        /// A checkpoint record is to be written for every integer-1 records in file-name-1 that are processed. 
        /// EVERY END OF REEL/UNIT
        /// A checkpoint record is to be written whenever end-of-volume for file-name-1 occurs.
        /// </summary>
        public CheckPointFrequency CheckPointFrequency { get; set; }

        /// <summary>
        /// A checkpoint record is to be written for every integer-1 records in file-name-1 that are processed. 
        /// If you specify the integer-1 RECORDS phrase, you must specify assignment-name-1. 
        /// </summary>
        public int EveryRecordCount { get; set; }

        public SymbolReference<FileName> OfFileName { get; set; }
    }

    /// <summary>
    /// EVERY integer-1 RECORDS 
    /// A checkpoint record is to be written for every integer-1 records in file-name-1 that are processed. 
    /// When multiple integer-1 RECORDS phrases are specified, no two of them can specify the same value for file-name-1. 
    /// If you specify the integer-1 RECORDS phrase, you must specify assignment-name-1. 
    /// EVERY END OF REEL/UNIT
    /// A checkpoint record is to be written whenever end-of-volume for file-name-1 occurs. The terms REEL and UNIT are interchangeable. 
    /// When multiple END OF REEL/UNIT phrases are specified, no two of them can specify the same value for file-name-1. 
    /// The END OF REEL/UNIT phrase can be specified only if file-name-1 is a sequentially organized file.
    /// </summary>
    public enum CheckPointFrequency
    {
        EveryRecordCount,
        EveryEndOfReelUnit
    }

    /// <summary>
    /// The SAME AREA clause is syntax checked, but has no effect on the execution of the program.
    /// The SAME AREA clause specifies that two or more files that do not represent sort or merge files are to use the same main storage area during processing.
    /// The files named in a SAME AREA clause need not have the same organization or access.
    /// The SAME RECORD AREA clause specifies that two or more files are to use the same main storage area for processing the current logical record.
    /// The files named in a SAME RECORD AREA clause need not have the same organization or access. 
    /// // file-name-3 , file-name-4 Must be specified in the file-control paragraph of the same program. file-name-3 and file-name-4 must not reference a file that is defined with the EXTERNAL clause.
    /// All of the files can be opened at the same time. A logical record in the shared storage area is considered to be both of the following ones: 
    /// - A logical record of each opened output file in the SAME RECORD AREA clause 
    /// - A logical record of the most recently read input file in the SAME RECORD AREA clause
    /// More than one SAME RECORD AREA clause can be included in a program. However: 
    /// - A specific file-name must not appear in more than one SAME RECORD AREA clause. 
    /// - If one or more file-names of a SAME AREA clause appear in a SAME RECORD AREA clause, all the file-names in that SAME AREA clause must appear in that SAME RECORD AREA clause. However, the SAME RECORD AREA clause can contain additional file-names that do not appear in the SAME AREA clause. 
    /// - The rule that in the SAME AREA clause only one file can be open at one time takes precedence over the SAME RECORD AREA rule that all the files can be open at the same time. 
    /// - If the SAME RECORD AREA clause is specified for several files, the record description entries or the file description entries for these files must not include the GLOBAL clause.
    /// - The SAME RECORD AREA clause must not be specified when the RECORD CONTAINS 0 CHARACTERS clause is specified.
    /// The SAME SORT AREA clause is syntax checked but has no effect on the execution of the program. 
    /// The SAME SORT-MERGE AREA clause is equivalent to the SAME SORT AREA clause.
    /// </summary>
    public class SameAreaClause : IOControlEntry
    {
        public SameAreaType SameAreaType { get; set; }

        public IList<SymbolReference<FileName>> FileNames { get; set; }
    }

    /// <summary>
    /// The SAME AREA clause specifies that two or more files that do not represent sort or merge files are to use the same main storage area during processing.
    /// The SAME RECORD AREA clause specifies that two or more files are to use the same main storage area for processing the current logical record.
    /// The SAME SORT AREA clause is syntax checked but has no effect on the execution of the program. 
    /// The SAME SORT-MERGE AREA clause is equivalent to the SAME SORT AREA clause.
    /// </summary>
    public enum SameAreaType
    {
        SameArea,
        SameRecordArea,
        SameSortArea,
        SameSortMergeArea
    }

    /// <summary>
    /// The MULTIPLE FILE TAPE clause (format 1) specifies that two or more files share the same physical reel of tape.
    /// This clause is syntax checked, but has no effect on the execution of the program. 
    /// The function is performed by the system through the LABEL parameter of the DD statement.
    /// </summary>
    public class MultipleFileTapeClause : IOControlEntry
    {
        IList<SymbolReference<FileName>> FileNames { get; set; }

        IList<int> FilePositions { get; set; }
    }

    /// <summary>
    /// The APPLY WRITE-ONLY clause optimizes buffer and device space allocation for files that have standard sequential organization, have variable-length records, and are blocked.
    /// If you specify this phrase, the buffer is truncated only when the space available in the buffer is smaller than the size of the next record. 
    /// Otherwise, the buffer is truncated when the space remaining in the buffer is smaller than the maximum record size for the file.
    /// APPLY WRITE-ONLY is effective only for QSAM files. 
    /// For an alternate method of achieving the APPLY WRITE-ONLY results, see the description of the compiler option, AWO in the Enterprise COBOL Programming Guide.
    /// </summary>
    public class ApplyWriteOnlyClause : IOControlEntry
    {
        /// <summary>
        /// file-name-2 Each file must have standard sequential organization.    
        /// </summary>
        IList<SymbolReference<FileName>> FileNames { get; set; }
    }
}
