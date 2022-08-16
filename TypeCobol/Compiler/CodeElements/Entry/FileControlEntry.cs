using System;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The FILE-CONTROL paragraph associates each file in the COBOL program with
    /// an external data set, specifies its access mode.
    /// </summary>
    public class FileControlEntry : CodeElement
    {
        public FileControlEntry() : base(CodeElementType.FileControlEntry)
        { }

        /// <summary>
        /// Name of the file in the Cobol program.
        /// Must be identified by an FD or SD entry in the DATA DIVISION.
        /// </summary>
        [CanBeNull]
        public SymbolReference FileName { get; set; }

        /// <summary>
        /// The ASSIGN clause associates the name of a file in a program with the actual external name of the data file.
        /// It is just a character string. It cannot contain an underscore character.
        /// Format: assignment-name for QSAM files
        /// label-? S-? name
        /// Format: assignment-name for VSAM sequential file
        /// label-? AS- name
        /// Format: assignment-name for line-sequential, VSAM indexed, or VSAM relative file
        /// label-? name
        /// The name component of assignment-name-1 is initially treated as a ddname. 
        /// If no file has been allocated using this ddname, then name is treated as an environment variable
        /// </summary>
        [CanBeNull]
        public ExternalName ExternalDataSet { get; set; }

        /// <summary>
        /// Description of the structure of the file
        /// </summary>
        public FileStructure Structure { get; set; }

        /// <summary>
        /// You must specify SELECT OPTIONAL for those input files that are not necessarily available each time the object program is executed. 
        /// Can be specified only for files opened in the input, I-O, or extend mode. 
        /// </summary>
        public SyntaxProperty<bool> IsOptional { get; set; }

        /// <summary>
        /// The ACCESS MODE clause defines the manner in which the records of the file are made available for processing. 
        /// If the ACCESS MODE clause is not specified, sequential access is assumed.
        /// NB : File organization is the permanent logical structure of the file.
        /// You tell the computer how to retrieve records from the file by specifying the access mode (sequential, random, or dynamic).
        /// </summary>
        public SyntaxProperty<FileAccessMode> AccessMode { get; set; }

        /// <summary>
        /// RESERVE clause 
        /// The RESERVE clause allows the user to specify the number of input/output buffers to be allocated at run time for the files.
        /// The RESERVE clause is not supported for line-sequential files.
        /// If the RESERVE clause is omitted, the number of buffers at run time is taken from the DD statement. 
        /// If none is specified, the system default is taken.
        /// </summary>
        public IntegerValue ReserveIOBuffersCount { get; set; }

        /// <summary>
        /// The FILE STATUS clause monitors the execution of each input-output operation for the file.
        /// When the FILE STATUS clause is specified, the system moves a value into the file status key data item after each input-output operation that explicitly or implicitly refers to this file. 
        /// The value indicates the status of execution of the statement. 
        /// data-name-1 The file status key data item can be defined in the WORKING-STORAGE, LOCAL-STORAGE, or LINKAGE SECTION as one of the following items: v A two-character data item of category alphanumeric v A two-character data item of category national v A two-digit data item of category numeric with usage DISPLAY or NATIONAL (an external decimal data item) 
        /// The file status key data item must not be variably located; that is, the data item cannot follow a data item that contains an OCCURS DEPENDING ON clause. 
        /// </summary>
        public ReceivingStorageArea FileStatus { get; set; }

        /// <summary>
        /// The FILE STATUS clause monitors the execution of each input-output operation for the file.
        /// When the FILE STATUS clause is specified, the system moves a value into the file status key data item after each input-output operation that explicitly or implicitly refers to this file. 
        /// The value indicates the status of execution of the statement. 
        /// data-name-8 Must be defined as an alphanumeric group item of 6 bytes in the WORKING-STORAGE SECTION or LINKAGE SECTION of the DATA DIVISION. 
        /// Specify data-name-8 only if the file is a VSAM file (that is, ESDS, KSDS, RRDS). 
        /// data-name-8 holds the 6-byte VSAM return code, which is composed as follows: v The first 2 bytes of data-name-8 contain the VSAM return code in binary format. The value for this code is defined (by VSAM) as 0, 8, or 12. v The next 2 bytes of data-name-8 contain the VSAM function code in binary format. The value for this code is defined (by VSAM) as 0, 1, 2, 3, 4, or 5. v The last 2 bytes of data-name-8 contain the VSAM feedback code in binary format. The code value is 0 through 255. If VSAM returns a nonzero return code, data-name-8 is set. If FILE STATUS is returned without having called VSAM, data-name-8 is zero. 
        /// If data-name-1 is set to zero, the content of data-name-8 is undefined. VSAM status return code information is available without transformation in the currently defined COBOL FILE STATUS code. User identification and handling of exception conditions are allowed at the same level as that defined by VSAM. 
        /// Function code and feedback code are set if and only if the return code is set to a nonzero value. If they are referenced when the return code is set to zero, the contents of the fields are not dependable.
        /// Values in the return code, function code, and feedback code fields are defined by VSAM. There are no COBOL additions, deletions, or modifications to the VSAM definitions. 
        /// </summary>
        public ReceivingStorageArea VSAMReturnCode { get; set; }
    }

    /// <summary>
    /// File organization is the permanent logical structure of the file. 
    /// You tell the computer how to retrieve records from the file by specifying the access mode (sequential, random, or dynamic).
    /// </summary>
    public enum FileAccessMode
    {
        /// <summary>
        /// Can be specified for all file organizations.
        /// Format 1: sequential. Records in the file are accessed in the sequence established when the file is created or extended. Format 1 supports only sequential access. 
        /// Format 2: indexed. Records in the file are accessed in the sequence of ascending record key values according to the collating sequence of the file. 
        /// Format 3: relative. Records in the file are accessed in the ascending sequence of relative record numbers of existing records in the file. 
        /// Format 4: line-sequential. Records in the file are accessed in the sequence established when the file is created or extended. Format 4 supports only sequential access. 
        /// </summary>
        Sequential,
        /// <summary>
        /// Can be specified for indexed or relative file organizations only.
        /// Format 2: indexed. The value placed in a record key data item specifies the record to be accessed. 
        /// Format 3: relative. The value placed in a relative key data item specifies the record to be accessed.
        /// </summary>
        Random,
        /// <summary>
        /// Can be specified for indexed or relative file organizations only.
        /// Format 2: indexed. Records in the file can be accessed sequentially or randomly, depending on the form of the specific input-output statement used. 
        /// Format 3: relative. Records in the file can be accessed sequentially or randomly, depending on the form of the specific input-output request. 
        /// </summary>
        Dynamic
    }
}
