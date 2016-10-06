using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p364:
    /// The MERGE statement combines two or more identically sequenced files (that is,
    /// files that have already been sorted according to an identical set of ascending or
    /// descending keys) on one or more keys and makes records available in merged
    /// order to an output procedure or output file.
    ///
    /// A MERGE statement can appear anywhere in the PROCEDURE DIVISION except
    /// in a declarative section.
    ///
    /// The MERGE statement is not supported for programs compiled with the THREAD
    /// compiler option.
    /// </summary>
    public class MergeStatement : StatementElement
    {
        public MergeStatement() : base(CodeElementType.MergeStatement, StatementType.MergeStatement)
        { }
        
        /// <summary>
        /// p364:
        /// file-name-1
        /// The name given in the SD entry that describes the records to be merged.
        ///
        /// No file-name can be repeated in the MERGE statement.
        ///
        /// No pair of file-names in a MERGE statement can be specified in the same
        /// SAME AREA, SAME SORT AREA, or SAME SORT-MERGE AREA clause.
        /// However, any file-names in the MERGE statement can be specified in the
        /// same SAME RECORD AREA clause.
        ///
        /// When the MERGE statement is executed, all records contained in file-name-2,
        /// file-name-3, ... , are accepted by the merge program and then merged according to
        /// the keys specified.
        /// </summary>
        public SymbolReference FileName { get; set; }

        /// <summary>
        /// p365-366:
        /// ASCENDING/DESCENDING KEY phrase
        /// This phrase specifies that records are to be processed in an ascending or
        /// descending sequence (depending on the phrase specified), based on the specified
        /// merge keys.
        ///
        /// data-name-1
        /// Specifies a KEY data item on which the merge will be based. Each such
        /// data-name must identify a data item in a record associated with file-name-1.
        ///
        /// The data-names following the word KEY are listed from left to right in the
        /// MERGE statement in order of decreasing significance without regard to
        /// how they are divided into KEY phrases. The leftmost data-name is the
        /// major key, the next data-name is the next most significant key, and so
        /// forth.
        ///
        /// The following rules apply:
        /// * A specific key data item must be physically located in the same position
        /// and have the same data format in each input file. However, it need not
        /// have the same data-name.
        /// * If file-name-1 has more than one record description, the KEY data items
        /// need be described in only one of the record descriptions.
        /// * If file-name-1 contains variable-length records, all of the KEY data-items
        /// must be contained within the first n character positions of the record,
        /// where n equals the minimum records size specified for file-name-1.
        /// * KEY data items must not contain an OCCURS clause or be subordinate
        /// to an item that contains an OCCURS clause.
        /// * KEY data items cannot be:
        ///   – Variably located
        ///   – Group items that contain variable-occurrence data items
        ///   – Category numeric described with usage NATIONAL (national decimal type)
        ///   – Category external floating-point described with usage NATIONAL
        ///   (national floating-point)
        ///   – Category DBCS
        /// * KEY data items can be qualified.
        /// * KEY data items can be any of the following data categories:
        ///   – Alphabetic, alphanumeric, alphanumeric-edited
        ///   – Numeric (except numeric with usage NATIONAL)
        ///   – Numeric-edited (with usage DISPLAY or NATIONAL)
        ///   – Internal floating-point or display floating-point
        ///   – National or national-edited
        ///
        /// The direction of the merge operation depends on the specification of the
        /// ASCENDING or DESCENDING keywords as follows:
        /// * When ASCENDING is specified, the sequence is from the lowest key value to
        /// the highest key value.
        /// * When DESCENDING is specified, the sequence is from the highest key value to
        /// the lowest key value.
        /// If the KEY data item is described with usage NATIONAL, the sequence of the KEY
        /// values is based on the binary values of the national characters.
        ///
        /// When the COLLATING SEQUENCE phrase is not specified, the key comparisons
        /// are performed according to the rules for comparison of operands in a relation
        /// condition. For details, see “General relation conditions” on page 260.
        /// When the COLLATING SEQUENCE phrase is specified, the indicated collating
        /// sequence is used for key data items of alphabetic, alphanumeric,
        /// alphanumeric-edited, external floating-point, and numeric-edited categories. For all
        /// other key data items, the comparisons are performed according to the rules for
        /// comparison of operands in a relation condition.
        /// </summary>
        public IList<SortingKey> SortingKeys { get; set; }

        /// <summary>
        /// p366:
        /// COLLATING SEQUENCE phrase
        /// This phrase specifies the collating sequence to be used in alphanumeric
        /// comparisons for the KEY data items in this merge operation.
        ///
        /// The COLLATING SEQUENCE phrase has no effect for keys that are not alphabetic
        /// or alphanumeric.
        ///
        /// alphabet-name-1
        /// Must be specified in the ALPHABET clause of the SPECIAL-NAMES
        /// paragraph. Any one of the alphabet-name clause phrases can be specified,
        /// with the following results:
        ///
        ///   STANDARD-1
        ///   The ASCII collating sequence is used for all alphanumeric
        ///   comparisons. (The ASCII collating sequence is shown in “US
        ///   English ASCII code page” on page 572.)
        ///
        ///   STANDARD-2
        ///   The 7-bit code defined in the International Reference Version of
        ///   ISO/IEC 646, 7-bit coded character set for information interchange is
        ///   used for all alphanumeric comparisons.
        ///
        ///   NATIVE
        ///   The EBCDIC collating sequence is used for all alphanumeric
        ///   comparisons. (The EBCDIC collating sequence is shown in
        ///   “EBCDIC collating sequence” on page 569.)
        ///
        ///   EBCDIC
        ///   The EBCDIC collating sequence is used for all alphanumeric
        ///   comparisons. (The EBCDIC collating sequence is shown in
        ///   “EBCDIC collating sequence” on page 569.)
        ///   literal
        ///
        /// The collating sequence established by the specification of literals in
        /// the ALPHABET-NAME clause is used for all alphanumeric
        /// comparisons.
        ///
        /// When the COLLATING SEQUENCE phrase is omitted, the PROGRAM
        /// COLLATING SEQUENCE clause (if specified) in the OBJECT-COMPUTER
        /// paragraph identifies the collating sequence to be used. When both the
        /// COLLATING SEQUENCE phrase of the MERGE statement and the PROGRAM
        /// COLLATING SEQUENCE clause of the OBJECT-COMPUTER paragraph are
        /// omitted, the EBCDIC collating sequence is used.
        /// </summary>
        public SymbolReference CollatingSequence { get; set; }

        /// <summary>
        /// pp366-367:
        /// USING phrase
        /// file-name-2 , file-name-3 , ...
        /// Specifies the input files.
        ///
        /// During the MERGE operation, all the records on file-name-2, file-name-3, ... (that is,
        /// the input files) are transferred to file-name-1. At the time the MERGE statement is
        /// executed, these files must not be open. The input files are automatically opened,
        /// read, and closed. If DECLARATIVE procedures are specified for these files for
        /// input operations, the declaratives will be driven for errors if errors occur.
        /// All input files must specify sequential or dynamic access mode and be described in
        /// FD entries in the DATA DIVISION.
        ///
        /// If file-name-1 contains variable-length records, the size of the records contained in
        /// the input files (file-name-2, file-name-3, ...) must be neither less than the smallest
        /// record nor greater than the largest record described for file-name-1. If file-name-1
        /// contains fixed-length records, the size of the records contained in the input files
        /// must not be greater than the largest record described for file-name-1. For more
        /// information, see Sorting and merging files in the Enterprise COBOL Programming Guide.
        /// </summary>
        public SymbolReference[] InputFiles { get; set; }

        /// <summary>
        /// p367:
        /// GIVING phrase
        /// file-name-4 , ...
        /// Specifies the output files.
        ///
        /// When the GIVING phrase is specified, all the merged records in file-name-1 are
        /// automatically transferred to the output files (file-name-4, ...).
        /// All output files must specify sequential or dynamic access mode and be described
        /// in FD entries in the DATA DIVISION.
        ///
        /// If the output files (file-name-4, ...) contain variable-length records, the size of the
        /// records contained in file-name-1 must be neither less than the smallest record nor
        /// greater than the largest record described for the output files. If the output files
        /// contain fixed-length records, the size of the records contained in file-name-1 must
        /// not be greater than the largest record described for the output files. For more
        /// information, see Sorting and merging files in the Enterprise COBOL Programming Guide.
        ///
        /// At the time the MERGE statement is executed, the output files (file-name-4, ...) must
        /// not be open. The output files are automatically opened, read, and closed. If
        /// DECLARATIVE procedures are specified for these files for output operations, the
        /// declaratives will be driven for errors if errors occur.
        /// </summary>
        public SymbolReference[] OutputFiles { get; set; }

        /// <summary>
        /// pp367-368:
        /// OUTPUT PROCEDURE phrase
        /// This phrase specifies the name of a procedure that is to select or modify output
        /// records from the merge operation.
        ///
        /// procedure-name-1
        /// Specifies the first (or only) section or paragraph in the OUTPUT PROCEDURE.
        ///
        /// procedure-name-2
        /// Identifies the last section or paragraph of the OUTPUT PROCEDURE.
        ///
        /// The OUTPUT PROCEDURE can consist of any procedure needed to select, modify,
        /// or copy the records that are made available one at time by the RETURN statement
        /// in merged order from the file referenced by file-name-1. The range includes all
        /// statements that are executed as the result of a transfer of control by CALL, EXIT,
        /// GO TO, PERFORM, and XML PARSE statements in the range of the output
        /// procedure. The range also includes all statements in declarative procedures that are
        /// executed as a result of the execution of statements in the range of the output
        /// procedure. The range of the output procedure must not cause the execution of any
        /// MERGE, RELEASE, or SORT statement.
        ///
        /// If an output procedure is specified, control passes to it after the file referenced by
        /// file-name-1 has been sequenced by the MERGE statement. The compiler inserts a
        /// return mechanism at the end of the last statement in the output procedure and
        /// when control passes the last statement in the output procedure, the return
        /// mechanism provides the termination of the merge and then passes control to the
        /// next executable statement after the MERGE statement. Before entering the output
        /// procedure, the merge procedure reaches a point at which it can select the next
        /// record in merged order when requested. The RETURN statements in the output
        /// procedure are the requests for the next record.
        ///
        /// The OUTPUT PROCEDURE phrase is similar to a basic PERFORM statement. For
        /// example, if you name a procedure in an OUTPUT PROCEDURE, that procedure is
        /// executed during the merging operation just as if it were named in a PERFORM
        /// statement. As with the PERFORM statement, execution of the procedure is
        /// terminated after the last statement completes execution. The last statement in an
        /// OUTPUT PROCEDURE can be the EXIT statement (see “EXIT statement” on page 335).
        /// </summary>
        public SymbolReference OutputProcedure { get; set; }
        public SymbolReference ThroughOutputProcedure { get; set; }
    }

    /// <summary>
    /// p423:
    /// data-name-1
    /// Specifies a KEY data item on which the SORT statement will be based.
    /// Each such data-name must identify a data item in a record associated with
    /// file-name-1. The data-names following the word KEY are listed from left to
    /// right in the SORT statement in order of decreasing significance without
    /// regard to how they are divided into KEY phrases. The leftmost data-name
    /// is the major key, the next data-name is the next most significant key, and
    /// so forth. The following rules apply:
    /// * A specific KEY data item must be physically located in the same position
    /// and have the same data format in each input file. However, it need not
    /// have the same data-name.
    /// * If file-name-1 has more than one record description, the KEY data items
    /// need be described in only one of the record descriptions.
    /// * If file-name-1 contains variable-length records, all of the KEY data-items
    /// must be contained within the first n character positions of the record,
    /// where n equals the minimum records size specified for file-name-1.
    /// * KEY data items must not contain an OCCURS clause or be subordinate
    /// to an item that contains an OCCURS clause.
    /// * KEY data items cannot be:
    ///   – Variably located
    ///   – Group items that contain variable-occurrence data items
    ///   – Category numeric described with usage NATIONAL (national decimal item)
    ///   – Category external floating-point described with usage NATIONAL
    ///   (national floating-point item)
    ///   – Category DBCS
    /// * KEY data items can be qualified.
    /// * KEY data items can be any of the following data categories:
    ///   – Alphabetic, alphanumeric, alphanumeric-edited
    ///   – Numeric (except numeric with usage NATIONAL)
    ///   – Numeric-edited (with usage DISPLAY or NATIONAL)
    ///   – Internal floating-point or display floating-point
    ///   – National or national-edited
    ///
    /// pp423-424:
    /// If file-name-3 references an indexed file, the first specification of data-name-1 must
    /// be associated with an ASCENDING phrase and the data item referenced by that
    /// data-name-1 must occupy the same character positions in this record as the data
    /// item associated with the major record key for that file.
    ///
    /// The direction of the sorting operation depends on the specification of the
    /// ASCENDING or DESCENDING keywords as follows:
    /// * When ASCENDING is specified, the sequence is from the lowest key value to
    /// the highest key value.
    /// * When DESCENDING is specified, the sequence is from the highest key value to
    /// the lowest.
    /// * If the KEY data item is described with usage NATIONAL, the sequence of the
    /// KEY values is based on the binary values of the national characters.
    /// * If the KEY data item is internal floating point, the sequence of key values will be
    /// in numeric order.
    /// * When the COLLATING SEQUENCE phrase is not specified, the key
    /// comparisons are performed according to the rules for comparison of operands in
    /// a relation condition. See “General relation conditions” on page 260).
    /// * When the COLLATING SEQUENCE phrase is specified, the indicated collating
    /// sequence is used for key data items of alphabetic, alphanumeric,
    /// alphanumeric-edited, external floating-point, and numeric-edited categories. For
    /// all other key data items, the comparisons are performed according to the rules
    /// for comparison of operands in a relation condition.
    /// </summary>
    public class SortingKey
    {
        /// <summary>
        /// pp423-424:
        /// The direction of the sorting operation depends on the specification of the
        /// ASCENDING or DESCENDING keywords as follows:
        /// * When ASCENDING is specified, the sequence is from the lowest key value to
        /// the highest key value.
        /// </summary>
        public SyntaxProperty<SortingDirection> Direction { get; set; }

        /// <summary>
        /// p423:
        /// data-name-1
        /// Specifies a KEY data item on which the SORT statement will be based.
        /// Each such data-name must identify a data item in a record associated with
        /// file-name-1.
        /// </summary>
        public SymbolReference DataItem { get; set; }
    }

    /// <summary>
    /// pp423-424:
    /// The direction of the sorting operation depends on the specification of the
    /// ASCENDING or DESCENDING keywords as follows:
    /// * When DESCENDING is specified, the sequence is from the highest key value to
    /// the lowest.
    /// </summary>
    public enum SortingDirection
    {
        Ascending,
        Descending
    }
}
