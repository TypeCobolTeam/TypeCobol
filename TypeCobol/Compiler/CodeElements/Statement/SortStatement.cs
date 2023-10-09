namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p422:
    /// The SORT statement accepts records from one or more files, sorts them according
    /// to the specified keys, and makes the sorted records available either through an
    /// output procedure or in an output file.
    ///
    /// The SORT statement can appear anywhere in the PROCEDURE DIVISION except
    /// in the declarative portion.
    ///
    /// See also “MERGE statement” on page 364.
    ///
    /// The SORT statement is not supported for programs compiled with the THREAD option.
    /// </summary>
    public class SortStatement : StatementElement
    {
        public SortStatement() : base(CodeElementType.SortStatement, StatementType.SortStatement)
        { }

        /// <summary>
        /// file-name-1
        /// The name given in the SD entry that describes the records to be sorted.
        /// No pair of file-names in a SORT statement can be specified in the same SAME SORT AREA clause or the
        /// SAME SORT-MERGE AREA clause. File-names associated with the GIVING clause (file-name-3, ...) cannot
        /// be specified in the SAME AREA clause; however, they can be associated with the SAME RECORD AREA
        /// clause.
        /// 
        /// data-name-2
        /// Specifies a table data-name that is subject to the following rules:
        /// - data-name-2 must have an OCCURS clause in the data description entry.
        /// - data-name-2 can be qualified.
        /// - data-name-2 can be subscripted. The rightmost or only subscript of the table must be omitted or
        /// replaced with the word ALL.
        /// The number of occurrences of table elements that are referenced by data-name-2 is determined
        /// by the rules in the OCCURS clause. The sorted table elements are placed in the same table that is
        /// referenced by data-name-2.
        /// </summary>
        public Variable FileNameOrTableName { get; set; }

        /// <summary>
        /// p423:
        /// ASCENDING KEY and DESCENDING KEY phrases
        /// This phrase specifies that records are to be processed in ascending or descending
        /// sequence (depending on the phrase specified), based on the specified sort keys.
        /// </summary>
        public IList<SortingKey> SortingKeys { get; set; }

        /// <summary>
        /// p424:
        /// DUPLICATES phrase
        /// If the DUPLICATES phrase is specified, and the contents of all the key elements
        /// associated with one record are equal to the corresponding key elements in one or
        /// more other records, the order of return of these records is as follows:
        /// * The order of the associated input files as specified in the SORT statement.
        /// Within a given file the order is that in which the records are accessed from that
        /// file.
        /// * The order in which these records are released by an input procedure, when an
        /// input procedure is specified.
        ///
        /// If the DUPLICATES phrase is not specified, the order of these records is undefined.
        /// </summary>
        public SyntaxProperty<bool> WithDuplicates { get; set; }

        /// <summary>
        /// pp424-425:
        /// COLLATING SEQUENCE phrase
        /// This phrase specifies the collating sequence to be used in alphanumeric
        /// comparisons for the KEY data items in this sorting operation.
        ///
        /// The COLLATING SEQUENCE phrase has no effect for keys that are not alphabetic
        /// or alphanumeric.
        ///
        /// alphabet-name-1
        /// Must be specified in the ALPHABET clause of the SPECIAL-NAMES
        /// paragraph. alphabet-name-1 can be associated with any one of the
        /// ALPHABET clause phrases, with the following results:
        ///
        ///   STANDARD-1
        ///   The ASCII collating sequence is used for all alphanumeric
        ///   comparisons. (The ASCII collating sequence is shown in
        ///   Appendix C, “EBCDIC and ASCII collating sequences,” on page 569.)
        ///
        ///   STANDARD-2
        ///   The International Reference Version of ISO/IEC 646, 7-bit coded
        ///   character set for information processing interchange is used for all
        ///   alphanumeric comparisons.
        ///
        ///   NATIVE
        ///   The EBCDIC collating sequence is used for all alphanumeric
        ///   comparisons. (The EBCDIC collating sequence is shown in
        ///   Appendix C, “EBCDIC and ASCII collating sequences,” on page 569.)
        ///
        ///   EBCDIC
        ///   The EBCDIC collating sequence is used for all alphanumeric
        ///   comparisons. (The EBCDIC collating sequence is shown in
        ///   Appendix C, “EBCDIC and ASCII collating sequences,” on page 569.)
        ///
        ///   literal
        ///   The collating sequence established by the specification of literals in
        ///   the alphabet-name clause is used for all alphanumeric comparisons.
        ///
        /// When the COLLATING SEQUENCE phrase is omitted, the PROGRAM
        /// COLLATING SEQUENCE clause (if specified) in the OBJECT-COMPUTER
        /// paragraph specifies the collating sequence to be used. When both the COLLATING
        /// SEQUENCE phrase and the PROGRAM COLLATING SEQUENCE clause are
        /// omitted, the EBCDIC collating sequence is used.
        /// </summary>
        public SymbolReference CollatingSequence { get; set; }

        /// <summary>
        /// p425:
        /// USING phrase
        /// file-name-2 , ...
        /// The input files.
        ///
        /// When the USING phrase is specified, all the records in file-name-2, ..., (that
        /// is, the input files) are transferred automatically to file-name-1. At the time
        /// the SORT statement is executed, these files must not be open. The compiler
        /// opens, reads, makes records available, and closes these files automatically.
        /// If EXCEPTION/ERROR procedures are specified for these files, the
        /// compiler makes the necessary linkage to these procedures.
        ///
        /// All input files must be described in FD entries in the DATA DIVISION.
        ///
        /// If the USING phrase is specified and if file-name-1 contains variable-length
        /// records, the size of the records contained in the input files (file-name-2, ...)
        /// must be neither less than the smallest record nor greater than the largest
        /// record described for file-name-1. If file-name-1 contains fixed-length records,
        /// the size of the records contained in the input files must not be greater than
        /// the largest record described for file-name-1. For more information, see
        /// Describing the input to sorting or merging in the Enterprise COBOL
        /// Programming Guide.
        /// </summary>
        public SymbolReference[] InputFiles { get; set; }

        /// <summary>
        /// pp425-426:
        /// INPUT PROCEDURE phrase
        /// This phrase specifies the name of a procedure that is to select or modify input
        /// records before the sorting operation begins.
        ///
        /// procedure-name-1
        /// Specifies the first (or only) section or paragraph in the input procedure.
        ///
        /// procedure-name-2
        /// Identifies the last section or paragraph of the input procedure.
        /// The input procedure can consist of any procedure needed to select, modify,
        /// or copy the records that are made available one at a time by the RELEASE
        /// statement to the file referenced by file-name-1. The range includes all
        /// statements that are executed as the result of a transfer of control by CALL,
        /// EXIT, GO TO, PERFORM, and XML PARSE statements in the range of the
        /// input procedure, as well as all statements in declarative procedures that are
        /// executed as a result of the execution of statements in the range of the input
        /// procedure. The range of the input procedure must not cause the execution
        /// of any MERGE, RETURN, or SORT statement.
        ///
        /// If an input procedure is specified, control is passed to the input procedure
        /// before the file referenced by file-name-1 is sequenced by the SORT
        /// statement. The compiler inserts a return mechanism at the end of the last
        /// statement in the input procedure. When control passes the last statement in
        /// the input procedure, the records that have been released to the file
        /// referenced by file-name-1 are sorted.
        /// </summary>
        public SymbolReference InputProcedure { get; set; }
        public SymbolReference ThroughInputProcedure { get; set; }

        /// <summary>
        /// p426:
        /// GIVING phrase
        /// file-name-3 , ...
        /// The output files.
        ///
        /// When the GIVING phrase is specified, all the sorted records in file-name-1
        /// are automatically transferred to the output files (file-name-3, ...).
        ///
        /// All output files must be described in FD entries in the DATA DIVISION.
        ///
        /// If the output files (file-name-3, ...) contain variable-length records, the size
        /// of the records contained in file-name-1 must be neither less than the
        /// smallest record nor greater than the largest record described for the output
        /// files. If the output files contain fixed-length records, the size of the records
        /// contained in file-name-1 must not be greater than the largest record
        /// described for the output files. For more information, see Describing the
        /// output from sorting or merging in the Enterprise COBOL Programming Guide.
        ///
        /// At the time the SORT statement is executed, the output files (file-name-3, ...)
        /// must not be open. For each of the output files, the execution of the SORT
        /// statement causes the following actions to be taken:
        /// * The processing of the file is initiated. The initiation is performed as if an
        /// OPEN statement with the OUTPUT phrase had been executed.
        /// * The sorted logical records are returned and written onto the file. Each
        /// record is written as if a WRITE statement without any optional phrases
        /// had been executed.
        /// For a relative file, the relative key data item for the first record returned
        /// contains the value '1'; for the second record returned, the value '2'. After
        /// execution of the SORT statement, the content of the relative key data
        /// item indicates the last record returned to the file.
        /// * The processing of the file is terminated. The termination is performed as
        /// if a CLOSE statement without optional phrases had been executed.
        ///
        /// These implicit functions are performed such that any associated USE
        /// AFTER EXCEPTION/ERROR procedures are executed; however, the
        /// execution of such a USE procedure must not cause the execution of any
        /// statement manipulating the file referenced by, or accessing the record area
        /// associated with, file-name-3. On the first attempt to write beyond the
        /// externally defined boundaries of the file, any USE AFTER STANDARD
        /// EXCEPTION/ERROR procedure specified for the file is executed. If control
        /// is returned from that USE procedure or if no such USE procedure is
        /// specified, the processing of the file is terminated.
        /// </summary>
        public SymbolReference[] OutputFiles { get; set; }

        /// <summary>
        /// p427:
        /// OUTPUT PROCEDURE phrase
        /// This phrase specifies the name of a procedure that is to select or modify output
        /// records from the sorting operation.
        ///
        /// procedure-name-3
        /// Specifies the first (or only) section or paragraph in the output procedure.
        ///
        /// procedure-name-4
        /// Identifies the last section or paragraph of the output procedure.
        ///
        /// The output procedure can consist of any procedure needed to select,
        /// modify, or copy the records that are made available one at a time by the
        /// RETURN statement in sorted order from the file referenced by file-name-1.
        /// The range includes all statements that are executed as the result of a
        /// transfer of control by CALL, EXIT, GO TO, PERFORM, and XML PARSE
        /// statements in the range of the output procedure. The range also includes
        /// all statements in declarative procedures that are executed as a result of the
        /// execution of statements in the range of the output procedure. The range of
        /// the output procedure must not cause the execution of any MERGE,
        /// RELEASE, or SORT statement.
        ///
        /// If an output procedure is specified, control passes to it after the file
        /// referenced by file-name-1 has been sequenced by the SORT statement. The
        /// compiler inserts a return mechanism at the end of the last statement in the
        /// output procedure and when control passes the last statement in the output
        /// procedure, the return mechanism provides the termination of the sort and
        /// then passes control to the next executable statement after the SORT
        /// statement. Before entering the output procedure, the sort procedure reaches
        /// a point at which it can select the next record in sorted order when
        /// requested. The RETURN statements in the output procedure are the
        /// requests for the next record.
        ///
        /// The INPUT PROCEDURE and OUTPUT PROCEDURE phrases are similar
        /// to those for a basic PERFORM statement. For example, if you name a
        /// procedure in an output procedure, that procedure is executed during the
        /// sorting operation just as if it were named in a PERFORM statement. As
        /// with the PERFORM statement, execution of the procedure is terminated
        /// after the last statement completes execution. The last statement in an input
        /// or output procedure can be the EXIT statement (see “EXIT statement” on
        /// page 335).
        /// </summary>
        public SymbolReference OutputProcedure { get; set; }
        public SymbolReference ThroughOutputProcedure { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, FileNameOrTableName, WithDuplicates, CollatingSequence, 
                   InputProcedure, ThroughInputProcedure, OutputProcedure, ThroughOutputProcedure)
                   && this.ContinueVisitToChildren(astVisitor, SortingKeys, InputFiles, OutputFiles);
        }
    }    
}
