using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p546:
    /// The USE statement defines the conditions under which the procedures that follow
    /// the statement will be executed.
    /// The formats for the USE statement are:
    /// * EXCEPTION/ERROR declarative
    /// * DEBUGGING declarative
    ///
    /// For general information about declaratives, see “Declaratives” on page 251.
    ///
    /// p547:
    /// The EXCEPTION/ERROR declarative specifies procedures for input/output
    /// exception or error handling that are to be executed in addition to the standard
    /// system procedures.
    ///
    /// The words EXCEPTION and ERROR are synonymous and can be used interchangeably.
    ///
    /// p548:
    /// The EXCEPTION/ERROR procedure is executed:
    /// * Either after completing the system-defined input/output error routine, or
    /// * Upon recognition of an INVALID KEY or AT END condition when an INVALID
    /// KEY or AT END phrase has not been specified in the input/output statement, or
    /// * Upon recognition of an IBM-defined condition that causes file status key 1 to be
    /// set to 9. (See “File status key” on page 287.)
    ///
    /// After execution of the EXCEPTION/ERROR procedure, control is returned to the
    /// invoking routine in the input/output control system. If the input/output status
    /// value does not indicate a critical input/output error, the input/output control
    /// system returns control to the next executable statement following the input/output
    /// statement whose execution caused the exception.
    ///
    /// An applicable EXCEPTION/ERROR procedure is activated when an input/output
    /// error occurs during execution of a READ, WRITE, REWRITE, START, OPEN,
    /// CLOSE, or DELETE statement. To determine what conditions are errors, see
    /// “Common processing facilities” on page 286.
    ///
    /// The following rules apply to declarative procedures:
    /// * A declarative procedure can be performed from a nondeclarative procedure.
    /// * A nondeclarative procedure can be performed from a declarative procedure.
    /// * A declarative procedure can be referenced in a GO TO statement in a declarative
    /// procedure.
    /// * A nondeclarative procedure can be referenced in a GO TO statement in a
    /// declarative procedure.
    ///
    /// You can include a statement that executes a previously called USE procedure that
    /// is still in control. However, to avoid an infinite loop, you must be sure that there is
    /// an eventual exit at the bottom.
    ///
    /// You cannot use a GOBACK statement or a STOP RUN statement when an
    /// EXCEPTION/ERROR declarative is active due to a QSAM abend for a READ,
    /// WRITE, or REWRITE statement. You cannot use an EXIT PROGRAM statement in
    /// a non-nested subprogram when an EXCEPTION/ERROR declarative is active due
    /// to a QSAM abend for a READ, WRITE, or REWRITE statement. When a QSAM
    /// abend occurs during a READ, WRITE, or REWRITE statement, the file status code
    /// can be "34" or "90".
    ///
    /// You cannot use a GOBACK statement or an EXIT PROGRAM statement while a
    /// declarative is active in a nested program. You cannot use a GOBACK statement or
    /// an EXIT METHOD statement while a declarative is active in a method.
    /// EXCEPTION/ERROR procedures can be used to check the file status key values
    /// whenever an input/output error occurs.
    ///
    /// pp548-549:
    /// Precedence rules for nested programs
    /// Special precedence rules are followed when programs are contained within other
    /// programs.
    /// 
    /// In applying these rules, only the first qualifying declarative is selected for
    /// execution. The order of precedence for selecting a declarative is:
    /// 1. A file-specific declarative (that is, a declarative of the form USE AFTER ERROR
    /// ON file-name-1) within the program that contains the statement that caused the
    /// qualifying condition.
    /// 2. A mode-specific declarative (that is, a declarative of the form USE AFTER
    /// ERROR ON INPUT) within the program that contains the statement that
    /// caused the qualifying condition.
    /// 3. A file-specific declarative that specifies the GLOBAL phrase and is within the
    /// program directly containing the program that was last examined for a
    /// qualifying declarative.
    /// 4. A mode-specific declarative that specifies the GLOBAL phrase and is within the
    /// program directly containing the program that was last examined for a
    /// qualifying condition.
    /// 
    /// Steps 3 and 4 are repeated until the last examined program is the outermost
    /// program, or until a qualifying declarative has been found.
    /// </summary>
    public class UseErrorsStatement : CodeElement
    {
        /// <summary>
        /// p547:
        /// file-name-1
        /// Valid for all files. When this option is specified, the procedure is executed
        /// only for the files named. No file-name can refer to a sort or merge file. For
        /// any given file, only one EXCEPTION/ERROR procedure can be specified;
        /// thus, file-name specification must not cause simultaneous requests for
        /// execution of more than one EXCEPTION/ERROR procedure.
        ///
        /// A USE AFTER EXCEPTION/ERROR declarative statement specifying the
        /// name of a file takes precedence over a declarative statement specifying the
        /// open mode of the file.
        /// </summary>
        public IList<SymbolReference<FileName>> FileNames = new List<SymbolReference<FileName>>();

        /// <summary>
        /// p547:
        /// INPUT
        /// Valid for all files. When this option is specified, the procedure is executed
        /// for all files opened in INPUT mode or in the process of being opened in
        /// INPUT mode that get an error.
        ///
        /// OUTPUT
        /// Valid for all files. When this option is specified, the procedure is executed
        /// for all files opened in OUTPUT mode or in the process of being opened in
        /// OUTPUT mode that get an error.
        ///
        /// I-O
        /// Valid for all direct-access files. When this option is specified, the procedure
        /// is executed for all files opened in I-O mode or in the process of being
        /// opened in I-O mode that get an error.
        ///
        /// EXTEND
        /// Valid for all files. When this option is specified, the procedure is executed
        /// for all files opened in EXTEND mode or in the process of being opened in
        /// EXTEND mode that get an error.
        /// </summary>
        public OpenMode Mode;

        public UseErrorsStatement() : base(CodeElementType.UseStatement) { }
    }

    /// <summary>
    /// p546:
    /// The USE statement defines the conditions under which the procedures that follow
    /// the statement will be executed.
    /// The formats for the USE statement are:
    /// * EXCEPTION/ERROR declarative
    /// * DEBUGGING declarative
    ///
    /// For general information about declaratives, see “Declaratives” on page 251.
    ///
    /// p549:
    /// Debugging sections are permitted only in the outermost program; they are not
    /// valid in nested programs. Debugging sections are never triggered by procedures
    /// contained in nested programs.
    /// Debugging sections are not permitted in:
    /// * A method
    /// * A program defined with the recursive attribute
    /// * A program compiled with the THREAD compiler option
    ///
    /// The WITH DEBUGGING MODE clause of the SOURCE-COMPUTER paragraph
    /// activates all debugging sections and lines that have been compiled into the object
    /// code. See Appendix D, “Source language debugging,” on page 577 for additional
    /// details.
    ///
    /// When the debugging mode is suppressed by not specifying the WITH
    /// DEBUGGING MODE clause, all USE FOR DEBUGGING declarative procedures
    /// and all debugging lines are inhibited.
    ///
    /// Automatic execution of a debugging section is not caused by a statement that
    /// appears in a debugging section.
    ///
    /// p549:
    /// USE FOR DEBUGGING
    /// All debugging statements must be written together in a section
    /// immediately after the DECLARATIVES header.
    /// Except for the USE FOR DEBUGGING sentence itself, within the
    /// debugging procedure there must be no reference to any nondeclarative
    /// procedures.
    /// </summary>
    public class UseDebuggingStatement : CodeElement
    {
        /// <summary>
        /// p550:
        /// procedure-name-1
        /// Must not be defined in a debugging session.
        ///
        /// Table 55 [p550] shows, for each valid option, the points during execution when
        /// the USE FOR DEBUGGING procedures are executed.
        ///
        /// Any given procedure-name can appear in only one USE FOR
        /// DEBUGGING sentence, and only once in that sentence. All procedures
        /// must appear in the outermost program.
        /// </summary>
        public IList<QualifiedProcedureName> procedures = new List<QualifiedProcedureName>();

        /// <summary>
        /// p550:
        /// ALL PROCEDURES
        /// procedure-name-1 must not be specified in any USE FOR DEBUGGING
        /// sentences. The ALL PROCEDURES phrase can be specified only once in a
        /// program. Only the procedures contained in the outermost program will
        /// trigger execution of the debugging section.
        /// </summary>
        public bool AllProcedures = false;

        public UseDebuggingStatement() : base(CodeElementType.UseStatement) { }
    }
}
