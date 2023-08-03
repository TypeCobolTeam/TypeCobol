using System.Collections.Generic;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// The CALL statement transfers control from one object program to another within
    /// the run unit.
    ///
    /// The program containing the CALL statement is the calling program; the program
    /// identified in the CALL statement is the called subprogram. Called programs can
    /// contain CALL statements; however, only programs defined with the RECURSIVE
    /// clause can execute a CALL statement that directly or indirectly calls itself.
    /// </summary>
    public class CallStatement : StatementElement
    {
        public CallStatement() : base(CodeElementType.CallStatement, StatementType.CallStatement)
        { }

        /// <summary>
        /// p304:
        /// identifier-1, literal-1
        /// literal-1 must be an alphanumeric literal. identifier-1 must be an
        /// alphanumeric, alphabetic, or numeric data item described with USAGE
        /// DISPLAY such that its value can be a program-name.
        /// The rules of formation for program-names are dependent on the
        /// PGMNAME compiler option. For details, see the discussion of
        /// 304 Enterprise COBOL for z/OS, V5.1.1 Language Reference
        /// program-names in “PROGRAM-ID paragraph” on page 100 and also the
        /// description of PGMNAME in the Enterprise COBOL Programming Guide.
        /// Usage note: Do not specify the name of a class or method in the CALL
        /// statement.
        ///
        /// p305:
        /// When the called subprogram is to be entered at the beginning of the PROCEDURE
        /// DIVISION, literal-1 or the contents of identifier-1 must specify the program-name of
        /// the called subprogram.
        /// When the called subprogram is entered through an ENTRY statement, literal-1 or
        /// the contents of identifier-1 must be the same as the name specified in the called
        /// subprogram's ENTRY statement.
        /// For information about how the compiler resolves calls to program-names found in
        /// multiple programs, see “Conventions for program-names” on page 86.
        ///
        /// p305:
        /// procedure-pointer-1
        /// Must be defined with USAGE IS PROCEDURE-POINTER and must be set
        /// to a valid program entry point; otherwise, the results of the CALL
        /// statement are undefined.
        /// After a program has been canceled by COBOL, released by PL/I or C, or
        /// deleted by assembler, any procedure-pointers that had been set to that
        /// program's entry point are no longer valid.
        ///
        /// p305:
        /// function-pointer-1
        /// Must be defined with USAGE IS FUNCTION-POINTER and must be set to
        /// a valid function or program entry point; otherwise, the results of the CALL
        /// statement are undefined.
        /// After a program has been canceled by COBOL, released by PL/I or C, or
        /// deleted by the assembler, any function-pointers that had been set to that
        /// function or program's entry point are no longer valid.
        /// </summary>
        public SymbolReferenceVariable ProgramOrProgramEntryOrProcedureOrFunction { get; set; }

        /// <summary>
        /// p305:
        /// The USING phrase specifies arguments that are passed to the target program.
        /// Include the USING phrase in the CALL statement only if there is a USING phrase
        /// in the PROCEDURE DIVISION header or the ENTRY statement through which the
        /// called program is run. The number of operands in each USING phrase must be
        /// identical.
        /// For more information about the USING phrase, see “The PROCEDURE DIVISION
        /// header” on page 247.
        /// The sequence of the operands in the USING phrase of the CALL statement and in
        /// the corresponding USING phrase in the called subprogram's PROCEDURE
        /// DIVISION header or ENTRY statement determines the correspondence between the
        /// operands used by the calling and called programs. This correspondence is
        /// positional.
        /// The values of the parameters referenced in the USING phrase of the CALL
        /// statement are made available to the called subprogram at the time the CALL
        /// statement is executed. The description of the data items in the called program must
        /// describe the same number of character positions as the description of the
        /// corresponding data items in the calling program.
        /// </summary>
        public IList<CallSiteParameter> InputParameters { get; set; }

        /// <summary>
        /// pp308-309:
        /// RETURNING phrase
        /// identifier-5
        /// The RETURNING data item, which can be any data item defined in the
        /// DATA DIVISION. The return value of the called program is implicitly
        /// stored into identifier-5.
        ///
        /// You can specify the RETURNING phrase for calls to functions written in COBOL,
        /// C, or in other programming languages that use C linkage conventions. If you
        /// specify the RETURNING phrase on a CALL to a COBOL subprogram:
        ///
        /// * The called subprogram must specify the RETURNING phrase on its
        /// PROCEDURE DIVISION header.
        ///
        /// * identifier-5 and the corresponding PROCEDURE DIVISION RETURNING
        /// identifier in the target program must have the same PICTURE, USAGE, SIGN,
        /// SYNCHRONIZE, JUSTIFIED, and BLANK WHEN ZERO clauses (except that
        /// PICTURE clause currency symbols can differ, and periods and commas can be
        /// interchanged due to the DECIMAL POINT IS COMMA clause).
        /// When the target returns, its return value is assigned to identifier-5 using the rules
        /// for the SET statement if identifier-6 is of usage INDEX, POINTER,
        /// FUNCTION-POINTER, PROCEDURE-POINTER, or OBJECT REFERENCE.
        /// When identifier-5 is of any other usage, the rules for the MOVE statement are
        /// used.
        ///
        /// The CALL ... RETURNING data item is an output-only parameter. On entry to the
        /// called program, the initial state of the PROCEDURE DIVISION RETURNING data
        /// item has an undefined and unpredictable value. You must initialize the
        /// PROCEDURE DIVISION RETURNING data item in the called program before you
        /// reference its value. The value that is passed back to the calling program is the final
        /// value of the PROCEDURE DIVISION RETURNING data item when the called
        /// program returns.
        ///
        /// Note: If a COBOL program returns a doubleword binary item via a PROCEDURE
        /// DIVISION RETURNING header to a calling COBOL program with a CALL ...
        /// RETURNING statement, an issue occurs if only one of the programs is recompiled
        /// with Enterprise COBOL V5. Both the called and calling programs must be
        /// recompiled with Enterprise COBOL V5 together, so that the linkage convention for
        /// the RETURNING item is consistent.
        ///
        /// If an EXCEPTION or OVERFLOW occurs, identifier-5 is not changed. identifier-5
        /// must not be reference-modified.
        ///
        /// The RETURN-CODE special register is not set by execution of CALL statements
        /// that include the RETURNING phrase.
        /// </summary>
        public CallSiteParameter OutputParameter { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, ProgramOrProgramEntryOrProcedureOrFunction, OutputParameter)
                   && this.ContinueVisitToChildren(astVisitor, InputParameters);
        }
    }



    public class ProcedureStyleCallStatement : StatementElement
    {
        public ProcedureCall ProcedureCall { get; private set; }

        /// <summary>
        /// p304:
        /// identifier-1, literal-1
        /// literal-1 must be an alphanumeric literal. identifier-1 must be an
        /// alphanumeric, alphabetic, or numeric data item described with USAGE
        /// DISPLAY such that its value can be a program-name.
        /// The rules of formation for program-names are dependent on the
        /// PGMNAME compiler option. For details, see the discussion of
        /// 304 Enterprise COBOL for z/OS, V5.1.1 Language Reference
        /// program-names in “PROGRAM-ID paragraph” on page 100 and also the
        /// description of PGMNAME in the Enterprise COBOL Programming Guide.
        /// Usage note: Do not specify the name of a class or method in the CALL
        /// statement.
        ///
        /// p305:
        /// When the called subprogram is to be entered at the beginning of the PROCEDURE
        /// DIVISION, literal-1 or the contents of identifier-1 must specify the program-name of
        /// the called subprogram.
        /// When the called subprogram is entered through an ENTRY statement, literal-1 or
        /// the contents of identifier-1 must be the same as the name specified in the called
        /// subprogram's ENTRY statement.
        /// For information about how the compiler resolves calls to program-names found in
        /// multiple programs, see “Conventions for program-names” on page 86.
        ///
        /// p305:
        /// procedure-pointer-1
        /// Must be defined with USAGE IS PROCEDURE-POINTER and must be set
        /// to a valid program entry point; otherwise, the results of the CALL
        /// statement are undefined.
        /// After a program has been canceled by COBOL, released by PL/I or C, or
        /// deleted by assembler, any procedure-pointers that had been set to that
        /// program's entry point are no longer valid.
        ///
        /// p305:
        /// function-pointer-1
        /// Must be defined with USAGE IS FUNCTION-POINTER and must be set to
        /// a valid function or program entry point; otherwise, the results of the CALL
        /// statement are undefined.
        /// After a program has been canceled by COBOL, released by PL/I or C, or
        /// deleted by the assembler, any function-pointers that had been set to that
        /// function or program's entry point are no longer valid.
        /// </summary>
        public SymbolReference ProgramOrProgramEntryOrProcedureOrFunctionOrTCProcedureFunction { get; set; }
        public SymbolReference ProgramNameOrProgramEntry { get; set; }
        public SymbolReference ProcdurePointerOrTCProcedureFunction { get; set; }


        //SMEDILOL: idea use 2 SymbolReference split per CandidatesType
        //Both are ambiguous, but less ambiguous than one
        //But this can be a bad idea, because code that use CallStatement will now have to deal with 2 variables
        //So try it, but don't hesitate to remove if it's a bad idea

        //public SymbolReferenceVariable ProgramNameOrProgramEntry { get; set; }
        //public SymbolReferenceVariable ProcdurePointerOrTCProcedureFunction { get; set; }

    public ProcedureStyleCallStatement(ProcedureCall call)
        : base(CodeElementType.ProcedureStyleCall, StatementType.CallStatement)
    {
        this.ProcedureCall = call;
    }
        public ProcedureStyleCallStatement()
        : base(CodeElementType.ProcedureStyleCall, StatementType.CallStatement) { }

        public override bool VisitCodeElement(IASTVisitor astVisitor) {
            if (!base.VisitCodeElement(astVisitor))
                return false;
            if (!astVisitor.Visit(this))
                return false;
            return this.ContinueVisitToChildren(astVisitor,
                       ProgramOrProgramEntryOrProcedureOrFunctionOrTCProcedureFunction, ProcedureCall);
        }
    }

}
