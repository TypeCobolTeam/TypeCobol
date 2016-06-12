using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p330:
    /// The ENTRY statement establishes an alternate entry point into a COBOL called
    /// subprogram.
    ///
    /// The ENTRY statement cannot be used in:
    /// * Programs that specify a return value using the PROCEDURE DIVISION
    /// RETURNING phrase. For details, see the discussion of the RETURNING phrase
    /// under “The PROCEDURE DIVISION header” on page 247.
    /// * Nested program. See “Nested programs” on page 85 for a description of nested
    /// programs.
    ///
    /// When a CALL statement that specifies the alternate entry point is executed in a
    /// calling program, control is transferred to the next executable statement following
    /// the ENTRY statement.
    ///
    /// Execution of the called program begins at the first executable statement following
    /// the ENTRY statement whose literal corresponds to the literal or identifier specified
    /// in the CALL statement.
    ///
    /// The entry point name on the ENTRY statement can be affected by the PGMNAME
    /// compiler option. For details, see PGMNAME in the Enterprise COBOL Programming
    /// Guide.
    /// </summary>
    public class EntryStatement : StatementElement
    {
        public EntryStatement() : base(CodeElementType.EntryStatement, StatementType.EntryStatement)
        { }

        /// <summary>
        /// p330:
        /// literal-1
        /// Must be an alphanumeric literal that conform to the rules for the formation
        /// of a program-name in an outermost program (see “PROGRAM-ID
        /// paragraph” on page 100).
        ///
        /// Must not match the program-ID or any other ENTRY literal in this
        /// program.
        ///
        /// Must not be a figurative constant.
        /// </summary>
        public Literal ProgramName;

        /// <summary>
        /// p330:
        /// USING phrase
        /// For a discussion of the USING phrase, see “The PROCEDURE DIVISION header”
        /// on page 247.
        /// </summary>
        public IList<Using<Identifier>> Usings = new List<Using<Identifier>>();



        /// <summary>
        /// pp248-249:
        /// The USING phrase
        /// The USING phrase specifies the parameters that a program or method receives
        /// when the program is called or the method is invoked.
        ///
        /// The USING phrase is valid in the PROCEDURE DIVISION header of a called
        /// subprogram or invoked method entered at the beginning of the nondeclaratives
        /// portion. Each USING identifier must be defined as a level-01 or level-77 item in the
        /// LINKAGE SECTION of the called subprogram or invoked method.
        ///
        /// In a called subprogram entered at the first executable statement following an
        /// ENTRY statement, the USING phrase is valid in the ENTRY statement. Each
        /// USING identifier must be defined as a level-01 or level-77 item in the LINKAGE
        /// SECTION of the called subprogram.
        ///
        /// However, a data item specified in the USING phrase of the CALL statement can be
        /// a data item of any level in the DATA DIVISION of the calling COBOL program or
        /// method. A data item specified in the USING phrase of an INVOKE statement can
        /// be a data item of any level in the DATA DIVISION of the invoking COBOL
        /// program or method.
        ///
        /// A data item in the USING phrase of the header can have a REDEFINES clause in
        /// its data description entry.
        ///
        /// It is possible to call COBOL programs from non-COBOL programs or to pass user
        /// parameters from a system command to a COBOL main program. COBOL methods
        /// can be invoked only from Java or COBOL.
        ///
        /// The order of appearance of USING identifiers in both calling and called
        /// subprograms, or invoking methods or programs and invoked methods, determines
        /// the correspondence of single sets of data available to both. The correspondence is
        /// positional and not by name. For calling and called subprograms, corresponding
        /// identifiers must contain the same number of bytes although their data descriptions
        /// need not be the same.
        ///
        /// For index-names, no correspondence is established. Index-names in calling and
        /// called programs, or invoking method or program and invoked methods, always
        /// refer to separate indexes.
        ///
        /// The identifiers specified in a CALL USING or INVOKE USING statement name the
        /// data items available to the calling program or invoking method or program that
        /// can be referred to in the called program or invoked method. These items can be
        /// defined in any DATA DIVISION section.
        ///
        /// A given identifier can appear more than once in a USING phrase. The last value
        /// passed to it by a CALL or INVOKE statement is used.
        ///
        /// The BY REFERENCE or BY VALUE phrase applies to all parameters that follow
        /// until overridden by another BY REFERENCE or BY VALUE phrase.
        /// </summary>
        public class Using<T>
        {
            public IList<T> Items = new List<T>();

            public void Add(T item) { Items.Add(item); }

            /// <summary>
            /// p249:
            /// BY REFERENCE (for programs only)
            /// When an argument is passed BY CONTENT or BY REFERENCE, BY
            /// REFERENCE must be specified or implied for the corresponding formal
            /// parameter on the PROCEDURE or ENTRY USING phrase.
            ///
            /// BY REFERENCE is the default if neither BY REFERENCE nor BY VALUE is
            /// specified.
            ///
            /// If the reference to the corresponding data item in the CALL statement
            /// declares the parameter to be passed BY REFERENCE (explicit or implicit),
            /// the program executes as if each reference to a USING identifier in the
            /// called subprogram is replaced by a reference to the corresponding USING
            /// identifier in the calling program.
            ///
            /// If the reference to the corresponding data item in the CALL statement
            /// declares the parameter to be passed BY CONTENT, the value of the item is
            /// moved when the CALL statement is executed and placed into a
            /// system-defined storage item that possesses the attributes declared in the
            /// LINKAGE SECTION for data-name-1. The data description of each
            /// parameter in the BY CONTENT phrase of the CALL statement must be the
            /// same, meaning no conversion or extension or truncation, as the data
            /// description of the corresponding parameter in the USING phrase of the
            /// header.
            /// </summary>
            public bool ByReference { get { return !ByValue; } }

            /// <summary>
            /// p249:
            /// BY VALUE
            /// When an argument is passed BY VALUE, the value of the argument is
            /// passed, not a reference to the sending data item. The receiving subprogram
            /// or method has access only to a temporary copy of the sending data item.
            /// Any modifications made to the formal parameters that correspond to an
            /// argument passed BY VALUE do not affect the argument.
            ///
            /// Parameters specified in the USING phrase of a method procedure division
            /// header must be passed to the method BY VALUE. See Passing data in the
            /// Enterprise COBOL Programming Guide for examples that illustrate these
            /// concepts.
            /// </summary>
            public bool ByValue = false;
        }
    }
}
