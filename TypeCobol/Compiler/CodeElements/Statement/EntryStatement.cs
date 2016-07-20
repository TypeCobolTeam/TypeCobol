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
        public SymbolDefinition ProgramEntry { get; set; }

        /// <summary>
        /// The USING phrase specifies the parameters that a program or method receives
        /// when the program is called or the method is invoked.
        /// Each USING identifier must be defined as a level-01 or level-77 item in the
        /// LINKAGE SECTION of the called subprogram or invoked method.
        /// The argument receiving mode can be : BY REFERENCE or BY VALUE
        /// </summary>
        public IList<ProgramInputParameter> InputParameters { get; set; }
    }
}
