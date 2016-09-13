using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p311:
    /// The CANCEL statement ensures that the referenced subprogram is entered in
    /// initial state the next time that it is called.
    ///
    /// After a CANCEL statement for a called subprogram has been executed, that
    /// subprogram no longer has a logical connection to the program. The contents of
    /// data items in external data records described by the subprogram are not changed
    /// when that subprogram is canceled. If a CALL statement is executed later by any
    /// program in the run unit naming the same subprogram, that subprogram is entered
    /// in its initial state.
    ///
    /// When a CANCEL statement is executed, all programs contained within the
    /// program referenced in the CANCEL statement are also canceled. The result is the
    /// same as if a valid CANCEL were executed for each contained program in the
    /// reverse order in which the programs appear in the separately compiled program.
    /// A CANCEL statement closes all open files that are associated with an internal file
    /// connector in the program named in an explicit CANCEL statement. USE
    /// procedures associated with those files are not executed.
    /// </summary>
    public class CancelStatement : CodeElement
    {
        /// <summary>
        /// p311:
        /// identifier-1, literal-1
        /// literal-1 must be an alphanumeric literal. identifier-1 must be an
        /// alphanumeric, alphabetic, or zoned decimal data item such that its value
        /// can be a program-name. The rules of formation for program-names are
        /// dependent on the PGMNAME compiler option. For details, see the
        /// discussion of program-names in “PROGRAM-ID paragraph” on page 100
        /// and the description of PGMNAME in the Enterprise COBOL Programming
        /// Guide.
        ///
        /// literal-1 or the contents of identifier-1 must be the same as a literal or the
        /// contents of an identifier specified in an associated CALL statement.
        /// Do not specify the name of a class or a method in the CANCEL statement.
        /// </summary>
        public IList<Expression> Items = new List<Expression>();

        public CancelStatement() : base(CodeElementType.CancelStatement) { }
    }
}
