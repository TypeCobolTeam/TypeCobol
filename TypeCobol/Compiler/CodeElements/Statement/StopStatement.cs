using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p432:
    /// The STOP statement halts execution of the object program either permanently or
    /// temporarily.
    /// </summary>
    public class StopStatement : StatementElement
    {
        public StopStatement() : base(CodeElementType.StopStatement, StatementType.StopStatement)
        { }

        /// <summary>
        /// p432:
        /// literal
        /// Can be a fixed-point numeric literal (signed or unsigned) or an
        /// alphanumeric literal. It can be any figurative constant except ALL literal.
        ///
        /// When STOP literal is specified, the literal is communicated to the operator, and
        /// object program execution is suspended. Program execution is resumed only after
        /// operator intervention, and continues at the next executable statement in sequence.
        ///
        /// The STOP literal statement is useful for special situations when operator
        /// intervention is needed during program execution; for example, when a special tape
        /// or disk must be mounted or a specific daily code must be entered. However, the
        /// ACCEPT and DISPLAY statements are preferred when operator intervention is
        /// needed.
        ///
        /// Do not use the STOP literal statement in programs compiled with the THREAD
        /// compiler option.
        /// </summary>
        public NumericValue ReturnCode { get; set; }
        public AlphanumericValue ReturnMessage { get; set; }

        /// <summary>
        /// p432:
        /// When STOP RUN is specified, execution is terminated and control is returned to
        /// the system. When STOP RUN is not the last or only statement in a sequence of
        /// imperative statements within a sentence, the statements following STOP RUN are
        /// not executed.
        ///
        /// The STOP RUN statement closes all files defined in any of the programs in the run
        /// unit.
        ///
        /// STOP RUN in Main program returns to the calling program.
        /// (Can be the system, which causes the application to end.)
        ///
        /// STOP RUN in Subprogram returns directly to the program that called the main program.
        /// (Can be the system, which causes the application to end.)
        /// </summary>
        public SyntaxProperty<bool> StopRun { get; set; }

    }
}