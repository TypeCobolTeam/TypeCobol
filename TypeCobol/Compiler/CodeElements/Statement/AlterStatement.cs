using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p301:
    /// The ALTER statement changes the transfer point specified in a GO TO statement.
    ///
    /// The ALTER statement encourages the use of unstructured programming practices;
    /// the EVALUATE statement provides the same function as the ALTER statement but
    /// helps to ensure that a program is well-structured.
    ///
    /// The ALTER statement acts as a program switch, allowing, for example, one
    /// sequence of execution during initialization and another sequence during the bulk
    /// of file processing.
    ///
    /// Altered GO TO statements in programs with the INITIAL attribute are returned to
    /// their initial states each time the program is entered.
    ///
    /// Do not use the ALTER statement in programs that have the RECURSIVE attribute,
    /// in methods, or in programs compiled with the THREAD option.
    ///
    /// pp301-302:
    /// Segmentation considerations
    ///
    /// A GO TO statement that is coded in an independent segment must not be
    /// referenced by an ALTER statement in a segment with a different priority-number.
    /// All other uses of the ALTER statement are valid and are performed even if the GO
    /// TO referenced by the ALTER statement is in a fixed segment.
    ///
    /// Altered GO TO statements in independent segments are returned to their initial
    /// state when control is transferred to the independent segment that contains the
    /// ALTERED GO TO from another independent segment with a different
    /// priority-number.
    ///
    /// This transfer of control can take place because of:
    /// * The effect of previous statements
    /// * An explicit transfer of control with a PERFORM or GO TO statement
    /// * A sort or merge statement with the INPUT or OUTPUT phrase specified
    /// </summary>
    public class AlterStatement : StatementElement
    {
        public AlterStatement() : base(CodeElementType.AlterStatement, StatementType.AlterStatement) { }

        public AlterGotoInstruction[] AlterGotoInstructions { get; set; }         
    }

    /// <summary>
    /// p301:
    /// Before the ALTER statement is executed, when control reaches the paragraph
    /// specified in procedure-name-1, the GO TO statement transfers control to the
    /// paragraph specified in the GO TO statement. After execution of the ALTER
    /// statement however, the next time control reaches the paragraph specified in
    /// procedure-name-1, the GO TO statement transfers control to the paragraph specified
    /// in procedure-name-2.
    /// </summary>
    public class AlterGotoInstruction
    {
        /// <summary>
        /// p301:
        /// procedure-name-1
        /// Must name a PROCEDURE DIVISION paragraph that contains only one
        /// sentence: a GO TO statement without the DEPENDING ON phrase.
        /// procedure-name-2
        /// Must name a PROCEDURE DIVISION section or paragraph.
        /// </summary>
        public SymbolReference AlteredProcedure;

        /// <summary>
        /// p301:
        /// procedure-name-2
        /// Must name a PROCEDURE DIVISION section or paragraph.
        /// </summary>
        public SymbolReference NewTargetProcedure;
    }
}
