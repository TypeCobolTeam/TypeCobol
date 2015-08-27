using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// p339:
    /// The GO TO statement transfers control from one part of the PROCEDURE DIVISION to another.
    /// The types of GO TO statements are:
    /// * Unconditional
    /// * Conditional
    /// * Altered
    /// </summary>
    public class GotoStatement : CodeElement
    {
        /// <summary>
        /// p339:
        /// procedure-name-1 (Unconditional)
        /// Must name a procedure or a section in the same PROCEDURE DIVISION
        /// as the GO TO statement.
        ///
        /// p340:
        /// procedure-name-1 (Conditional)
        /// Must be a procedure or a section in the same PROCEDURE DIVISION as
        /// the GO TO statement. The number of procedure-names must not exceed
        /// 255.
        /// </summary>
        public IList<QualifiedProcedureName> Procedures = new List<QualifiedProcedureName>();

        /// <summary>
        /// p340:
        /// identifier-1
        /// Must be a numeric elementary data item that is an integer.
        ///
        /// If 1, control is transferred to the first statement in the procedure named by
        /// the first occurrence of procedure-name-1.
        ///
        /// If 2, control is transferred to the first statement in the procedure named by
        /// the second occurrence of procedure-name-1, and so forth.
        ///
        /// If the value of identifier is anything other than a value within the range of
        /// 1 through n (where n is the number of procedure-names specified in this
        /// GO TO statement), no control transfer occurs. Instead, control passes to the
        /// next statement in the normal sequence of execution.
        /// </summary>
        public Identifier DependingOn = null;

        /// <summary>
        /// p339:
        /// The unconditional GO TO statement transfers control to the first statement in the
        /// paragraph or section identified by procedure-name, unless the GO TO statement
        /// has been modified by an ALTER statement.
        ///
        /// When the unconditional GO TO statement is not the last statement in a sequence
        /// of imperative statements, the statements following the GO TO are not executed.
        ///
        /// When a paragraph is referred to by an ALTER statement, the paragraph must
        /// consist of a paragraph-name followed by an unconditional or altered GO TO
        /// statement.
        ///
        /// For more information, see “ALTER statement” on page 301.
        /// </summary>
        public bool IsUnconditional { get { return this.Procedures.Count == 1 && this.DependingOn == null; } }

        /// <summary>
        /// p339:
        /// The conditional GO TO statement transfers control to one of a series of procedures,
        /// depending on the value of the data item referenced by identifier-1.
        /// </summary>
        public bool IsConditional { get { return this.Procedures.Count > 0; } }

        /// <summary>
        /// p340:
        /// The altered GO TO statement transfers control to the first statement of the
        /// paragraph named in the ALTER statement.
        ///
        /// You cannot specify the altered GO TO statement in the following cases:
        /// * A program or method that has the RECURSIVE attribute
        /// * A program compiled with the THREAD compiler option
        /// An ALTER statement referring to the paragraph that contains the altered GO TO
        /// statement should be executed before the GO TO statement is executed. Otherwise,
        /// the GO TO statement acts like a CONTINUE statement.
        ///
        /// When an ALTER statement refers to a paragraph, the paragraph can consist only of
        /// the paragraph-name followed by an unconditional or altered GO TO statement.
        /// </summary>
        public bool IsAltered { get { return this.Procedures.Count == 0 && this.DependingOn == null; } }

        public GotoStatement() : base(CodeElementType.GotoStatement) { }
    }
}
