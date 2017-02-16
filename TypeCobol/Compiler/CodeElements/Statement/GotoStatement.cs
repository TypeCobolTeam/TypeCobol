using System.Collections.Generic;

namespace TypeCobol.Compiler.CodeElements {
/// <summary>
/// p339:
/// The GO TO statement transfers control from one part of the PROCEDURE DIVISION to another.
/// The types of GO TO statements are:
// * Unconditional
/// * Conditional
/// * Altered
/// </summary>
public abstract class GotoStatement: StatementElement {
    protected GotoStatement(StatementType statementType) : base(CodeElementType.GotoStatement, statementType) { }
}

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
public class GotoSimpleStatement: GotoStatement {
	public GotoSimpleStatement() : base(StatementType.GotoSimpleStatement) { }

	/// <summary>
	/// p339:
	/// procedure-name-1 (Unconditional)
	/// Must name a procedure or a section in the same PROCEDURE DIVISION
	/// as the GO TO statement.
	/// </summary>
	public SymbolReference ProcedureName { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, ProcedureName);
        }

        public override string ToString() {
		return "GOTO "+(ProcedureName!=null? ProcedureName.ToString():"?");
	}
}

/// <summary>
/// p339:
/// The conditional GO TO statement transfers control to one of a series of procedures,
/// depending on the value of the data item referenced by identifier-1.
/// </summary>
public class GotoConditionalStatement: GotoStatement {
	public GotoConditionalStatement() : base(StatementType.GotoConditionalStatement) { }

	/// <summary>
	/// p340:
	/// procedure-name-1 (Conditional)
	/// Must be a procedure or a section in the same PROCEDURE DIVISION as
	/// the GO TO statement. The number of procedure-names must not exceed
	/// 255.
	/// </summary>
	public SymbolReference[] ProcedureNames { get; set; }

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
	public Variable DependingOn { get; set; }

        public override bool VisitCodeElement(IASTVisitor astVisitor)
        {
            return base.VisitCodeElement(astVisitor) && astVisitor.Visit(this)
                   && this.ContinueVisitToChildren(astVisitor, (IEnumerable<IVisitable>)ProcedureNames)
                   && this.ContinueVisitToChildren(astVisitor, DependingOn);
        }

        public override string ToString() {
		var str = new System.Text.StringBuilder("GOTO");
		if (ProcedureNames.Length < 1) str.Append(" ?");
		foreach(var name in ProcedureNames) str.Append(' ').Append(name.ToString());
		if (DependingOn != null) str.Append(" DEPENDING ON ").Append(DependingOn.ToString());
		return str.ToString();
	}
}

}
