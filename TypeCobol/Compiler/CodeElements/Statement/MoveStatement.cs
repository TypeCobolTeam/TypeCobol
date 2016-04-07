using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.CodeElements
{
	/// <summary>
	/// p369: The MOVE statement transfers data from one area of storage to one or more other areas.
	/// </summary>
	public class MoveStatement : CodeElement, SymbolUser, SymbolWriter
	{
		/// <summary>
		/// identifier-1 , literal-1
		/// The sending area.
		/// </summary>
		public Expression Sending;
		/// <summary>
		/// identifier-2
		/// The receiving areas. identifier-2 must not reference an intrinsic function.
		/// </summary>
		private IList<Identifier> Receiving;
		/// <summary>
		/// CORR is an abbreviation for, and is equivalent to, CORRESPONDING.
		///
		/// When format 1 (no CORRESPONDING) is specified :
		/// * All identifiers can reference alphanumeric group items, national group items, or
		/// elementary items.
		/// * When one of identifier-1 or identifier-2 references a national group item and the
		/// other operand references an alphanumeric group item, the national group is
		/// processed as a group item; in all other cases, the national group item is
		/// processed as an elementary data item of category national.
		/// * The data in the sending area is moved into the data item referenced by each
		/// identifier-2 in the order in which the identifier-2 data items are specified in the
		/// MOVE statement. See “Elementary moves” on page 370 and “Group moves” on page 374 below.
		///
		/// When format 2 (with CORRESPONDING) is specified:
		/// * Both identifiers must be group items.
		/// * A national group item is processed as a group item (and not as an elementary
		/// data item of category national).
		/// * Selected items in identifier-1 are moved to identifier-2 according to the rules for
		/// the “CORRESPONDING phrase” on page 281. The results are the same as if
		/// each pair of CORRESPONDING identifiers were referenced in a separate MOVE
		/// statement.
		/// </summary>
		private bool IsCorresponding;

		public MoveStatement(Expression sending, IList<Identifier> receiving, bool corresponding)
			: base(CodeElementType.MoveStatement) {
			this.Sending   = sending;
			this.Receiving = receiving;
			this.IsCorresponding = corresponding;
		}

		ICollection<QualifiedName> SymbolUser.Symbols {
			get {
				var symbols = new List<QualifiedName>();
				var name = asUserDefinedName(Sending);
				if (name != null) symbols.Add(name);
				foreach (var identifier in Receiving) {
					name = asUserDefinedName(identifier);
					if (name != null) symbols.Add(name);
				}
				return symbols;
			}
		}
		ICollection<System.Tuple<QualifiedName,QualifiedName>> SymbolWriter.Symbols {
			get {
				var list = new List<System.Tuple<QualifiedName,QualifiedName>>();
				var sending = asUserDefinedName(Sending);
				foreach(var r in Receiving) {
					var receiving = asUserDefinedName(r);
					list.Add(new System.Tuple<QualifiedName,QualifiedName>(sending, receiving));
				}
				return list;
			}
		}
		private QualifiedName asUserDefinedName(Expression expression) {
			if (!(expression is Identifier)) return null;
			if (expression is SpecialRegister) return null;
			if (expression is FunctionReference) return null;
			return ((Identifier)expression).Name;
		}

		public bool IsUnsafe { get; set; }
	}
}
