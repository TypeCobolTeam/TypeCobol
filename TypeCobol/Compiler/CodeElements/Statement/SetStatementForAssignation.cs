using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Compiler.CodeElements
{
	class SetStatementForAssignation : SetStatement, IdentifierUser, SymbolWriter
	{
		/// <summary>
		/// index-name, identifier(numeric integer item), pointer, procedure-pointer, function-pointer, object reference id
		/// </summary>
		public List<Expression> Receiving { get; set; }
		/// <summary>
		/// index-name, identifier, positive integer, address of, null, nulls, entry identifier|literal, object reference id, pointer,
		/// procedure-pointer, function-pointer,
		/// </summary>
		public Expression Sending { get; set; }

		public override string ToString()
		{
			if (Receiving == null && Sending == null)
			{
				return base.ToString();
			}
			var sb = new System.Text.StringBuilder("Set ");
			if (Receiving != null)
			{
				foreach (Expression receivingField in Receiving)
				{
					sb.Append(' ');
					sb.Append(receivingField);
				}
			}
			sb.Append(" TO ");
			if (Sending != null)
			{
				sb.AppendLine(Sending.ToString());
			}
			return sb.ToString();
		}

		ICollection<Identifier> IdentifierUser.Identifiers {
			get {
				var identifiers = new List<Identifier>();
				if (Sending is Identifier) identifiers.Add(Sending as Identifier);
				foreach(var expression in Receiving)
					if (expression is Identifier)
						identifiers.Add(expression as Identifier);
				return identifiers;
			}
		}

		/// <summary>
		/// Regarding the sending element, only one of the pair elements is not null:
		/// either we know its qualified name, or its type.
		/// </summary>
		ICollection<System.Tuple<System.Tuple<QualifiedName,DataType>,QualifiedName>> SymbolWriter.Symbols {
			get {
				var list = new List<System.Tuple<System.Tuple<QualifiedName,DataType>,QualifiedName>>();
				var sending = new System.Tuple<QualifiedName,DataType>(IdentifierUtils.GetQualifiedName(Sending), IdentifierUtils.GetDataType(Sending));
				foreach(var r in Receiving) {
					var receiving = IdentifierUtils.GetQualifiedName(r);
					list.Add(new System.Tuple<System.Tuple<QualifiedName,DataType>,QualifiedName>(sending, receiving));
				}
				return list;
			}
		}
		public bool IsUnsafe { get { return true; } }

	}
}
