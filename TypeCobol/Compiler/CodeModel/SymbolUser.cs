﻿using System.Collections.Generic;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeModel
{
	interface Sending {
		/// <summary>A sending area</summary>
		Expression Expression { get; }
	}
	interface Receiving {
        /// <summary>List of receiving areas</summary>
        [NotNull]
        IList<Expression> Expressions { get; }
	}

	interface SymbolUser
	{
		/// <summary>List of symbols used, wether they are written or read-only.</summary>
		ICollection<QualifiedName> Symbols { get; }
	}

	interface IdentifierUser
	{
		/// <summary>List of identifiers used, wether they are written or read-only.</summary>
		ICollection<Identifier> Identifiers { get; }
	}

	interface SymbolWriter
	{
		/// <summary>
		/// List of symbol pairs: the first element of the pair is read-only
		/// and its content is written into the second element of the pair.
		/// </summary>
		ICollection<System.Tuple<System.Tuple<QualifiedName,TypeCobol.Compiler.CodeElements.DataType>,QualifiedName>> Symbols { get; }
		/// <summary>Are unsafe write operations allowed?</summary>
		bool IsUnsafe { get; }
	}
}
