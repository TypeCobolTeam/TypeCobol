using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeModel
{
	interface SymbolUser
	{
		/// <summary>List of symbols used, as long as if they are written (true) or read-only (false)</summary>
		Dictionary<QualifiedName,bool> Symbols { get; }
	}
}
