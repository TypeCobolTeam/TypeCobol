using System.Collections.Generic;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Nodes {

	public interface Generated {
		/// <summary>Generated code.</summary>
		IEnumerable<ITextLine> Lines { get; }
		/// <summary>Must be treated as a leaf regarding codegen.</summary>
		bool IsLeaf { get; }
	}
}
