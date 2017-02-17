using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Tools {

public class CodeElementDiagnostics {
	private ISearchableReadOnlyList<ICodeElementsLine> Lines;

	/// <param name="lines">For example CodeElementsDocument.Lines</param>
	public CodeElementDiagnostics(ISearchableReadOnlyList<ICodeElementsLine> lines) {
		this.Lines = lines;
	}

	public IList<Diagnostic> GetDiagnostics(CodeElement e) {
		var results = new List<Diagnostic>();
        if (e.Diagnostics != null)
        {
            results.AddRange(e.Diagnostics);
        }
		return results;
	}

    
}



}
