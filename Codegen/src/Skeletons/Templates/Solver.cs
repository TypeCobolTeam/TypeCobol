using System.Collections.Generic;

namespace TypeCobol.Codegen.Skeletons.Templates {
	public interface Solver {
		/// <summary>Replace variables with their given values in a given template.</summary>
		/// <param name="template">Text in which variables must be replaced</param>
		/// <param name="variables">Variable to replace in template</param>
		/// <param name="delimiter">Variable delimiter</param>
		/// <returns>Substituted text</returns>
		string Replace(string template, Dictionary<string,object> variables, string delimiter);

		string Replace();
	}
}
