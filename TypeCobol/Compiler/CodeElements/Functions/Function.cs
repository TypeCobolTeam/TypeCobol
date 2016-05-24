using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements.Functions {
	public class Function {
		public QualifiedName Name;
		public ICollection<Parameter> Parameters;

		public Function(string name, ICollection<Parameter> parameters = null) {
			this.Name = new URI(name);
			if (parameters != null) this.Parameters = parameters;
			else this.Parameters = new List<Parameter>();
		}
	}

	public class Parameter {
		public string Name;
		public DataType Type;
		public int Length;
	}
}
