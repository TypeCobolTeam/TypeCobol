using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements.Functions {
	public class Function {
		public QualifiedName Name;
		public IList<Parameter> Parameters;
		public Parameter Result;

		public Function(string name, Parameter result, IList<Parameter> parameters = null) {
			this.Name = new URI(name);
			if (parameters != null) this.Parameters = parameters;
			else this.Parameters = new List<Parameter>();
			this.Result = result;
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder(Name!=null? Name.ToString() : "?");
			str.Append('(');
			foreach(var p in Parameters) str.Append(p.ToString()).Append(", ");
			if (Parameters.Count > 0) str.Length -= 2;
			str.Append(')').Append(':').Append(Result.ToString());
			return str.ToString();
		}
	}

	public class Parameter {
		public DataType Type;
		public int Length;

		public Parameter(DataType type, int length=int.MaxValue) {
			this.Type   = type;
			this.Length = length;
			if (length < 1) throw new System.ArgumentOutOfRangeException("Length must be >0 (actual: "+length+')');
		}

		public override string ToString() {
			return (Type != null? Type.ToString() : "?") + (Length < int.MaxValue? '('+Length.ToString()+')' : "");
		}
	}

	public class SampleFactory {
		public static Function Create(string name) {
			return new Function(name,
				new Parameter(DataType.Numeric, 9),
				new List<Parameter>() {
					new Parameter(DataType.Numeric),
					new Parameter(DataType.Numeric, 3),
				});
		}
	}
}
