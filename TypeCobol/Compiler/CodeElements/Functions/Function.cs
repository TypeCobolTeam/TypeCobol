using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements.Functions {
	public class Function {
		public QualifiedName QualifiedName;
		public IList<Parameter> Parameters;
		public Parameter Result;

		public Function(string name, string program, Parameter result, IList<Parameter> parameters = null)
		  : this(new URI(program+'.'+name), result, parameters) { }

		public Function(QualifiedName name, Parameter result, IList<Parameter> parameters = null) {
			this.QualifiedName = name;
			this.Parameters = parameters ?? new List<Parameter>();
			this.Result = result;
		}

		public string Name { get { return QualifiedName.Head; } }
		public string Program {
			get {
				string[] parts = QualifiedName.ToString().Split('.');
				var tail = new System.Text.StringBuilder();
				for (int c=0; c<parts.Length-1; c++) tail.Append(parts[c]).Append('.');
				if (parts.Length > 1) tail.Length -= 1;
				return tail.ToString();
			}
		}
		public string Copy { get { return Program+"cpy"; } }
		public string Lib  { get { return Program; } }

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
		public bool IsCustom;

		public Parameter(bool isCustom, DataType type, int length=int.MaxValue) {
			this.Type   = type;
			this.Length = length;
			if (length < 1) throw new System.ArgumentOutOfRangeException("Length must be >0 (actual: "+length+')');
			this.IsCustom = isCustom;
		}

		public string Definition {
			get {
				if (IsCustom) return "TYPE "+Type.Name;
			    if (Type == DataType.Numeric)
			    {
                    return "PIC 9" + (Length > 0 ? "(" + Length + ")" : "");
                }
				return "PIC X"+(Length>0?"("+Length+")":"");
			}
		}

		public override string ToString() {
			return (Type != null? Type.ToString() : "?") + (Length < int.MaxValue? '('+Length.ToString()+')' : "");
		}
	}
	public class CallParameter: Parameter {
		public string Value { get; private set; }
		public bool ByReference { get; private set; }
		public CallParameter(string Value, bool ByReference = true)
		  : base (false, null) {
			this.Value = Value;
			this.ByReference = ByReference;
		}
		public string Mode {
			get { return ByReference?"REFERENCE":"CONTENT"; }
		}
	}

	public class SampleFactory {
		public static Function Create(string name, string library = "TC-DEFAULT") {
			return new Function(name, library,
				new Parameter(false, DataType.Numeric, 8),
				new List<Parameter>() {
					new Parameter(false, DataType.Numeric),
					new Parameter(false, DataType.Numeric, 3),
				});
		}
		public static Function CreateCall(string name, string library = "TC-DEFAULT") {
			return new Function(name, library,
				new Parameter(false, DataType.Numeric, 8),
				new List<Parameter>() {
					new CallParameter("param1"),
					new CallParameter("'42'", false),
				});
		}
	}
}
