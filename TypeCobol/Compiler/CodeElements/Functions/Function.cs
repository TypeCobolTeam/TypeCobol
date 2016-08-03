using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Compiler.CodeElements.Functions {

	public class Function {
		public AccessModifier Visibility { get; private set; }
		public QualifiedName QualifiedName { get; private set; }
		public IList<Parameter> InputParameters { get; private set; }
		public IList<Parameter> OutputParameters { get; private set; }
		public IList<Parameter> InoutParameters { get; private set; }
		public Parameter ReturningParameter { get; private set; }

		public Parameter Result {
			get {
				if (ReturningParameter != null) return ReturningParameter;
				if (OutputParameters.Count == 1) return OutputParameters[0];
				throw new System.InvalidOperationException(QualifiedName+" has "+OutputParameters.Count+" output parameters");
			}
		}

		/// <summary>Creates function.</summary>
		public Function(QualifiedName name, IList<Parameter> inputs, Parameter returning, AccessModifier visibility = AccessModifier.Private)
			: this(name, inputs, null, null, returning, visibility) { }
		/// <summary>Creates procedure.</summary>
		public Function(QualifiedName name, IList<Parameter> inputs, IList<Parameter> outputs, IList<Parameter> inouts = null, AccessModifier visibility = AccessModifier.Private)
			: this(name, inputs, outputs, inouts, null, visibility) { }
		/// <summary>Creates functions or procedure</summary>
		public Function(QualifiedName name, IList<Parameter> inputs, IList<Parameter> outputs, IList<Parameter> inouts, Parameter returning, AccessModifier visibility = AccessModifier.Private) {
			this.QualifiedName = name;
			this.InputParameters  = inputs  ?? new List<Parameter>();
			this.OutputParameters = outputs ?? new List<Parameter>();
			this.InoutParameters  = inouts  ?? new List<Parameter>();
			this.ReturningParameter = returning;
			this.Visibility = visibility;
		}

		/// <summary>TCRFUN_NO_RETURNING_FOR_PROCEDURES</summary>
		public bool IsProcedure { get { return ReturningParameter == null; } }
		/// <summary>TCRFUN_NO_INOUT_OR_OUTPUT_FOR_FUNCTIONS</summary>
		public bool IsFunction  { get { return OutputParameters.Count == 0 && InoutParameters.Count == 0; } }

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
			var str = new System.Text.StringBuilder(Name ?? "?");
			str.Append('(');
			foreach(var p in InputParameters) str.Append(p).Append(", ");
			if (InputParameters.Count > 0) str.Length -= 2;
			str.Append("):(");
			foreach(var p in OutputParameters) str.Append(p).Append(", ");
			if (OutputParameters.Count > 0) str.Length -= 2;
			str.Append("):(");
			foreach(var p in InoutParameters) str.Append(p).Append(", ");
			if (InoutParameters.Count > 0) str.Length -= 2;
			str.Append("):(");
			if (ReturningParameter != null) str.Append(ReturningParameter);
			str.Append(')');
			return str.ToString();
		}
	}

	public class Parameter {
		public string Name;
		public DataType Type;
		public int Length;
		public bool IsCustom;

		public Parameter(string name, bool isCustom, DataType type, int length=int.MaxValue) {
			this.Name = name;
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
			return (Name ?? "?")+ ' ' + (Type!=null? Type.ToString():"?") + (Length<int.MaxValue? '('+Length.ToString()+')':"");
		}
	}
	public class CallParameter: Parameter {
		public string Value { get; private set; }
		public bool ByReference { get; private set; }
		public CallParameter(string Value, bool ByReference = true)
		  : base (null, false, null) {
			this.Value = Value;
			this.ByReference = ByReference;
		}
		public string Mode {
			get { return ByReference?"REFERENCE":"CONTENT"; }
		}
	}

	public enum AccessModifier {
		Public,
		Private
	}

}
