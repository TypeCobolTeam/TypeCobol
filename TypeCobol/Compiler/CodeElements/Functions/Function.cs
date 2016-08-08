namespace TypeCobol.Compiler.CodeElements.Functions {

using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements.Expressions;

	public class Function {
		public AccessModifier Visibility { get; private set; }
		public QualifiedName QualifiedName { get; private set; }
		public ParametersProfile Profile { get; private set; }

		public ParameterDescription Result {
			get {
				if (Profile.ReturningParameter != null) return Profile.ReturningParameter;
				if (Profile.OutputParameters.Count == 1) return Profile.OutputParameters[0];
				throw new System.InvalidOperationException(QualifiedName+" has "+Profile.OutputParameters.Count+" output parameters");
			}
		}

		/// <summary>Creates function.</summary>
		public Function(QualifiedName name, IList<ParameterDescription> inputs, ParameterDescription returning, AccessModifier visibility = AccessModifier.Private)
			: this(name, inputs, null, null, returning, visibility) { }
		/// <summary>Creates procedure.</summary>
		public Function(QualifiedName name, IList<ParameterDescription> inputs, IList<ParameterDescription> outputs, IList<ParameterDescription> inouts = null, AccessModifier visibility = AccessModifier.Private)
			: this(name, inputs, outputs, inouts, null, visibility) { }
		/// <summary>Creates functions or procedure</summary>
		public Function(QualifiedName name, IList<ParameterDescription> inputs, IList<ParameterDescription> outputs, IList<ParameterDescription> inouts, ParameterDescription returning, AccessModifier visibility = AccessModifier.Private) {
			QualifiedName = name;
			Profile = new ParametersProfile();
			Profile.InputParameters  = inputs  ?? new List<ParameterDescription>();
			Profile.OutputParameters = outputs ?? new List<ParameterDescription>();
			Profile.InoutParameters  = inouts  ?? new List<ParameterDescription>();
			Profile.ReturningParameter = returning;
			Visibility = visibility;
		}

		/// <summary>TCRFUN_NO_RETURNING_FOR_PROCEDURES</summary>
		public bool IsProcedure { get { return Profile.ReturningParameter == null; } }
		/// <summary>TCRFUN_NO_INOUT_OR_OUTPUT_FOR_FUNCTIONS</summary>
		public bool IsFunction  { get { return Profile.OutputParameters.Count == 0 && Profile.InoutParameters.Count == 0; } }

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
			str.Append(Profile.ToString());
			return str.ToString();
		}
	}

	public class Parameter {
		public DataName Name;
		public DataType Type;
		public int Length;
		public bool IsCustom;

		public Parameter(DataName name): this(name, false, null, int.MaxValue) { }
		public Parameter(DataName name, bool isCustom, DataType type, int length=int.MaxValue) {
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
			return (Name==null? "?":Name.Name)+ ' ' + (Type!=null? Type.ToString():"?") + (Length<int.MaxValue? '('+Length.ToString()+')':"");
		}
	}

	public enum AccessModifier {
		Public,
		Private
	}

}
