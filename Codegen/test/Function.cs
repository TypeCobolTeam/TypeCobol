namespace TypeCobol.Codegen.Config
{

using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeElements.Expressions;

	public class Function {
		public AccessModifier Visibility { get; private set; }
		public QualifiedName QualifiedName { get; private set; }
		public ParametersProfile Profile { get; private set; }

		public ParameterDescription Result {
			get {
				if (Profile.ReturningParameter != null) return new ParameterDescription(Profile.ReturningParameter);
				if (Profile.OutputParameters.Count == 1) return new ParameterDescription(Profile.OutputParameters[0]);
				throw new System.InvalidOperationException(QualifiedName+" has "+Profile.OutputParameters.Count+" output parameters");
			}
		}

		/// <summary>Creates function.</summary>
		public Function(QualifiedName name, IList<ParameterDescriptionEntry> inputs, ParameterDescriptionEntry returning, AccessModifier visibility = AccessModifier.Private)
			: this(name, inputs, null, null, returning, visibility) { }
		/// <summary>Creates procedure.</summary>
		public Function(QualifiedName name, IList<ParameterDescriptionEntry> inputs, IList<ParameterDescriptionEntry> outputs, IList<ParameterDescriptionEntry> inouts = null, AccessModifier visibility = AccessModifier.Private)
			: this(name, inputs, outputs, inouts, null, visibility) { }
		/// <summary>Creates functions or procedure</summary>
		public Function(QualifiedName name, IList<ParameterDescriptionEntry> inputs, IList<ParameterDescriptionEntry> outputs, IList<ParameterDescriptionEntry> inouts, ParameterDescriptionEntry returning, AccessModifier visibility = AccessModifier.Private) {
			QualifiedName = name;
			Profile = new ParametersProfile();
			Profile.InputParameters  = inputs  ?? new List<ParameterDescriptionEntry>();
			Profile.OutputParameters = outputs ?? new List<ParameterDescriptionEntry>();
			Profile.InoutParameters  = inouts  ?? new List<ParameterDescriptionEntry>();
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
		public string Copy    { get { return Program+"cpy"; } }
		public string Library { get { return Program; } }
		public IList<ParameterDescriptionEntry> InputParameters {
			get {
				var parameters = new List<ParameterDescriptionEntry>();
				parameters.AddRange(Profile.InputParameters);
				parameters.AddRange(Profile.InoutParameters);
				parameters.AddRange(Profile.OutputParameters);
				return parameters;
			}
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder(Name ?? "?");
			str.Append(Profile.ToString());
			return str.ToString();
		}
	}

    public class ParameterDescription : TypeCobol.Compiler.Nodes.DataDescription
    {
        public ParameterDescription(ParameterDescriptionEntry entry) : base(entry) { }
    }


    public class Parameter {
		public SymbolReference Name;
		public DataType Type;
		public int Length;
		public bool IsCustom;

		public Parameter(SymbolReference name): this(name, false, null, int.MaxValue) { }
		public Parameter(SymbolReference name, bool isCustom, DataType type, int length=int.MaxValue) {
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



}
