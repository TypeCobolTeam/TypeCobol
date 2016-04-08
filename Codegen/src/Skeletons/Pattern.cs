using System.Collections.Generic;

namespace TypeCobol.Codegen.Skeletons {

	public class Pattern {
		/// <summary>Pattern identifier.</summary>
		public string Name      { get; internal set; }
		/// <summary>Should code in <paramref name="Template"/> be generated more than once?</summary>
		public string Group     { get; internal set; }
		/// <summary>URI in an abstract syntax tree.</summary>
		public string Location  { get; internal set; }
		/// <summary>Variables usable in the <paramref name="Template"/>.</summary>
		public Dictionary<string,string> Variables { get; internal set; }
		/// <summary>Code template.</summary>
		public string Template  { get; internal set; }

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			if (Name != null) str.Append("name:").Append(Name).Append(' ');
			if (Group != null) str.Append("group:").Append(Group).Append(' ');
			if (Location != null) str.Append("location:").Append(Location).Append(' ');
			if (Variables.Count > 0) {
				str.Append("variables: {");
				foreach(var kv in Variables)
					str.Append(' ').Append(kv.Key).Append(':').Append(kv.Value).Append(',');
				str.Length -= 1;
				str.Append(" }");
			}
			return str.ToString();
		}
	}

}
