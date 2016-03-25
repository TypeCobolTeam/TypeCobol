using System.Collections.Generic;

namespace TypeCobol.Codegen.Skeletons {

	public class Skeleton {
		/// <summary>Skeleton identifier.</summary>
		public string Name            { get; internal set; }
		/// <summary>List of <see cref="Pattern"/> defining this skeleton.</summary>
		public List<Pattern> Patterns { get; internal set; }

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			if (Name != null) str.Append("name:").Append(Name).Append(' ');
			str.Append('(').Append(Patterns.Count).AppendLine(" patterns)");
			foreach(var pattern in Patterns)
				str.Append(" - ").AppendLine(pattern.ToString());
			return str.ToString();
		}
	}

}
