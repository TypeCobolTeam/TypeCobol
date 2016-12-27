using System.Collections.Generic;

namespace TypeCobol.Codegen.Skeletons {

	public class Skeleton: IList<Pattern> {
		/// <summary>Skeleton identifier.</summary>
		public string Name            { get; internal set; }
		/// <summary>List of <see cref="Condition"/> for this skeleton to be used.</summary>
		public List<Condition> Conditions { get; internal set; }
		/// <summary>List of <see cref="Pattern"/> defining this skeleton.</summary>
		public List<Pattern> Patterns { get; internal set; }
		/// <summary>List of properties necessary for code generation of Patterns.</summary>
		public ICollection<string> Properties { get; set; }

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			if (Name != null) str.Append(Name).Append(' ');
			str.Append('(').Append(Patterns.Count).AppendLine(" pattern(s))");
			foreach(var condition in Conditions)
				str.Append(" - condition: ").AppendLine(condition.ToString());
			foreach(var pattern in this)
				str.Append(" - pattern: ").AppendLine(pattern.ToString());
			str.Append("Properties: [ ");
			foreach(var pname in Properties) str.Append(pname).Append(", ");
			if (Properties.Count > 0) str.Length -= 2;
			str.AppendLine(" ]");
			return str.ToString();
		}

		// from IList<Pattern>
		public Pattern this[int index] { get { return Patterns[index]; } set { Patterns[index] = value; } }
		public int IndexOf(Pattern item) { return Patterns.IndexOf(item); }
		public void Insert(int index, Pattern item) { Patterns.Insert(index, item); }
		public void RemoveAt(int index) { Patterns.RemoveAt(index); }
		// from ICollection<Pattern>
		public int Count { get { return Patterns.Count; } }
		public bool IsReadOnly { get { return false; } }
		public void Add(Pattern item) { Patterns.Add(item); }
		public void Clear() { Patterns.Clear(); }
		public bool Contains(Pattern item) { return Patterns.Contains(item); }
		public void CopyTo(Pattern[] array, int index) { Patterns.CopyTo(array, index); }
		public bool Remove(Pattern item) { return Patterns.Remove(item); }
		// from IEnumerable<Pattern>
		public IEnumerator<Pattern> GetEnumerator() { foreach(var item in Patterns) yield return item; }
		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() { return GetEnumerator(); }
	}
}
