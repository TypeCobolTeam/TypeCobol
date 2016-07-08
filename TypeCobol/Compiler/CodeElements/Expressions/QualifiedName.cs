using System.Collections.Generic;
using System.Collections.Specialized;

namespace TypeCobol.Compiler.CodeElements.Expressions {

	public interface QualifiedName: IList<string> {
		string Head { get; }
		bool IsExplicit { get; }
		bool Matches(string uri);
	}

	public interface Subscripted {
		Subscript this[string name] { get; }
		IEnumerable<Subscript> Subscripts { get; }
	}



	public abstract class AbstractQualifiedName: QualifiedName {
		public virtual bool IsExplicit { get { return true; } }

		public abstract string Head { get; }
		public abstract int Count { get; }
		public abstract IEnumerator<string> GetEnumerator();

		System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() { return GetEnumerator(); }

		public bool IsReadOnly { get { return true; } }
		public void Add(string item)    { throw new System.NotSupportedException(); }
		public bool Remove(string item) { throw new System.NotSupportedException(); }
		public void Clear()             { throw new System.NotSupportedException(); }
		public bool Contains(string item) {
			foreach(string name in this)
				if (name.Equals(item)) return true;
			return false;
		}
		public void CopyTo(string[] array, int index) {
			if (array == null) throw new System.ArgumentNullException();
			if (index < 0) throw new System.ArgumentOutOfRangeException();
			if (array.Length < index+Count) throw new System.ArgumentException();
			int c = 0;
			foreach(string name in this) {
				array[index+c] = name;
				c++;
			}
		}
		public string this[int index] {
			get {
				int c = 0;
				foreach(string name in this)
					if (c == index) return name;
					else c++;
				throw new System.ArgumentOutOfRangeException(index+" outside of [0,"+Count+"[");
			}
			set { throw new System.NotSupportedException(); }
		}
		public int IndexOf(string item) {
			int c = 0;
			foreach(string name in this)
				if (name.Equals(item)) return c;
				else c++;
			return -1;
		}
		public void Insert(int index, string item) { throw new System.NotSupportedException(); }
		public void RemoveAt(int index)            { throw new System.NotSupportedException(); }

		public override bool Equals(System.Object other) {
			if (other == null) return false;
			return Equals(other as QualifiedName);
		}
		public virtual bool Equals(QualifiedName other) {
			if (other == null) return false;
//			if (other.IsExplicit != IsExplicit) return false;
			if (other.Count != Count) return false;
			for(int c = 0; c < Count; c++)
				if (!other[c].Equals(this[c])) return false;
			return true;
		}
		public override int GetHashCode() {
			int hash = 13;
//			hash = hash*7 + IsExplicit.GetHashCode();
			hash = hash*7 + Count.GetHashCode();
			foreach(string part in this)
				hash = hash*7 + part.GetHashCode();
			return hash;
		}

		public bool Matches(string uri) {
			return this.ToString().Equals(uri);
		}
	}



	public class SyntacticQualifiedName: AbstractQualifiedName {
		public Symbol Symbol { get; private set; }
		public IList<DataName> DataNames { get; private set; }
		public FileName FileName { get; private set; }

		public override bool IsExplicit { get { return _explicit; } }
		private bool _explicit;

		public SyntacticQualifiedName(Symbol symbol, IList<DataName> datanames = null, FileName filename = null, bool isExplicit = false) {
			this.Symbol = symbol;
			this.DataNames = datanames != null ? datanames : new List<DataName>();
			this.FileName = filename;
			_explicit = isExplicit;
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			foreach (string name in this) str.Append(name).Append('.');
			str.Length -= 1;
			return str.ToString();
		}

		public override string Head {
			get {
				if (Symbol == null) return null;
				return Symbol.Name;
			}
		}

		public override IEnumerator<string> GetEnumerator() {
			if (FileName != null) yield return FileName.Name;
			foreach (var dataname in DataNames) yield return dataname.Name;
			if (Symbol != null) yield return Symbol.Name;
		}

		public override int Count { get { return DataNames.Count+(FileName!=null?2:1); } }
	}



	public class SubscriptedQualifiedName: AbstractQualifiedName, Subscripted {
		protected List<KeyValuePair<string,Subscript>> names = new List<KeyValuePair<string,Subscript>>();

		public override bool IsExplicit { get { return _explicit; } }
		private bool _explicit;

		public SubscriptedQualifiedName(bool isExplicit = true) {
			_explicit = isExplicit;
		}

		internal void Add(string name,Subscript subscript) {
			names.Add(new KeyValuePair<string,Subscript>(name, subscript));
		}

		public override string ToString() {
			var str = new System.Text.StringBuilder();
			foreach(var item in names) {
				str.Append(item.Key);
				if (item.Value != null) str.Append('[').Append(item.Value.ToString()).Append(']');
				str.Append('.');
			}
			if (names.Count > 0) str.Length -= 1;
			return str.ToString();
		}


		public override string Head {
			get {
				if (names.Count < 1) return null;
				return names[names.Count-1].Key;
			}
		}

		public override IEnumerator<string> GetEnumerator() {
			foreach (var item in names)
				yield return (string)item.Key;
		}

		public override int Count { get { return names.Count; } }

		public Subscript this[string name] {
			get {
				foreach(var item in names)
					if (item.Key.Equals(name)) return item.Value;
					//TODO what if same name more than once?
				return null;
			}
		}
		public IEnumerable<Subscript> Subscripts {
			get {
				foreach(var item in names)
					if (item.Value != null)
						yield return item.Value;
			}
		}





		/// <summary>Factory method.</summary>
		/// <param name="identifier">Parsed identifier for this name qualification</param>
		/// <param name="data">Data declaration the created name will fully-qualify</param>
		/// <param name="messages">Error messages. If there are some, there is something wrong with <paramref name="identifier"/>'s name qualification</param>
		/// <returns></returns>
		public static SubscriptedQualifiedName Create(Identifier identifier, DataDescriptionEntry data, out List<string> messages) {
			var names = CreatePairs(identifier, data, out messages);
			var qelement = new TypeCobol.Compiler.CodeElements.Expressions.SubscriptedQualifiedName();
			foreach(var pair in names) qelement.Add(pair.Item1,pair.Item2);
			return qelement;
		}

		private static List<System.Tuple<string,Subscript>> CreatePairs(Identifier identifier, DataDescriptionEntry data, out List<string> errors) {
			var names = new List<System.Tuple<string,Subscript>>();
			errors = new List<string>();
			var subscripts = new List<Subscript>();
			subscripts.AddRange((identifier as Subscriptable).Subscripts);
			subscripts.Reverse();
			int c = 0;
			var current = data;
			while (current != null) {
				string name = current.QualifiedName[current.QualifiedName.Count-1];
				TypeCobol.Compiler.CodeElements.Expressions.Subscript subscript = null;
				if (current.IsTableOccurence) {
					if (c < subscripts.Count) subscript = subscripts[c];
					c++;
				}
				names.Add(new System.Tuple<string,TypeCobol.Compiler.CodeElements.Expressions.Subscript>(name, subscript));
				current = current.TopLevel;
			}
			if (c < subscripts.Count) errors.Add(identifier.Name+": too many subscripts ("+subscripts.Count+" vs expected="+c+')');
			names.Reverse();
			return names;
		}
	}





	public class URI: AbstractQualifiedName {
		public string Value { get; private set; }
		public char Separator { get; private set; }
		private string[] parts;

		public URI(string uri, char separator = '.') {
			if (uri == null) throw new System.ArgumentNullException("URI must not be null.");
			this.Separator = separator != null ? separator : '.';
			this.Value = uri;
			this.parts = Value.Split(this.Separator);
		}

		public override string ToString() { return Value; }

		public override string Head { get { return parts[parts.Length-1]; } }

		public override IEnumerator<string> GetEnumerator() {
			foreach(string part in parts) yield return part;
		}

		public override int Count { get { return parts.Length; } }
	}
}
