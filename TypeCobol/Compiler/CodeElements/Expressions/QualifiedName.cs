using System.Collections.Generic;
using System.Collections.Specialized;

namespace TypeCobol.Compiler.CodeElements.Expressions {

	public interface QualifiedName: IList<string> {
		char Separator { get; }
		string Head { get; }
		string Tail { get; }
		QualifiedName Parent { get; }
		bool IsExplicit { get; }
		bool Matches(string uri);
		bool Matches(QualifiedName name);
	}



	public abstract class AbstractQualifiedName: QualifiedName {
		public virtual bool IsExplicit { get { return false; } }
		public virtual char Separator {
			get { return '.'; }
			set { throw new System.NotSupportedException(); }
		}

		public abstract string Head { get; }
		public virtual string Tail {
			get {
				var uri = this.ToString();
				return uri.Remove(uri.Length-2-Head.Length);
			}
		}
		public abstract QualifiedName Parent { get; }
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
			return this.ToString().EndsWith(uri);
		}
		public bool Matches(QualifiedName name) {
			return this.Matches(name.ToString());
		}
	}



	public class URI: AbstractQualifiedName {
		public string Value { get; private set; }
		private string[] parts;

		public URI(string uri, char separator = '.') {
			if (uri == null) throw new System.ArgumentNullException("URI must not be null.");
			this.separator = separator != null ? separator : '.';
			this.Value = uri;
			this.parts = Value.Split(this.Separator);
		}

		private char separator;
		public override char Separator {
			get { return separator; }
			set { separator = value; }
		}

		public override string ToString() { return Value; }

		public override string Head { get { return parts[parts.Length-1]; } }
		public override QualifiedName Parent { get { return new URI(Value.Remove(Value.Length-1-Head.Length), Separator); } }

		public override IEnumerator<string> GetEnumerator() {
			foreach(string part in parts) yield return part;
		}

		public override bool IsExplicit { get { return false; } }

		public override int Count { get { return parts.Length; } }
	}
}
