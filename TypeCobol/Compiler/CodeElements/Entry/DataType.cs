using System;

namespace TypeCobol.Compiler.CodeElements
{
	public class DataType {
		public string Name { get; private set; }
		public DataType(string name) { Name = name; }

		public override string ToString() { return Name; }

		public override int GetHashCode() { return Name.GetHashCode(); }

		public override bool Equals(object obj) {
			var other = obj as DataType;
			if (other == null) return false;
			return other == this;
		}

		public static bool operator ==(DataType x, DataType y) {
			if (Object.ReferenceEquals(x, null) && Object.ReferenceEquals(y, null)) return true;
			if (Object.ReferenceEquals(x, null) || Object.ReferenceEquals(y, null)) return false;
			return x.Name == y.Name;
		}
		public static bool operator !=(DataType x, DataType y) {
			return !(x == y);
		}



		public static DataType Create(string picture) {
			char[] chars = distinct(picture);
			if (contains(chars, 'E'))
				return DataType.FloatingPoint;// ±?E±99
			if (contains(chars, new char[]{'X'}))
				if (contains(chars, new char[]{'B','0','/'}))
					 return DataType.AlphanumericEdited;
				else return DataType.Alphanumeric;
			if (contains(chars, new char[]{'A'}))
				return DataType.Alphabetic;
			if (contains(chars, new char[]{'G','N'}))
				return DataType.DBCS;
			if (contains(chars, new char[]{'9','S','V','P'}))
				if (contains(chars, new char[]{'B','0','/','.','Z','+','-','*','D'/*,'B'*/,'C','S'}))
					 return DataType.NumericEdited;
				else return DataType.Numeric;
			return DataType.Unknown;
		}

		// we don't want a dependency to Linq just for these
		private static bool contains(char[] array, char c) {
			return Array.IndexOf(array, c) > -1;
		}
		private static bool contains(char[] array, char[] chars) {
			foreach(char c in chars)
				if (contains(array, c)) return true;
			return false;
		}
		private static char[] distinct(string input) {
			System.Collections.Generic.HashSet<char> set = new System.Collections.Generic.HashSet<char>(input);
			char[] result = new char[set.Count];
			set.CopyTo(result);
			return result;
		}



		public static readonly DataType Unknown            = new DataType("?");
		public static readonly DataType Alphabetic         = new DataType("Alphabetic");
		public static readonly DataType Numeric            = new DataType("Numeric");
		public static readonly DataType NumericEdited      = new DataType("NumericEdited");
		public static readonly DataType Alphanumeric       = new DataType("Alphanumeric");
		public static readonly DataType AlphanumericEdited = new DataType("AlphanumericEdited");
		public static readonly DataType DBCS               = new DataType("DBCS");
		public static readonly DataType FloatingPoint      = new DataType("FloatingPoint");
	}
}
