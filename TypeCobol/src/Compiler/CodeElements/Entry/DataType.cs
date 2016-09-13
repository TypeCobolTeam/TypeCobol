using System;

namespace TypeCobol.Compiler.CodeElements
{
	public class DataType {
		public string Name { get; private set; }
		public bool IsStrong { get; private set; }
		public bool IsNestable { get; private set; }

		public DataType(string name, bool IsStrong=false, bool IsNestable=true) {
			Name = name;
			if (name == null) throw new ArgumentNullException();
			this.IsStrong = IsStrong;
			this.IsNestable = IsNestable;
		}

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
			return x.Name.ToUpper() == y.Name.ToUpper();
		}
		public static bool operator !=(DataType x, DataType y) {
			return !(x == y);
		}



		public static DataType Create(string picture) {
			var basic = new char[]{'.','Z','+','-','*','D'/*,'B'*/,'C'/*,'S'*/};
			return doCreate(picture, basic);
		}
		public static DataType Create(string picture, char[] currencies) {
			var basic = new char[]{'.','Z','+','-','*','D'/*,'B'*/,'C'/*,'S'*/};
			var all = new char[basic.Length + currencies.Length];
			basic.CopyTo(all, 0);
			currencies.CopyTo(all, basic.Length);
			return doCreate(picture, all);
		}
		private static DataType doCreate(string picture, char[] numericEditingSpecificChars) {
			//if (picture.Length > 50) return DataType.Unknown;// it's not our job to detect this
			char[] editingBase = new char[]{'B','0','/'};
			char[] editingNumeric = new char[editingBase.Length + numericEditingSpecificChars.Length];
			editingBase.CopyTo(editingNumeric, 0);
			numericEditingSpecificChars.CopyTo(editingNumeric, editingBase.Length);

			picture = remove(picture, '(',')');
			char[] chars = distinct(picture.ToUpper());
			if (contains(chars, 'E'))
				return DataType.FloatingPoint;// ±?E±99
			if (contains(chars, new char[]{'X'}))
				if (contains(chars, editingBase))
					 return DataType.AlphanumericEdited;
				else return DataType.Alphanumeric;
			if (contains(chars, new char[]{'A'}))
				return DataType.Alphabetic;
			if (contains(chars, new char[]{'G','N'}))
				return DataType.DBCS;
			if (contains(chars, editingNumeric))
				return DataType.NumericEdited;
			if (contains(chars, new char[]{'9','S','V','P'}))
				return DataType.Numeric;
			return DataType.Unknown;
		}
		/// <summary>Remove from string expr all substrings between characters begin and end.</summary>
		/// <param name="expr">string to be analyzed</param>
		/// <param name="begin">first character of the the substring to be removed</param>
		/// <param name="end">last character of the substrings to be removed</param>
		/// <returns>Resulting string</returns>
		private static string remove(string expr, char begin, char end) {
			string regex = string.Format("\\{0}.*?\\{1}", begin, end);
			return new System.Text.RegularExpressions.Regex(regex).Replace(expr, "");
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
		/// <summary>
		/// Return all distinct characters composing the input string.
		/// Even if present more than once in the input string, each character is only present once in the result array.
		/// A character not present in the input string won't be present in the result array.
		/// </summary>
		/// <param name="input">String to be analyzed</param>
		/// <returns>Distinct characters composing input.</returns>
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
// [TYPECOBOL]
		public static readonly DataType Boolean            = new DataType("BOOL", true, true);

		public static readonly TypeDefinition Date = CreateDate();
		private static TypeDefinition CreateDate() {
			var type = new CustomTypeDefinition(new DataType("DATE", true, true));
			CreateMember(type, 5, "YYYY", Numeric,4);
			CreateMember(type, 5, "MM",   Numeric,2);
			CreateMember(type, 5, "DD",   Numeric,2);
			return type;
		}
		private static void CreateMember(DataDescriptionEntry parent, int level, string name, DataType type, int length) {
			var data = new DataDescriptionEntry();
			data.LevelNumber = level;
			data.DataName = new StringDataName(name);
			data.DataType = type;
			data.MemoryArea = new TypeCobol.Compiler.CodeModel.DataInMemory(length, 0);//TODO half-assed
			data.Picture = String.Format("9 ({0})", data.MemoryArea.Length);
			data.TopLevel = parent;
			parent.Subordinates.Add(data);
		}
// [/TYPECOBOL]

	}

	public interface TypeDefinition {
		bool IsTypeDefinition { get; }
		DataType DataType { get; }
		System.Collections.Generic.ICollection<DataDescriptionEntry> Subordinates { get; }
	}
	public class CustomTypeDefinition: DataDescriptionEntry, TypeDefinition {
		public CustomTypeDefinition(DataType type) {
			this.DataType = type;
		}
		public override bool IsTypeDefinition { get { return true; } }
	}



	public class StringDataName: DataName {
		private string name_;
		public override string Name { get { return name_; } }
		/// <param name="name">Cannot be null</param>
        public StringDataName(string name): base(null) { name_ = name; }

		public override bool Equals(object obj) {
			var other = obj as StringDataName;
			if (other == null) return false;
			return other == this;
		}
		public static bool operator ==(StringDataName x, StringDataName y) {
			if (Object.ReferenceEquals(x, null) && Object.ReferenceEquals(y, null)) return true;
			if (Object.ReferenceEquals(x, null) || Object.ReferenceEquals(y, null)) return false;
			return x.Name.ToUpper() == y.Name.ToUpper();
		}
		public static bool operator !=(StringDataName x, StringDataName y) {
			return !(x == y);
		}

		public override string ToString() { return Name; }
	}
}
