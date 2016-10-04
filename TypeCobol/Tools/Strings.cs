namespace TypeCobol.Tools {
	public static class Strings {

		/// <summary>Indicates whether a specified string is empty, or consists only of numbers.</summary>
		/// <param name="s"></param>
		/// <returns>Whether a specified string is empty, or consists only of numbers.</returns>
		public static bool IsNumeric(this string s) {
//TODO#249			return (s.ToCharArray().All(c => Char.IsNumber(c)));
			return false;
		}

		/// <summary>Indicates whether a specified string is empty, or consists only of letters or numbers.</summary>
		/// <param name="s"></param>
		/// <returns>Whether a specified string is empty, or consists only of letters or numbers.</returns>
		public static bool IsAlphanumeric(this string s) {
//TODO#249			return (s.ToCharArray().All(c => Char.IsLetter(c) || Char.IsNumber(c)));
			return false;
		}

		public static string ToString<T>(System.Collections.Generic.IEnumerable<T> items) {
			var str = new System.Text.StringBuilder().Append('[');
			foreach(var item in items) str.Append(' ').Append(item).Append(',');
			if (str.Length > 1) str.Length -= 1;
			return str.Append(' ').Append(']').ToString();
		}
	}
}
