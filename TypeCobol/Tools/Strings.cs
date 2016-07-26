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
	}
}
