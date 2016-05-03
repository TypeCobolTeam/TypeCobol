using System.Collections.Generic;

namespace TypeCobol.Codegen.Config {

	public class Cheetah {

		public static string Replace(string text, string variable, string value, char delimiter='%') {
			return text.Replace(delimiter+variable, value);
		}

		public static string Replace(string text, Dictionary<string,string> variables, char delimiter='%') {
			foreach(var x in variables) {
				text = Replace(text, x.Key, x.Value, delimiter);
			}
			return text;
		}

	}
}
