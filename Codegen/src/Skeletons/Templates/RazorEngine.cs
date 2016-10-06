using System.Collections.Generic;
using RazorEngine;
using RazorEngine.Templating; // For extension methods.

namespace TypeCobol.Codegen.Skeletons.Templates {

	public class RazorEngine: Solver {

		public static string DEFAULT_DELIMITER = "%";

		private string Template;
		private Dictionary<string,object> Variables;
		private string Delimiter;

		public RazorEngine(): this("", new Dictionary<string,object>(), DEFAULT_DELIMITER) { }

		public RazorEngine(string template, Dictionary<string,object> variables, string delimiter = null) {
			this.Template = template;
			this.Variables = variables;
			this.Delimiter = delimiter;
		}
		public string Replace() {
			return Replace(Template, Variables, Delimiter);
		}

		public string Replace(string template, Dictionary<string,object> variables = null, string delimiter = null) {
			if ("@".Equals(delimiter)) throw new System.ArgumentException("Illegal delimiter: @");
			if (delimiter == null) delimiter = DEFAULT_DELIMITER;
			template = template.Replace(delimiter, "@Model.");
			object model = variables == null ? new { } : CreateAnonymousObjectFromDictionary(variables);
			// TODO: make key unique
			// key should be unique because razor caches the result
			// so it can then be retrieved with only this key
			string key = md5(template);
			template = template.Trim(' ').TrimStart('\r','\n').Replace("\r\n","\n");
			string html = "";
			try { html = Engine.Razor.RunCompile(template, key, null, model); }
			catch(System.Exception ex) { 
				if (!ex.Message.Contains("does not contain a definition for")) throw;
				char[] array = ex.Message.ToCharArray();
				System.Array.Reverse( array );
				var reversed = new string(array);
				int start = ex.Message.Length - reversed.IndexOf('\'',1);
				string problem = ex.Message.Substring(start, ex.Message.Length-start-1);
				throw new System.ArgumentException("Variable \""+problem+"\" undefined");
			}
			return System.Net.WebUtility.HtmlDecode(html);
		}

		private object CreateAnonymousObjectFromDictionary(Dictionary<string,object> properties) {
			var obj = new System.Dynamic.ExpandoObject();
			var props = (ICollection<KeyValuePair<string, object>>)obj;
			foreach (var x in properties) props.Add(x);
			return obj;
		}

		private static string md5(string input) {
			var bytes = System.Text.Encoding.ASCII.GetBytes(input);
			var hash = System.Security.Cryptography.MD5.Create().ComputeHash(bytes);
			var res = new System.Text.StringBuilder();
			foreach (byte t in hash) res.Append(t.ToString("X2"));
            return res.ToString();
        }



		public static Solver Create(string template, Dictionary<string,object> variables, string delimiter) {
			return new RazorEngine(template, variables ?? new Dictionary<string,object>(), delimiter ?? DEFAULT_DELIMITER);
		}
	}

}
