using System;
using System.Collections.Generic;
using System.Text;
using RazorEngine;
using RazorEngine.Templating; // For extension methods.

namespace TypeCobol.Codegen.Skeletons.Templates {

    /// <summary>
    /// This is the Razor substitution engine.
    /// </summary>
	public class RazorEngine: Solver {

		public static string DEFAULT_DELIMITER = "%";

		private string Template;
		private Dictionary<string,object> Variables;
		private string Delimiter;

		public RazorEngine(): this("", new Dictionary<string,object>(), DEFAULT_DELIMITER) { }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="template">The template</param>
        /// <param name="variables">The substitution environment variables</param>
        /// <param name="delimiter">Variable delimiter in the template</param>
		public RazorEngine(string template, Dictionary<string,object> variables, string delimiter = null) {
			this.Template = template;
			this.Variables = variables;
			this.Delimiter = delimiter;
		}

        /// <summary>
        /// Perform the substition.
        /// </summary>
        /// <returns>The result of the substitution applied to the template</returns>
		public string Replace() {
			return Replace(Template, Variables, Delimiter);
		}

        /// <summary>
        /// Perform the substition in a template.
        /// </summary>
        /// <param name="template">The template</param>
        /// <param name="variables">The substitution environment variables</param>
        /// <param name="delimiter">Variable delimiter in the template</param>
        /// <returns>The result of the substitution applied to the template</returns>
		public string Replace(string template, Dictionary<string,object> variables = null, string delimiter = null) {
			if ("@".Equals(delimiter)) throw new System.ArgumentException("Illegal delimiter: @");
			if (delimiter == null) delimiter = DEFAULT_DELIMITER;
            
            //Replace all variable prefix (delimier) by @Model
            //The @Model will contain all the data of the current page (that all substitution environment variables).
            template = template.Replace(delimiter, "@Model.");
            //Dynamically create @Model
			object model = variables == null ? new { } : CreateAnonymousObjectFromDictionary(variables);
			// TODO: make key unique
			// key should be unique because razor caches the result
			// so it can then be retrieved with only this key
			string key = md5(template);
			template = template.TrimStart('\r','\n').Replace("\r\n","\n").TrimEnd();
            //Create the subtituted code
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

        /// <summary>
        /// Dynamically create @Model
        /// </summary>
        /// <param name="properties">All subsitution variables values</param>
        /// <returns>An object object whose members can be dynamically added and removed at run time </returns>
		private object CreateAnonymousObjectFromDictionary(Dictionary<string,object> properties) {
			var obj = new System.Dynamic.ExpandoObject();
			var props = (ICollection<KeyValuePair<string, object>>)obj;
			foreach (var x in properties) props.Add(x);
			return obj;
		}

        /// <summary>
        /// Create a md5 unique key.
        /// </summary>
        /// <param name="input"></param>
        /// <returns></returns>
		private static string md5(string input) {
			var bytes = System.Text.Encoding.ASCII.GetBytes(input);
			var hash = System.Security.Cryptography.MD5.Create().ComputeHash(bytes);
			var res = new System.Text.StringBuilder();
			foreach (byte t in hash) res.Append(t.ToString("X2"));
            return res.ToString();
        }

        /// <summary>
        /// Allocate a RazorEngine substition solver
        /// </summary>
        /// <param name="template">The template to be applied</param>
        /// <param name="variables">The substitution environment variables</param>
        /// <param name="delimiter">Variable delimiter in the template</param>
        /// <returns>The Substitution solver instance</returns>
		public static Solver Create(string template, Dictionary<string,object> variables, string delimiter) {
			return new RazorEngine(template, variables ?? new Dictionary<string,object>(), delimiter ?? DEFAULT_DELIMITER);
		}
	}
}
