using System;
using System.Reflection;
using System.Text;

namespace TypeCobol.Compiler.Parser {
	public class CodeGenerator {

		/// <summary>
		/// 
		/// </summary>
		/// <param name="composite">Composite grammar builder class (eg. TypeCobolBuilder)</param>
		/// <param name="imported">Imported grammar builder class (eg. CodeElementBuilder)</param>
		public StringBuilder GenerateCompositeBuilder(Type composite, Type imported) {
			var im = imported.GetMethods(BindingFlags.Public|BindingFlags.Instance);
			StringBuilder text = new StringBuilder();
			GenerateBegin(text, composite);
			var cm = composite.GetMethods(BindingFlags.Public|BindingFlags.Instance|BindingFlags.DeclaredOnly);
			foreach(var method in im) {
				// don't generate body for manually re-implemented methods
				if (Contains(cm, method)) continue;
				// only generate body for context-specific methods
				if (method.Name.StartsWith("Enter")
				 || method.Name.StartsWith("Exit"))
				GenerateCode(text, method);
			}
			GenerateEnd(text);
			return text;
		}

		private bool Contains(MethodInfo[] methods, MethodInfo method) {
			foreach(var m in methods)
				if (m.Name == method.Name) return true;
			return false;
		}

		/// <summary>Namespace and class header.</summary>
		private void GenerateBegin(StringBuilder text, Type composite) {
			text.Append("namespace ").Append(composite.Namespace).Append('{').AppendLine();
			text.Append("\tinternal partial class ").Append(composite.Name).Append('{').AppendLine();
		}

		/// <summary>Method definition.</summary>
		private void GenerateCode(StringBuilder text, MethodInfo method) {
			var args = new StringBuilder();
			text.Append("\t\t");
			text.Append(method.ReturnType.Name.ToLower()).Append(' ').Append(method.Name).Append('(');
			foreach(var arg in method.GetParameters()) {
				text.Append(arg.ParameterType.Name).Append(' ').Append(arg.Name).Append(',');
				args.Append(arg.Name).Append(',');
			}
			if (method.GetParameters().Length > 0 ) {
				text.Length -= 1;
				args.Length -= 1;
			}
			text.Append(") {").AppendLine();
			text.Append("\t\t\tBuilder.").Append(method.Name).Append('(').Append(args).Append(");").AppendLine();
			text.Append("\t\t}").AppendLine();
		}

		/// <summary>Close class and namespace.</summary>
		private void GenerateEnd(StringBuilder text) {
			text.Append("\t}").AppendLine().Append('}').AppendLine();
		}
	}
}
