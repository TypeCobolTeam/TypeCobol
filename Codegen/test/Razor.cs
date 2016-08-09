using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons.Templates;
using TypeCobol.Compiler.CodeElements.Functions;
using TypeCobol.Codegen;

namespace TypeCobol.Codegen.Config {

	[TestClass]
	public class RazorTest {

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceSingleLetterVariables() {
			var variables = new Dictionary<string,object> { {"x","y"} };
			var solver = new RazorEngine();
			Assert.AreEqual("y", solver.Replace("%x", variables));
			Assert.AreEqual("y", solver.Replace("$x", variables, "$"));
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceWordVariables() {
			var variables = new Dictionary<string,object> { {"bar","fooo"} };
			var solver = new RazorEngine();
			Assert.AreEqual("fooo", solver.Replace("%bar", variables));
			Assert.AreEqual("fooo", solver.Replace("$bar", variables, "$"));
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceTwoVariables() {
			var variables = new Dictionary<string,object> { {"LEVEL","01"}, {"NAME","var"} };
			string input = "%LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			string expected = "01  var-value PIC X VALUE LOW-VALUE.\n  88  var       VALUE 'T'.\n  88  var-false VALUE 'F'.";
			Assert.AreEqual(expected, new RazorEngine().Replace(input, variables));
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceVariablesButOneIsMissing() {
			Dictionary<string,object> variables;
			string input = "%LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			variables = new Dictionary<string,object> { {"NAME", "var"} };
			try { new RazorEngine().Replace(input, variables); }
			catch (System.ArgumentException ex) { Assert.AreEqual("Variable \"LEVEL\" undefined", ex.Message); }
			variables = new Dictionary<string,object> { {"LEVEL", "01"} };
			try { new RazorEngine().Replace(input, variables); }
			catch (System.ArgumentException ex) { Assert.AreEqual("Variable \"NAME\" undefined", ex.Message); }
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceVariablesButBothAreMissing() {
			string input;
			input = "%LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			try { new RazorEngine().Replace(input); }
			catch (System.ArgumentException ex) { Assert.AreEqual("Variable \"LEVEL\" undefined", ex.Message); }
			input = "LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			try { new RazorEngine().Replace(input); }// there's only an error on the first undefined variable
			catch (System.ArgumentException ex) { Assert.AreEqual("Variable \"NAME\" undefined", ex.Message); }
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceFunctionPatterns() {
			var path = System.IO.Path.Combine("resources", "config", "TypeCobol","FUNCTION.xml");
			var parser = Config.CreateParser(System.IO.Path.GetExtension(path));
			var skeleton = parser.Parse(path)[0];
			Solver solver = new RazorEngine();
			string input, expected;
			var variables = new Dictionary<string,object> { {"function", RazorFactory.Create("fun", "mylib")} };

			input = skeleton.Patterns[0].Template;
			expected =
"01 mylibcpy COPY mylibcpy.\n"+
"01 mylib PIC X(08) VALUE 'mylib'.\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));

			input = skeleton.Patterns[1].Template;
			expected =
"01 RETURN-CODE PIC X(08).\n";
			Assert.AreEqual(expected, solver.Replace(input, null, "%"));

			input = skeleton.Patterns[2].Template;
//			expected =
//"01 fun-RESULT PIC 9(8).\n";
//			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));
//
//			input = skeleton.Patterns[3].Template;
			expected =
"IF mylibcpy-POINTER-TABLE = LOW_VALUE\n"+
"    CALL mylib USING mylibcpy\n"+
"END-IF\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));

			input = skeleton.Patterns[3].Template;
//			input = skeleton.Patterns[4].Template;
			variables = new Dictionary<string,object> { {"function", RazorFactory.CreateCall("fun", "mylib")}, {"receiver", "myresult"} };
			expected =
"CALL fun USING\n"+
"    BY REFERENCE param1\n"+
"    BY CONTENT '42'\n"+
"\n"+
"    BY REFERENCE RETURN-CODE\n"+
"    BY REFERENCE fun-RESULT\n"+
"\n"+
"IF RETURN-CODE = ZERO\n"+
"    MOVE fun-RESULT TO myresult\n"+
"ELSE\n"+
"*    TODO: error management\n"+
"END-IF\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));
		}

		private class RazorFactory {
			public static Function Create(string name, string library = "TC-DEFAULT") {
				return new Function(new TypeCobol.Compiler.CodeElements.Expressions.URI(library+"."+name),
					new List<ParameterDescription>() {
						new RazorParameter(null),
						new RazorParameter(null, 3),
					},
					new List<ParameterDescription>() {
						new RazorParameter(null, 8),
					});
			}
			public static Function CreateCall(string name, string library = "TC-DEFAULT") {
				return new Function(new TypeCobol.Compiler.CodeElements.Expressions.URI(library+"."+name),
					new List<ParameterDescription>() {
						new CallParameter {
								Value = "param1",
								ByReference = true,
							},
						new CallParameter {
								Value = "'42'",
								ByReference = false,
							},
					},
					new List<ParameterDescription>() {
						new RazorParameter(null, 8),
					});
			}
		}
		private class RazorParameter: ParameterDescription {
			public RazorParameter(string name, int length=int.MaxValue) {
				DataName = new GeneratedSymbolDefinition(name);
//				Picture = "PIC 9("+length+")";
			}
		}
		private class GeneratedSymbolDefinition: Compiler.CodeElements.SymbolDefinition {
			private string name;
			public GeneratedSymbolDefinition(string name): base(null) { this.name = name; }
			public override string Name { get { return name; } }
		}

	}
}
