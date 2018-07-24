using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.Globalization;
using System.Threading;
using TypeCobol.Codegen.Skeletons.Templates;
using TypeCobol.Compiler.CodeElements;
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
            Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
            Thread.CurrentThread.CurrentUICulture = CultureInfo.InvariantCulture;

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
            Thread.CurrentThread.CurrentCulture = CultureInfo.InvariantCulture;
            Thread.CurrentThread.CurrentUICulture = CultureInfo.InvariantCulture;
            string input;
			input = "%LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			try { new RazorEngine().Replace(input); }
			catch (System.ArgumentException ex) { Assert.AreEqual("Variable \"LEVEL\" undefined", ex.Message); }
			input = "LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			try { new RazorEngine().Replace(input); }// there's only an error on the first undefined variable
			catch (System.ArgumentException ex) { Assert.AreEqual("Variable \"NAME\" undefined", ex.Message); }
		}
/*
		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceFunctionPatterns() {
			var path = System.IO.Path.Combine("resources", "config", "TypeCobol","ALL.xml");
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
"01 ERROR-CODE PIC X(08).\n";
			Assert.AreEqual(expected, solver.Replace(input, null, "%"));

			input = skeleton.Patterns[2].Template;
//			expected =
//"01 fun-RESULT PIC 9(8).\n";
//			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));
//
//			input = skeleton.Patterns[3].Template;
			expected =
"    IF mylibcpy-POINTER-TABLE = LOW_VALUE\n"+
"        CALL mylib USING mylibcpy\n"+
"    END-IF\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));

			input = skeleton.Patterns[3].Template;
//			input = skeleton.Patterns[4].Template;
			variables = new Dictionary<string,object> { {"function", RazorFactory.CreateCall("fun", "mylib")}, {"receiver", "myresult"} };
			expected =
"    CALL fun USING\n"+
"        BY REFERENCE param1\n"+
"        BY CONTENT '42'\n"+
"\n"+
"        BY REFERENCE fun-RESULT\n"+
"        BY REFERENCE ERROR-CODE\n"+
"\n"+
"    IF ERROR-CODE = ZERO\n"+
"        MOVE fun-RESULT TO myresult\n"+
"    ELSE\n"+
"*        TODO: error management\n"+
"    END-IF\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));//TODO#249: refactor parameters and make the objects specific to this test less ... specific
		}
*/
		private class RazorFactory {
			public static Function Create(string name, string library = "TC-DEFAULT") {
				return new Function(new TypeCobol.Compiler.CodeElements.Expressions.URI(new List<string>() {library, name}), 
					new List<ParameterDescriptionEntry>() {
						new RazorParameter(null),
						new RazorParameter(null, 3),
					},
					new List<ParameterDescriptionEntry>() {
						new RazorParameter(null, 8),
					});
			}
/*TODO#249
			public static Function CreateCall(string name, string library = "TC-DEFAULT") {
				return new Function(new TypeCobol.Compiler.CodeElements.Expressions.URI(library+"."+name),
					new List<ParameterDescription>() {
						new ParameterDescription(new CallParameterDescription {
								Value = "param1",
								ByReference = true,
							}),
						new ParameterDescription(new CallParameterDescription {
								Value = "'42'",
								ByReference = false,
							}),
					},
					new List<ParameterDescription>() {
						new ParameterDescription(new RazorParameter(null, 8)),
					});
			}
 */
		}

		private class RazorParameter: ParameterDescriptionEntry {
			public RazorParameter(string name, int length=int.MaxValue) {
				DataName = new GeneratedSymbolDefinition(name);
//				Picture = "PIC 9("+length+")";
			}
		}
		private class GeneratedSymbolDefinition: Compiler.CodeElements.SymbolDefinition {
			private string name;
			public GeneratedSymbolDefinition(string name): base(null, SymbolType.TO_BE_RESOLVED) { this.name = name; }
			public override string Name { get { return name; } }
		}

	}
}
