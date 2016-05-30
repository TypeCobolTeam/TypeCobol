﻿using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using TypeCobol.Codegen.Skeletons.Templates;
using TypeCobol.Compiler.CodeElements.Functions;

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
			var variables = new Dictionary<string,object> { {"function", SampleFactory.Create("fun", "mylib")} };

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
			expected = 
"01 fun-RESULT PIC X(8).\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));

			input = skeleton.Patterns[3].Template;
			expected = 
"IF mylibcpy-POINTER-TABLE = LOW_VALUE\n"+
"    CALL mylib USING mylibcpy\n"+
"END-IF\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));

			input = skeleton.Patterns[4].Template;
			variables = new Dictionary<string,object> { {"function", SampleFactory.CreateCall("fun", "mylib")} };
			expected = "\n"+
"CALL fun USING\n"+
"    BY REFERENCE param1\n"+
"    BY CONTENT '42'\n"+
"\n"+
"    BY REFERENCE RETURN-CODE\n"+
"    BY REFERENCE fun-RESULT\n"+
"\n"+
"IF RETURN-CODE = ZERO\n"+
"* TODO original statement like MOVE or stuff\n"+
"ELSE\n"+
"*    TODO: error management\n"+
"END-IF\n";
			Assert.AreEqual(expected, solver.Replace(input, variables, "%"));
		}
	}
}
