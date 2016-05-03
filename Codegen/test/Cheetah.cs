using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;

namespace TypeCobol.Codegen.Config {

	[TestClass]
	public class CheetahTest {

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceSingleLetterVariables() {
			Assert.AreEqual(Cheetah.Replace("%x",       "x", "y", "%"), "y");
			Assert.AreEqual(Cheetah.Replace("$x",       "x", "y", "$"), "y");
			Assert.AreEqual(Cheetah.Replace("x%xxx%xx", "x", "y"), "xyxxyx");
			Assert.AreEqual(Cheetah.Replace("%%%xx%%",  "x", "y"), "%%yx%%");
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceWordVariables() {
			Assert.AreEqual(Cheetah.Replace("%bar",       "bar", "fooo", "%"), "fooo");
			Assert.AreEqual(Cheetah.Replace("$bar",       "bar", "fooo", "$"), "fooo");
			Assert.AreEqual(Cheetah.Replace("bar%barbarb%arba%r%barbar", "bar", "fooo"), "barfooobarb%arba%rfooobar");
			Assert.AreEqual(Cheetah.Replace("%%%barbar%%",  "bar", "fooo"), "%%fooobar%%");
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceVariables() {
			string input = "%LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			string expected = "01  var-value PIC X VALUE LOW-VALUE.\n  88  var       VALUE 'T'.\n  88  var-false VALUE 'F'.";
			Assert.AreEqual(Cheetah.Replace(input, new Dictionary<string,string>() { {"LEVEL","01"}, {"NAME","var"} }, "%"), expected);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceVariablesButFirstIsMissing() {
			string input = "%LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			string expected = "%LEVEL  var-value PIC X VALUE LOW-VALUE.\n  88  var       VALUE 'T'.\n  88  var-false VALUE 'F'.";
			Assert.AreEqual(Cheetah.Replace(input, new Dictionary<string,string>() { {"NAME","var"} }, "%"), expected);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ReplaceVariablesButSecondIsMissing() {
			string input = "%LEVEL  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			string expected = "01  %NAME-value PIC X VALUE LOW-VALUE.\n  88  %NAME       VALUE 'T'.\n  88  %NAME-false VALUE 'F'.";
			Assert.AreEqual(Cheetah.Replace(input, new Dictionary<string,string>() { {"LEVEL","01"} }, "%"), expected);
		}
	}
}
