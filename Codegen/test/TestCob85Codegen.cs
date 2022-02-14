using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

namespace TypeCobol.Codegen.Test {

	[TestClass]
	public class TestCob85Codegen
    {
		[TestMethod]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseFileControl() {
			string file = Path.Combine("Cobol85","FileControl");
            CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl");
		}

        [TestMethod]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseCopyNotExpanded() {
			string file = Path.Combine("Cobol85","CopyNotExpanded");
            CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl");
		}
    }
}
