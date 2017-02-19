using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

namespace TypeCobol.Test {

	[TestClass]
	public class NistTest
    {
        [TestMethod]
		[TestCategory("Parsing")]
		[TestProperty("Time","long")]
		public void CheckNistTest() {
			string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "Nist";
			string root = PlatformUtils.GetPathForProjectFile(samples);
            GrammarTest.CheckTests(root, @"NistTest", @"CheckNistResults.txt");
        }
	}
}
