using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

namespace TypeCobol.Test {

	[TestClass]
	public class ThirdParty
    {
        [TestMethod]
		[TestCategory("Parsing")]
		[TestProperty("Time","long")]
		public void CNAF() {
			string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "CNAF" + Path.DirectorySeparatorChar + "Batch";
			string root = PlatformUtils.GetPathForProjectFile(samples);
            GrammarTest.CheckTests(root, @"CNAFTest", @"CNAF.txt", ignoreWarningDiag: true, regex: "*");

        }

        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "long")]
        public void NistTest()
        {
            string samples = @"ThirdParty" + Path.DirectorySeparatorChar + "Nist";
            string root = PlatformUtils.GetPathForProjectFile(samples);
            GrammarTest.CheckTests(root, @"NistTest", @"CheckNistResults.txt", ignoreWarningDiag: true);

        }
    }
}
