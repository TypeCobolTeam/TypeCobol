using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests testing CFG for Programs.
    /// </summary>
    [TestClass]
    public class BasicCfgProgramTest
    {
        [TestMethod]
        [TestCategory("BasicCfgProgram")]
        public void HanoiPrgCfgExtended()
        {
            string path = Path.Combine(BasicCfgPrograms, "HanoiPrg.cbl");
            var cfgs = ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfgs.Count == 1);

            string expectedPath = Path.Combine(BasicCfgPrograms, "HanoiPrg.dot");
            GenDotCfgAndCompare(cfgs[0], path, expectedPath, true);
        }
    }
}
