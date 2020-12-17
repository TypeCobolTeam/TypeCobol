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
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);

            string expectedPath = Path.Combine(BasicCfgPrograms, "HanoiPrg.dot");
            GenDotCfgAndCompare(dfaResults.Graphs[0], path, expectedPath, true);
        }
    }
}
