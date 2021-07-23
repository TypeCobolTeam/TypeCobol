using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using TypeCobol.Test;
using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    [TestClass]
    public class CfgDfaDominatorTests
    {
        [TestMethod]
        [TestCategory("Dominator")]
        public void HanoiPrgCfgExtendedDominator()
        {
            string path = Path.Combine(BasicCfgPrograms, "HanoiPrg.cbl");
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);
            var blocks_dominators = dfaResults.Graphs[0].ComputeDominators();

            string expectedDomsFile = Path.Combine(BasicCfgPrograms, "HanoiPrg.doms");
            StringWriter writer = new StringWriter();
            dfaResults.Graphs[0].DumpDominators(blocks_dominators.Item2, writer);
            // compare with expected result
            string result = writer.ToString();
            string expected = File.ReadAllText(expectedDomsFile);
            TestUtils.compareLines(path, result, expected, expectedDomsFile);
        }

        private static void HanoiPrgCfgExtendedImmediateDominator(bool duplicate)
        {
            string path = Path.Combine(BasicCfgPrograms, "HanoiPrg.cbl");
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);
            var blocks_dominators = dfaResults.Graphs[0].ComputeDominators();
            var idoms = dfaResults.Graphs[0].ComputeImmediateDominators(blocks_dominators.Item1, blocks_dominators.Item2, duplicate);

            string expectedDomsFile = Path.Combine(BasicCfgPrograms, "HanoiPrg.idoms");
            StringWriter writer = new StringWriter();
            dfaResults.Graphs[0].DumpDominators(idoms, writer);
            // compare with expected result
            string result = writer.ToString();
            string expected = File.ReadAllText(expectedDomsFile);
            TestUtils.compareLines(path, result, expected, expectedDomsFile);
        }

        [TestMethod]
        [TestCategory("ImmediateDominator")]
        public void HanoiPrgCfgExtendedImmediateDominatorDup()
        {
            HanoiPrgCfgExtendedImmediateDominator(true);
        }

        [TestMethod]
        [TestCategory("ImmediateDominator")]
        public void HanoiPrgCfgExtendedImmediateDominatorNoDup()
        {
            HanoiPrgCfgExtendedImmediateDominator(false);
        }
    }
}
