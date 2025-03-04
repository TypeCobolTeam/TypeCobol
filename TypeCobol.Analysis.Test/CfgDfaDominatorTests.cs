﻿using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test;

using CFG = TypeCobol.Analysis.Graph.ControlFlowGraph<TypeCobol.Compiler.Nodes.Node, TypeCobol.Analysis.Dfa.DfaBasicBlockInfo<TypeCobol.Compiler.Symbols.VariableSymbol>>;

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
            CFG.DumpDominators(blocks_dominators.Item2, writer);
            // compare with expected result
            string result = writer.ToString();
            TestUtils.CompareContent(path, result, new TestUtils.FileInfo(expectedDomsFile));
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
            CFG.DumpDominators(idoms, writer);
            // compare with expected result
            string result = writer.ToString();
            TestUtils.CompareContent(path, result, new TestUtils.FileInfo(expectedDomsFile));
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
