using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for control from instructions.
    /// </summary>
    [TestClass]
    public class DfaBuildUseAndDefListTest
    {
        [TestMethod]
        public void IfThenTest()
        {
            string path = Path.Combine(BasicCfgInstrs, "IfThen0.cbl");
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);

            var useList = dfaResults.GetUseList(dfaResults.Graphs[0]);
            Assert.AreEqual(1, useList.Count);
            Assert.IsTrue(useList[0].Instruction.CodeElement.Type == Compiler.CodeElements.CodeElementType.IfStatement);
            Assert.AreEqual(useList[0].Variable.Name, "A");

            var defList = dfaResults.GetDefList(dfaResults.Graphs[0]);
            Assert.AreEqual(1, defList.Count);
        }

        [TestMethod]
        public void SearchCond0()
        {
            string path = Path.Combine(BasicCfgInstrs, "SearchCond0.cbl");
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);

            var useList = dfaResults.GetUseList(dfaResults.Graphs[0]);
            Assert.AreEqual(9, useList.Count);
            string[] useVars = { "ELEM", "IDX", "NBJ", "IDX", "IDX", "NUM", "LIB", "IDX", "IDX-END"};
            for (int i = 0; i < 9; i++)
            {
                Assert.AreEqual(useList[i].Variable.Name, useVars[i]);
            }

            var defList = dfaResults.GetDefList(dfaResults.Graphs[0]);
            Assert.AreEqual(2, defList.Count);
            Assert.AreEqual(defList[0].Variable.Name, "IDX");
            Assert.AreEqual(defList[1].Variable.Name, "NUM");
        }

        [TestMethod]
        public void MixPerformEvaluateIf0()
        {
            string path = Path.Combine(BasicCfgInstrs, "MixPerformEvaluateIf0.cbl");
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);

            var cfg = dfaResults.Graphs[0];
            Assert.AreEqual(0, cfg.AllBlocks[0].Data.UseCount);
            Assert.AreEqual(0, cfg.AllBlocks[1].Data.UseCount);
            Assert.AreEqual(0, cfg.AllBlocks[2].Data.UseCount);
            Assert.AreEqual(0, cfg.AllBlocks[3].Data.UseCount);
            Assert.AreEqual(0, cfg.AllBlocks[4].Data.UseCount);
            Assert.AreEqual(1, cfg.AllBlocks[5].Data.UseCount);

            var useList = dfaResults.GetUseList(cfg);
            Assert.AreEqual("ef", useList[cfg.AllBlocks[1].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(2, cfg.AllBlocks[18].Data.UseCount);
            Assert.AreEqual("fct1", useList[cfg.AllBlocks[18].Data.UseListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", useList[cfg.AllBlocks[18].Data.UseListFirstIndex + 1].Variable.Name);

            Assert.AreEqual(1, cfg.AllBlocks[22].Data.UseCount);
            Assert.AreEqual("ef", useList[cfg.AllBlocks[22].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(4, cfg.AllBlocks[43].Data.UseCount);

            var defList = dfaResults.GetDefList(cfg);
            Assert.AreEqual(3, cfg.AllBlocks[1].Data.DefCount);
            Assert.AreEqual("fct1", defList[cfg.AllBlocks[1].Data.DefListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", defList[cfg.AllBlocks[1].Data.DefListFirstIndex + 1].Variable.Name);
            Assert.AreEqual("zres", defList[cfg.AllBlocks[1].Data.DefListFirstIndex + 2].Variable.Name);

            Assert.AreEqual(1, cfg.AllBlocks[5].Data.DefCount);
            Assert.AreEqual("ind", defList[cfg.AllBlocks[5].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, cfg.AllBlocks[18].Data.DefCount);
            Assert.AreEqual("zres", defList[cfg.AllBlocks[18].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, cfg.AllBlocks[22].Data.DefCount);
            Assert.AreEqual("ind", defList[cfg.AllBlocks[22].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, cfg.AllBlocks[43].Data.DefCount);
            Assert.AreEqual("fct3", defList[cfg.AllBlocks[43].Data.DefListFirstIndex].Variable.Name);
        }
    }
}
