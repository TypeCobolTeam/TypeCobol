using System.Collections.Generic;
using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;

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
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfg = ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfg.Count == 1);

            DefaultDataFlowGraphBuilder dfaBuilder = new DefaultDataFlowGraphBuilder(cfg[0]);
            dfaBuilder.ComputeUseList();
            Assert.AreEqual(1, dfaBuilder.UseList.Count);
            Assert.IsTrue(dfaBuilder.UseList[0].Instruction.CodeElement.Type == Compiler.CodeElements.CodeElementType.IfStatement);
            Assert.AreEqual(dfaBuilder.UseList[0].Variable.Name, "A");

            dfaBuilder.ComputeDefList();
            Assert.AreEqual(0, dfaBuilder.DefList.Count);
        }

        [TestMethod]
        public void SearchCond0()
        {
            string path = Path.Combine(BasicCfgInstrs, "SearchCond0.cbl");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfg = ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfg.Count == 1);

            DefaultDataFlowGraphBuilder dfaBuilder = new DefaultDataFlowGraphBuilder(cfg[0]);
            dfaBuilder.ComputeUseList();
            Assert.AreEqual(8, dfaBuilder.UseList.Count);
            string[] useVars = { "ELEM", "IDX", "NBJ", "IDX", "NUM", "LIB", "IDX", "IDX-END"};
            for (int i = 0; i < 8; i++)
            {
                Assert.AreEqual(dfaBuilder.UseList[i].Variable.Name, useVars[i]);
            }

            dfaBuilder.ComputeDefList();
            Assert.AreEqual(2, dfaBuilder.DefList.Count);
            Assert.AreEqual(dfaBuilder.DefList[0].Variable.Name, "IDX");
            Assert.AreEqual(dfaBuilder.DefList[1].Variable.Name, "NUM");
        }

        [TestMethod]
        public void MixPerformEvaluateIf0()
        {
            string path = Path.Combine(BasicCfgInstrs, "MixPerformEvaluateIf0.cbl");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs = ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfgs.Count == 1);

            DefaultDataFlowGraphBuilder dfaBuilder = new DefaultDataFlowGraphBuilder(cfgs[0]);
            dfaBuilder.ComputeUseList();
            Assert.AreEqual(0, cfgs[0].AllBlocks[0].Data.UseCount);
            Assert.AreEqual(0, cfgs[0].AllBlocks[1].Data.UseCount);
            Assert.AreEqual(0, cfgs[0].AllBlocks[2].Data.UseCount);
            Assert.AreEqual(0, cfgs[0].AllBlocks[3].Data.UseCount);
            Assert.AreEqual(0, cfgs[0].AllBlocks[4].Data.UseCount);
            Assert.AreEqual(1, cfgs[0].AllBlocks[5].Data.UseCount);
            Assert.AreEqual("ef", dfaBuilder.UseList[cfgs[0].AllBlocks[1].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(2, cfgs[0].AllBlocks[18].Data.UseCount);
            Assert.AreEqual("fct1", dfaBuilder.UseList[cfgs[0].AllBlocks[18].Data.UseListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", dfaBuilder.UseList[cfgs[0].AllBlocks[18].Data.UseListFirstIndex + 1].Variable.Name);

            Assert.AreEqual(1, cfgs[0].AllBlocks[22].Data.UseCount);
            Assert.AreEqual("ef", dfaBuilder.UseList[cfgs[0].AllBlocks[22].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(4, cfgs[0].AllBlocks[43].Data.UseCount);

            dfaBuilder.ComputeDefList();
            Assert.AreEqual(3, cfgs[0].AllBlocks[1].Data.DefCount);
            Assert.AreEqual("fct1", dfaBuilder.DefList[cfgs[0].AllBlocks[1].Data.DefListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", dfaBuilder.DefList[cfgs[0].AllBlocks[1].Data.DefListFirstIndex + 1].Variable.Name);
            Assert.AreEqual("zres", dfaBuilder.DefList[cfgs[0].AllBlocks[1].Data.DefListFirstIndex + 2].Variable.Name);

            Assert.AreEqual(1, cfgs[0].AllBlocks[5].Data.DefCount);
            Assert.AreEqual("ind", dfaBuilder.DefList[cfgs[0].AllBlocks[5].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, cfgs[0].AllBlocks[18].Data.DefCount);
            Assert.AreEqual("zres", dfaBuilder.DefList[cfgs[0].AllBlocks[18].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, cfgs[0].AllBlocks[22].Data.DefCount);
            Assert.AreEqual("ind", dfaBuilder.DefList[cfgs[0].AllBlocks[22].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, cfgs[0].AllBlocks[43].Data.DefCount);
            Assert.AreEqual("fct3", dfaBuilder.DefList[cfgs[0].AllBlocks[43].Data.DefListFirstIndex].Variable.Name);
        }
    }
}
