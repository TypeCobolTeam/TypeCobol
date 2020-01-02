using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Cfg;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for control from instructions.
    /// </summary>
    [TestClass]
    public class DfaBuildUseAndDefListTest
    {
        public static CfgDfaTestContext ctx = null;
        [TestInitialize]
        public void TestInitialize()
        {
            ctx = new CfgDfaTestContext(CfgDfaTestContext.Mode.Dfa);
            ctx.TestInitialize();
        }

        [TestCleanup]
        public void TestCleanup()
        {
            ctx.TestCleanup();
        }

        [TestMethod]
        public void IfThenTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThen0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThen0.dot");

            Assert.IsTrue(ctx.CfgDfaBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(ctx.CfgDfaBuilder.Cfg);
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
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "SearchCond0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "SearchCond0.dot");

            Assert.AreEqual(1, ctx.CfgDfaBuilder.AllCfgBuilder.Count);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(ctx.CfgDfaBuilder.Cfg);
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
        public void MixPeformEvaluateIf0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "MixPeformEvaluateIf0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "MixPeformEvaluateIf0.dot");

            Assert.IsTrue(ctx.CfgDfaBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(ctx.CfgDfaBuilder.Cfg);
            dfaBuilder.ComputeUseList();
            Assert.AreEqual(0, ctx.CfgDfaBuilder.Cfg.AllBlocks[0].Data.UseCount);
            Assert.AreEqual(0, ctx.CfgDfaBuilder.Cfg.AllBlocks[1].Data.UseCount);
            Assert.AreEqual(0, ctx.CfgDfaBuilder.Cfg.AllBlocks[2].Data.UseCount);
            Assert.AreEqual(0, ctx.CfgDfaBuilder.Cfg.AllBlocks[3].Data.UseCount);
            Assert.AreEqual(0, ctx.CfgDfaBuilder.Cfg.AllBlocks[4].Data.UseCount);
            Assert.AreEqual(1, ctx.CfgDfaBuilder.Cfg.AllBlocks[5].Data.UseCount);
            Assert.AreEqual("ef", dfaBuilder.UseList[ctx.CfgDfaBuilder.Cfg.AllBlocks[1].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(2, ctx.CfgDfaBuilder.Cfg.AllBlocks[18].Data.UseCount);
            Assert.AreEqual("fct1", dfaBuilder.UseList[ctx.CfgDfaBuilder.Cfg.AllBlocks[18].Data.UseListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", dfaBuilder.UseList[ctx.CfgDfaBuilder.Cfg.AllBlocks[18].Data.UseListFirstIndex + 1].Variable.Name);

            Assert.AreEqual(1, ctx.CfgDfaBuilder.Cfg.AllBlocks[22].Data.UseCount);
            Assert.AreEqual("ef", dfaBuilder.UseList[ctx.CfgDfaBuilder.Cfg.AllBlocks[22].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(4, ctx.CfgDfaBuilder.Cfg.AllBlocks[43].Data.UseCount);

            dfaBuilder.ComputeDefList();
            Assert.AreEqual(3, ctx.CfgDfaBuilder.Cfg.AllBlocks[1].Data.DefCount);
            Assert.AreEqual("fct1", dfaBuilder.DefList[ctx.CfgDfaBuilder.Cfg.AllBlocks[1].Data.DefListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", dfaBuilder.DefList[ctx.CfgDfaBuilder.Cfg.AllBlocks[1].Data.DefListFirstIndex + 1].Variable.Name);
            Assert.AreEqual("zres", dfaBuilder.DefList[ctx.CfgDfaBuilder.Cfg.AllBlocks[1].Data.DefListFirstIndex + 2].Variable.Name);

            Assert.AreEqual(1, ctx.CfgDfaBuilder.Cfg.AllBlocks[5].Data.DefCount);
            Assert.AreEqual("ind", dfaBuilder.DefList[ctx.CfgDfaBuilder.Cfg.AllBlocks[5].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, ctx.CfgDfaBuilder.Cfg.AllBlocks[18].Data.DefCount);
            Assert.AreEqual("zres", dfaBuilder.DefList[ctx.CfgDfaBuilder.Cfg.AllBlocks[18].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, ctx.CfgDfaBuilder.Cfg.AllBlocks[22].Data.DefCount);
            Assert.AreEqual("ind", dfaBuilder.DefList[ctx.CfgDfaBuilder.Cfg.AllBlocks[22].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, ctx.CfgDfaBuilder.Cfg.AllBlocks[43].Data.DefCount);
            Assert.AreEqual("fct3", dfaBuilder.DefList[ctx.CfgDfaBuilder.Cfg.AllBlocks[43].Data.DefListFirstIndex].Variable.Name);
        }
    }
}
