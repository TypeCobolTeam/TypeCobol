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
        public static TypeCobolConfiguration DefaultConfig = null;
        public static ProgramSymbolTableBuilder Builder = null;
        public static NodeListenerFactory BuilderNodeListenerFactory = null;
        public static NodeListenerFactory CfgBuilderNodeListenerFactory = null;
        public static string DefaultIntrinsicPath = null;

        //A Cfg for DataFlow Analysis on Node
        public static DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Node>> CfgBuilder;

        [TestInitialize]
        public void TestInitialize()
        {
            //Create a default configurations for options
            DefaultConfig = new TypeCobolConfiguration();
            if (File.Exists(DefaultIntrinsicPath))
            {
                DefaultConfig.Copies.Add(DefaultIntrinsicPath);
            }

            //DefaultConfig.Dependencies.Add(Path.Combine(Directory.GetCurrentDirectory(), "resources", "dependencies"));
            SymbolTableBuilder.Config = DefaultConfig;

            //Force the creation of the Global Symbol Table
            var global = SymbolTableBuilder.Root;

            //Allocate a static Program Symbol Table Builder
            BuilderNodeListenerFactory = () =>
            {
                Builder = new ProgramSymbolTableBuilder();
                return Builder;
            };
            NodeDispatcher.RegisterStaticNodeListenerFactory(BuilderNodeListenerFactory);

            //Alocate a static Default Control Flow Graph Builder
            CfgBuilderNodeListenerFactory = () =>
            {
                CfgBuilder = new DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Node>>();
                return CfgBuilder;
            };
            NodeDispatcher.RegisterStaticNodeListenerFactory(CfgBuilderNodeListenerFactory);
        }

        private static void RemovePrograms(ProgramSymbol prog)
        {
            foreach (var nestPrg in prog.Programs)
            {
                SymbolTableBuilder.Root.RemoveProgram(prog);
                RemovePrograms(nestPrg);
            }
            SymbolTableBuilder.Root.RemoveProgram(prog);
        }

        [TestCleanup]
        public void TestCleanup()
        {
            if (BuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(BuilderNodeListenerFactory);
                if (Builder.Programs.Count != 0)
                {
                    foreach (var prog in Builder.Programs)
                    {
                        RemovePrograms(prog);
                    }
                }
            }
        }

        [TestMethod]
        public void IfThenTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "IfThen0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "IfThen0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(CfgBuilder.Cfg);
            dfaBuilder.ComputeUseList();
            Assert.IsTrue(dfaBuilder.UseList.Count == 1);
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
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "SearchCond0.dot");

            Assert.AreEqual(1, CfgBuilder.AllCfgBuilder.Count);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(CfgBuilder.Cfg);
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
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "MixPeformEvaluateIf0.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(CfgBuilder.Cfg);
            dfaBuilder.ComputeUseList();
            Assert.AreEqual(0, CfgBuilder.Cfg.AllBlocks[0].Data.UseCount);
            Assert.AreEqual(0, CfgBuilder.Cfg.AllBlocks[1].Data.UseCount);
            Assert.AreEqual(0, CfgBuilder.Cfg.AllBlocks[2].Data.UseCount);
            Assert.AreEqual(0, CfgBuilder.Cfg.AllBlocks[3].Data.UseCount);
            Assert.AreEqual(0, CfgBuilder.Cfg.AllBlocks[4].Data.UseCount);
            Assert.AreEqual(1, CfgBuilder.Cfg.AllBlocks[5].Data.UseCount);
            Assert.AreEqual("ef", dfaBuilder.UseList[CfgBuilder.Cfg.AllBlocks[1].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(2, CfgBuilder.Cfg.AllBlocks[18].Data.UseCount);
            Assert.AreEqual("fct1", dfaBuilder.UseList[CfgBuilder.Cfg.AllBlocks[18].Data.UseListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", dfaBuilder.UseList[CfgBuilder.Cfg.AllBlocks[18].Data.UseListFirstIndex + 1].Variable.Name);

            Assert.AreEqual(1, CfgBuilder.Cfg.AllBlocks[22].Data.UseCount);
            Assert.AreEqual("ef", dfaBuilder.UseList[CfgBuilder.Cfg.AllBlocks[22].Data.UseListFirstIndex].Variable.Name);

            Assert.AreEqual(4, CfgBuilder.Cfg.AllBlocks[43].Data.UseCount);

            dfaBuilder.ComputeDefList();
            Assert.AreEqual(3, CfgBuilder.Cfg.AllBlocks[1].Data.DefCount);
            Assert.AreEqual("fct1", dfaBuilder.DefList[CfgBuilder.Cfg.AllBlocks[1].Data.DefListFirstIndex].Variable.Name);
            Assert.AreEqual("fct2", dfaBuilder.DefList[CfgBuilder.Cfg.AllBlocks[1].Data.DefListFirstIndex + 1].Variable.Name);
            Assert.AreEqual("zres", dfaBuilder.DefList[CfgBuilder.Cfg.AllBlocks[1].Data.DefListFirstIndex + 2].Variable.Name);

            Assert.AreEqual(1, CfgBuilder.Cfg.AllBlocks[5].Data.DefCount);
            Assert.AreEqual("ind", dfaBuilder.DefList[CfgBuilder.Cfg.AllBlocks[5].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, CfgBuilder.Cfg.AllBlocks[18].Data.DefCount);
            Assert.AreEqual("zres", dfaBuilder.DefList[CfgBuilder.Cfg.AllBlocks[18].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, CfgBuilder.Cfg.AllBlocks[22].Data.DefCount);
            Assert.AreEqual("ind", dfaBuilder.DefList[CfgBuilder.Cfg.AllBlocks[22].Data.DefListFirstIndex].Variable.Name);

            Assert.AreEqual(1, CfgBuilder.Cfg.AllBlocks[43].Data.DefCount);
            Assert.AreEqual("fct3", dfaBuilder.DefList[CfgBuilder.Cfg.AllBlocks[43].Data.DefListFirstIndex].Variable.Name);
        }
    }
}
