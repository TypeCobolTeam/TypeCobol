using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Cfg;
using TypeCobol.Analysis.Dfa;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests testing CFG for Programs.
    /// </summary>
    [TestClass]
    public class BasicCfgProgramTest
    {
        public static TypeCobolConfiguration DefaultConfig = null;
        public static ProgramSymbolTableBuilder Builder = null;
        public static NodeListenerFactory BuilderNodeListenerFactory = null;
        public static NodeListenerFactory CfgBuilderNodeListenerFactory = null;
        public static string DefaultIntrinsicPath = null;        

        //A Cfg for DataFlow Analysis on Node
        public static DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>> CfgBuilder;

        //DFA uses the Extend mode
        public static ControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>>.CfgMode cfgMode = ControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>>.CfgMode.Extended;

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
                CfgBuilder = new DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>>();
                CfgBuilder.Mode = cfgMode;
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
            if (CfgBuilderNodeListenerFactory != null)
            {
                NodeDispatcher.RemoveStaticNodeListenerFactory(CfgBuilderNodeListenerFactory);
            }
        }

        [TestMethod]
        [TestCategory("BasicCfgProgram")]
        public void HanoiPrgCfgExtended()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgPrograms", "HanoiPrg.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(Builder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "CfgPrograms", "HanoiPrg.dot");

            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            CfgTestUtils.GenericDotCfgAndCompare(CfgBuilder.Cfg, path, expectedPath, true);
        }

    }
}
