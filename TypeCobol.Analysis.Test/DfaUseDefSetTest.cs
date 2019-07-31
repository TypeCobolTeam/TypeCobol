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
using TypeCobol.Analysis.Graph;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests for UseDef set building.
    /// </summary>
    [TestClass]
    public class DfaUseDefSetTest
    {
        public static TypeCobolConfiguration DefaultConfig = null;
        public static ProgramSymbolTableBuilder Builder = null;
        public static NodeListenerFactory BuilderNodeListenerFactory = null;
        public static NodeListenerFactory CfgBuilderNodeListenerFactory = null;
        public static string DefaultIntrinsicPath = null;

        //A Cfg for DataFlow Analysis on Node
        public static DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<Symbol>> CfgBuilder;

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
                NodeDispatcher.RegisterStaticNodeListenerFactory(CfgBuilderNodeListenerFactory);
            }
        }

        /// <summary>
        /// This test check the GEN set of the sample SampleGotos0.cbl according to its corresponding
        /// dot graph.
        /// 
        //digraph Cfg {
        //    node[
        //    shape = "record"
        //    ]
        //edge[
        //arrowtail = "empty"
        //]
        //            Block0[
        //            label = "{START|}"
        //            ]
        //            Block1[
        //            label = "{L1. Block1|    MOVE 2 TO I\l    MOVE I TO J\l    ADD 1 TO J\l}"
        //            ]
        //            Block2[
        //            label = "{L2. Block2|    MOVE 1 TO I\l    IF J = 999 THEN\l}"
        //            ]
        //            Block3[
        //            label = "{Block3|         GO TO L4\l}"
        //            ]
        //            Block10[
        //            label = "{L4. Block10|    GO TO L2\l}"
        //            ]
        //            Block5[
        //            label = "{Block5|}"
        //            ]
        //            Block6[
        //            label = "{L3. Block6|    ADD 1 TO J\l    IF J = 999 THEN\l}"
        //            ]
        //            Block7[
        //            label = "{Block7|         GO TO L4\l}"
        //            ]
        //            Block9[
        //            label = "{Block9|    SUBTRACT 4 FROM J\l}"
        //            ]
        //            Block0->Block1
        //            Block1->Block2
        //            Block2->Block3
        //            Block2->Block5
        //            Block3->Block10
        //            Block10->Block2
        //            Block5->Block6
        //            Block6->Block7
        //            Block6->Block9
        //            Block7->Block10
        //            Block9->Block10
        //}
        /// </summary>
        [TestMethod]
        public void USE_DEF_SET_SampleGotos0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicDfaSamples", "SampleGotos0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(Builder.Programs.Count == 1);
            Assert.IsTrue(CfgBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(CfgBuilder.AllCfgBuilder);

            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<DfaBasicBlockInfo<Symbol>> dotGen = new CfgDotFileForNodeGenerator<DfaBasicBlockInfo<Symbol>>(CfgBuilder.Cfg, false);
            dotGen.FullInstruction = true;
            StringWriter writer = new StringWriter();
            dotGen.Report(writer);

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(CfgBuilder.Cfg);
            dfaBuilder.ComputeUseDefSet();

            //------------------------------------
            // All definitions
            // d(0) : MOVE 2 TO I
            // d(1) : MOVE 1 TO J
            // d(2) : ADD 1 TO J
            // d(3) : MOVE 1 TO I
            // d(4) : ADD 1 TO J
            // d(5) : SUBSTRACT 4 FROM J
            //-----------------------------------

            //Resolve variable I,J
            var multiI = Builder.Programs[0].ResolveReference(new string[] { "I" }, true);
            Assert.AreEqual(1, multiI.Count);
            Symbol I = multiI.Symbol;

            var multiJ = Builder.Programs[0].ResolveReference(new string[] { "J" }, true);
            Assert.AreEqual(1, multiJ.Count);
            Symbol J = multiJ.Symbol;

            //Check Var J definitions
            Assert.AreEqual("{0, 3}", dfaBuilder.VariableDefMap[I].ToString());

            //Check Var J definitions
            Assert.AreEqual("{1, 2, 4, 5}", dfaBuilder.VariableDefMap[J].ToString());

            //--------------------------
            // VAR : I
            // Block(1)
            //--------------------------
            Assert.AreEqual(1, dfaBuilder.UseList[0].BlockIndex);
            Assert.AreEqual(I, dfaBuilder.UseList[0].Variable);
            Assert.AreEqual("{0}", dfaBuilder.UseList[0].UseDef.ToString());

            //--------------------------
            // VAR : J
            // Block(2)
            //--------------------------
            Assert.AreEqual(2, dfaBuilder.UseList[1].BlockIndex);
            Assert.AreEqual(J, dfaBuilder.UseList[1].Variable);
            Assert.AreEqual("{2, 4, 5}", dfaBuilder.UseList[1].UseDef.ToString());

            //--------------------------
            // VAR : J
            // Block(6)
            //--------------------------
            Assert.AreEqual(6, dfaBuilder.UseList[2].BlockIndex);
            Assert.AreEqual(J, dfaBuilder.UseList[2].Variable);
            Assert.AreEqual("{4}", dfaBuilder.UseList[2].UseDef.ToString());
        }
    }
}
