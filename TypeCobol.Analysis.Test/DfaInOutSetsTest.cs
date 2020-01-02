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
    /// These are basic tests for IN and OUT sets building.
    /// </summary>
    [TestClass]
    public class DfaInOutSetsTest
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
        public void IN_OUT_SETS_SampleGotos0()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicDfaSamples", "SampleGotos0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            Assert.IsTrue(ctx.CfgDfaBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);

            //Create a Dot File Generator            
            CfgDotFileForNodeGenerator<DfaBasicBlockInfo<Symbol>> dotGen = new CfgDotFileForNodeGenerator<DfaBasicBlockInfo<Symbol>>(ctx.CfgDfaBuilder.Cfg, false);
            dotGen.FullInstruction = true;
            StringWriter writer = new StringWriter();
            dotGen.Report(writer);

            // Generate teh dot of the sample
            string result = writer.ToString();

            //Resolve variable I,J
            var multiI = document.Results.PrgSymbolTblBuilder.Programs[0].ResolveReference(new string[] { "I" }, true);
            Assert.AreEqual(1, multiI.Count);
            Symbol I = multiI.Symbol;

            var multiJ = document.Results.PrgSymbolTblBuilder.Programs[0].ResolveReference(new string[] { "J" }, true);
            Assert.AreEqual(1, multiJ.Count);
            Symbol J = multiJ.Symbol;

            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(ctx.CfgDfaBuilder.Cfg);
            dfaBuilder.ComputeInOutSet();

            //------------------------------------
            // All definitions
            // d(0) : MOVE 2 TO I
            // d(1) : MOVE 1 TO J
            // d(2) : ADD 1 TO J
            // d(3) : MOVE 1 TO I
            // d(4) : ADD 1 TO J
            // d(5) : SUBSTRACT 4 FROM J
            //-----------------------------------
            // Block(0)
            //-----------------------------------
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[0].Data.In.ToString());
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[0].Data.Out.ToString());

            //-----------------------------------
            // Block(1)
            //-----------------------------------
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[1].Data.In.ToString());
            Assert.AreEqual("{0, 2}", dfaBuilder.Cfg.AllBlocks[1].Data.Out.ToString());

            //-----------------------------------
            // Block(2)
            //-----------------------------------
            Assert.AreEqual("{0, 2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[2].Data.In.ToString());
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[2].Data.Out.ToString());

            //-----------------------------------
            // Block(3)
            //-----------------------------------
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[3].Data.In.ToString());
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[3].Data.Out.ToString());

            //-----------------------------------
            // Block(4)
            //-----------------------------------
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[4].Data.In.ToString());
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[4].Data.Out.ToString());

            //-----------------------------------
            // Block(5)
            //-----------------------------------
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[5].Data.In.ToString());
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[5].Data.Out.ToString());

            //-----------------------------------
            // Block(6)
            //-----------------------------------
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[6].Data.In.ToString());
            Assert.AreEqual("{3, 4}", dfaBuilder.Cfg.AllBlocks[6].Data.Out.ToString());

            //-----------------------------------
            // Block(7)
            //-----------------------------------
            Assert.AreEqual("{3, 4}", dfaBuilder.Cfg.AllBlocks[7].Data.In.ToString());
            Assert.AreEqual("{3, 4}", dfaBuilder.Cfg.AllBlocks[7].Data.Out.ToString());

            //-----------------------------------
            // Block(8)
            //-----------------------------------
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[8].Data.In.ToString());
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[8].Data.Out.ToString());

            //-----------------------------------
            // Block(9)
            //-----------------------------------
            Assert.AreEqual("{3, 4}", dfaBuilder.Cfg.AllBlocks[9].Data.In.ToString());
            Assert.AreEqual("{3, 5}", dfaBuilder.Cfg.AllBlocks[9].Data.Out.ToString());

            //-----------------------------------
            // Block(10)
            //-----------------------------------
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[10].Data.In.ToString());
            Assert.AreEqual("{2, 3, 4, 5}", dfaBuilder.Cfg.AllBlocks[10].Data.Out.ToString());

            //-----------------------------------
            // Block(11)
            //-----------------------------------
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[11].Data.In.ToString());
            Assert.AreEqual("{}", dfaBuilder.Cfg.AllBlocks[11].Data.Out.ToString());
        }
    }
}
