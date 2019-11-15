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
using TypeCobol.Analysis.Graph;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are simple tests to validate Cfg/Dfa constructions with miscelaneous bugs.
    /// </summary>
    [TestClass]
    public class CfgBuildTests
    {
        public static CfgDfaTestContext ctx = null;
        [TestInitialize]
        public void TestInitialize()
        {
            //Cfg/Dfa context In Dfa mode
            ctx = new CfgDfaTestContext(CfgDfaTestContext.Mode.Dfa);
            ctx.TestInitialize();
        }

        [TestCleanup]
        public void TestCleanup()
        {
            ctx.TestCleanup();
        }

        /// <summary>
        /// Class to listen that and ExecSqlStatement has been seen.
        /// </summary>
        class ExecNodeListener : NodeListener
        {
            internal bool ExecSqlSeen;
			internal ExecNodeListener()
            {
                ExecSqlSeen = false;
            }
            public void OnNode(Node node, Program program)
            {
				if (node?.CodeElement != null && node.CodeElement.Type == Compiler.CodeElements.CodeElementType.ExecStatement)
					ExecSqlSeen = true;
            }
        };

        /// <summary>
        /// Test that EXEC SQL statement outside a PROCEDURE DIVISION does not generate basic blocks.
        /// </summary>
        [TestMethod]
        [TestCategory("CfgDfaBuildTest")]
        public void ExecSqlOutsideProc()
        {

            ExecNodeListener listener = null;
            NodeListenerFactory ExecSqlDetector = () =>
            {
                listener = new ExecNodeListener();
                return listener;
            };

            NodeDispatcher.RegisterStaticNodeListenerFactory(ExecSqlDetector);

            string path = Path.Combine(Directory.GetCurrentDirectory(), "CfgDfaBuildTests", "ExecSqlOutsideProc.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            NodeDispatcher.RemoveStaticNodeListenerFactory(ExecSqlDetector);
            Assert.IsTrue(ctx.Builder.Programs.Count == 1);

            Assert.IsTrue(ctx.CfgDfaBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);
			//Check that Exec SQL has been detected
            Assert.IsNotNull(listener);
            Assert.IsTrue(listener.ExecSqlSeen);
            //We didn't entered in a PROCEDURE DIVISION
            Assert.IsFalse(ctx.CfgDfaBuilder.Cfg.IsInProcedure);
            //No basic Block was created
            Assert.IsNotNull(ctx.CfgDfaBuilder.Cfg.AllBlocks);
            Assert.AreEqual(0, ctx.CfgDfaBuilder.Cfg.AllBlocks.Count);
        }

        [TestMethod]
        [TestCategory("CfgDfaBuildTest")]
        public void PrgWithNoProcDiv()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "CfgDfaBuildTests", "PrgWithNoProcDiv.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);

            Assert.IsTrue(ctx.Builder.Programs.Count == 1);

            Assert.IsTrue(ctx.CfgDfaBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);

            //Try to compute predecessor edges.
            ctx.CfgDfaBuilder.Cfg.SetupPredecessorEdges();

            //Test Empty Cfg Generated.
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "EmptyCfg.dot");
            CfgTestUtils.GenDotCfgAndCompare(ctx.CfgDfaBuilder.Cfg, path, expectedPath, true);

            //Test DFA algorithms.
            TypeCobolDataFlowGraphBuilder dfaBuilder = new TypeCobolDataFlowGraphBuilder(ctx.CfgDfaBuilder.Cfg);
            dfaBuilder.ComputeUseList();
            dfaBuilder.ComputeDefList();
            dfaBuilder.ComputeGenSet();
            dfaBuilder.ComputeKillSet();
            dfaBuilder.ComputeInOutSet();
            dfaBuilder.ComputeUseDefSet();
        }
    }
}

