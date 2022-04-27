using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.CupParser.NodeBuilder;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are simple tests to validate Cfg/Dfa constructions with miscellaneous bugs.
    /// </summary>
    [TestClass]
    public class CfgDfaBuildTests
    {
        /// <summary>
        /// Check if an ExecSqlStatement has been seen or not in a program.
        /// </summary>
        class ExecNodeListener : SyntaxDrivenAnalyzerBase
        {
            internal bool ExecSqlSeen;

            internal ExecNodeListener()
                : base("CfgBuildTest.ExecNodeListener")
            {
                ExecSqlSeen = false;
            }

            public override void OnNode(Node node, Program program)
            {
                if (node?.CodeElement != null &&
                    node.CodeElement.Type == Compiler.CodeElements.CodeElementType.ExecStatement)
                    ExecSqlSeen = true;
            }

            public override object GetResult()
            {
                return null;
            }
        }

        /// <summary>
        /// Test that EXEC SQL statement outside a PROCEDURE DIVISION does not generate basic blocks.
        /// </summary>
        [TestMethod]
        [TestCategory("CfgDfaBuildTest")]
#if SQL_PARSING
        [Ignore]
#endif
        public void ExecSqlOutsideProc()
        {
            ExecNodeListener listener = null;
            ISyntaxDrivenAnalyzer CreateExecNodeListener(TypeCobolOptions o, TextSourceInfo t) => listener = new ExecNodeListener();

            string path = Path.Combine(CfgTestUtils.CfgDfaBuildTests, "ExecSqlOutsideProc.cbl");
            var graphs = ParseCompareDiagnostics<DfaBasicBlockInfo<VariableSymbol>>(path, CfgBuildingMode.WithDfa, null, CreateExecNodeListener);
            Assert.IsTrue(graphs.Count == 1);

            //Check generated graph has not been initialized
            Assert.IsFalse(graphs[0].IsInitialized);

            //Check that Exec SQL has been detected
            Assert.IsNotNull(listener);
            Assert.IsTrue(listener.ExecSqlSeen);
        }

        /// <summary>
        /// Simple execution test of some DFA algorithms.
        /// </summary>
        [TestMethod]
        [TestCategory("CfgDfaBuildTest")]
        public void PrgWithNoProcDiv()
        {
            string path = Path.Combine(CfgTestUtils.CfgDfaBuildTests, "PrgWithNoProcDiv.cbl");
            var dfaResults = ParseCompareDiagnosticsWithDfa(path);
            Assert.IsTrue(dfaResults.Graphs.Count == 1);

            //Try to compute predecessor edges.
            dfaResults.Graphs[0].SetupPredecessorEdgesFromRoot();

            //Test Empty Cfg Generated.
            string expectedPath = Path.Combine(CfgTestUtils.CfgDfaBuildTests, "EmptyCfg.dot");
            GenDotCfgAndCompare(dfaResults.Graphs[0], path, expectedPath, true);
        }
    }
}
