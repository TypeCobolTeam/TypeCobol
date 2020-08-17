using System.Collections.Generic;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;

using static TypeCobol.Analysis.Test.CfgTestUtils;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests testing CFG for Programs.
    /// </summary>
    [TestClass]
    public class BasicCfgProgramTest
    {
        [TestMethod]
        [TestCategory("BasicCfgProgram")]
        public void HanoiPrgCfgExtended()
        {
            string path = Path.Combine(BasicCfgPrograms, "HanoiPrg.cbl");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs = ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfgs.Count == 1);

            string expectedPath = Path.Combine(BasicCfgPrograms, "HanoiPrg.dot");
            GenDotCfgAndCompare(cfgs[0], path, expectedPath, true);
        }

        /// <summary>
        /// In DFA Extend mode a recursive PERFORM PROCEDURE will emit a recursion diagnostics.
        /// </summary>
        [TestMethod]
        public void DetectPerformProcRecursiveException()
        {
            string path = Path.Combine(BasicCfgInstrs, "PerformProcRecursive0.cbl");
            string diag = Path.Combine(BasicCfgInstrs, "DetectPerformProcRecursiveException.diag");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs = ParseCompareDiagnosticsForDfa(path, diag);
            Assert.IsTrue(cfgs.Count == 1);

            string expectedPath = Path.Combine(BasicCfgInstrs, "DetectPerformProcRecursiveException.dot");
            GenDotCfgAndCompare(cfgs[0], path, expectedPath, true);

            //Assert.IsTrue(ctx.CfgDfaBuilder.Diagnostics[0].Message.Contains(TypeCobol.Analysis.Resource.RecursiveBasicBlockGroupInstructions.Substring(0, 
            //    TypeCobol.Analysis.Resource.RecursiveBasicBlockGroupInstructions.LastIndexOf(':'))));

            GenDotCfgAndCompare(cfgs[0], path, expectedPath, true);
        }
    }
}
