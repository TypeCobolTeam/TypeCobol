using System.Collections.Generic;
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
    /// These are basic tests testing CFG for Programs.
    /// </summary>
    [TestClass]
    public class BasicCfgProgramTest
    {
        [TestMethod]
        [TestCategory("BasicCfgProgram")]
        public void HanoiPrgCfgExtended()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgPrograms", "HanoiPrg.cbl");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs = CfgTestUtils.ParseCompareDiagnosticsForDfa(path);
            Assert.IsTrue(cfgs.Count == 1);

            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgPrograms", "HanoiPrg.dot");
            CfgTestUtils.GenDotCfgAndCompare(cfgs[0], path, expectedPath, true);
        }

        /// <summary>
        /// In DFA Extend mode a recursive PERFORM PROCEDURE will emit a recursion diagnostics.
        /// </summary>
        [TestMethod]
        public void DetectPerformProcRecursiveException()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformProcRecursive0.cbl");
            string diag = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "DetectPerformProcRecursiveException.diag");
            IList<ControlFlowGraph<Node, DfaBasicBlockInfo<Symbol>>> cfgs = CfgTestUtils.ParseCompareDiagnosticsForDfa(path, diag);
            Assert.IsTrue(cfgs.Count == 1);

            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "DetectPerformProcRecursiveException.dot");
            CfgTestUtils.GenDotCfgAndCompare(cfgs[0], path, expectedPath, true);

            //Assert.IsTrue(ctx.CfgDfaBuilder.Diagnostics[0].Message.Contains(TypeCobol.Analysis.Resource.RecursiveBasicBlockGroupInstructions.Substring(0, 
            //    TypeCobol.Analysis.Resource.RecursiveBasicBlockGroupInstructions.LastIndexOf(':'))));

            CfgTestUtils.GenDotCfgAndCompare(cfgs[0], path, expectedPath, true);
        }
    }
}
