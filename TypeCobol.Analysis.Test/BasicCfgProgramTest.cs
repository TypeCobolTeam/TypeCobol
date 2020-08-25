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
        [TestCategory("BasicCfgProgram")]
        public void HanoiPrgCfgExtended()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgPrograms", "HanoiPrg.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "CfgPrograms", "HanoiPrg.dot");

            Assert.IsTrue(ctx.CfgDfaBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);

            CfgTestUtils.GenDotCfgAndCompare(ctx.CfgDfaBuilder.Cfg, path, expectedPath, true);
        }

        /// <summary>
        /// In DFA Extend mode a recursive PERFORM PROCEDURE will emit a recursion diagnostics.
        /// </summary>
        [TestMethod]
        public void DetectPerformProcRecursiveException()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "BasicCfgInstrs", "PerformProcRecursive0.cbl");
            var document = TypeCobol.Parser.Parse(path, /*format*/ DocumentFormat.RDZReferenceFormat, /*autoRemarks*/
                false, /*copies*/ null);
            Assert.IsTrue(document.Results.PrgSymbolTblBuilder.Programs.Count == 1);
            string expectedPath = Path.Combine(Directory.GetCurrentDirectory(), "DotOutput", "CfgPrograms", "DetectPerformProcRecursiveException.dot");

            Assert.IsTrue(ctx.CfgDfaBuilder.AllCfgBuilder.Count == 1);
            Assert.IsNotNull(ctx.CfgDfaBuilder.AllCfgBuilder);

            Assert.IsNotNull(ctx.CfgDfaBuilder.Diagnostics != null);
            Assert.IsTrue(ctx.CfgDfaBuilder.Diagnostics.Count > 0);
            Assert.IsTrue(ctx.CfgDfaBuilder.Diagnostics[0].Message.Contains(TypeCobol.Analysis.Resource.RecursiveBasicBlockGroupInstructions.Substring(0, 
                TypeCobol.Analysis.Resource.RecursiveBasicBlockGroupInstructions.LastIndexOf(':'))));

            CfgTestUtils.GenDotCfgAndCompare(ctx.CfgDfaBuilder.Cfg, path, expectedPath, true);
        }

    }
}
