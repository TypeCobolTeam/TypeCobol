using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Symbols;
using System.IO;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Analysis.Graph;
using TypeCobol.Analysis.Report;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// Dfa test for call module report
    /// </summary>
    [TestClass]
    public class DfaCallPgmReport
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
        public void InBulkCallPgmReportTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Report", "Input", "InBulkCallPgm.cbl");
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

            //Create the report file.
            ZCallPgmReport reporter = new ZCallPgmReport(ctx, null);
            using (System.IO.StringWriter sw = new StringWriter())
            {
                reporter.Report(sw);
                // compare with expected result
                string result = sw.ToString();
                string output = Path.Combine(Directory.GetCurrentDirectory(), "Report", "Output", "InBulkCallPgm.csv");
                string expected = File.ReadAllText(output, DocumentFormat.RDZReferenceFormat.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path, result, expected, output);
            }
        }

        [TestMethod]
        public void InBulkCallPgm88SetReportTest()
        {
            string path = Path.Combine(Directory.GetCurrentDirectory(), "Report", "Input", "InBulkCallPgm88Set.cbl");
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

            //Create the report file.
            ZCallPgmReport reporter = new ZCallPgmReport(ctx, null);
            using (System.IO.StringWriter sw = new StringWriter())
            {
                reporter.Report(sw);
                // compare with expected result
                string result = sw.ToString();
                string output = Path.Combine(Directory.GetCurrentDirectory(), "Report", "Output", "InBulkCallPgm88Set.csv");
                string expected = File.ReadAllText(output, DocumentFormat.RDZReferenceFormat.Encoding);
                TypeCobol.Test.TestUtils.compareLines(path, result, expected, output);
            }
        }

    }
}
