using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.LanguageServer.VsCodeProtocol;

namespace TypeCobol.LanguageServer.Test.ProcessorTests
{
    /// <summary>
    /// Tests for DataLayout processor with CSV
    /// </summary>
    [TestClass]
    public class DataLayoutProcessorCSVTest : DataLayoutProcessorTest
    {
        protected override string TestPrefix => "CSV-";

        protected override string GetActualResult(CompilationUnit compilationUnit, Position position)
        {
            // Execute processor
            var processorResult = _processor.GetDataLayoutAsCSV(compilationUnit, position, ";");

            // Build actual result
            var actual = new StringBuilder();
            actual.AppendLine(processorResult.Root);
            actual.AppendLine(processorResult.Header);
            foreach (var row in processorResult.Rows)
            {
                actual.AppendLine(row);
            }

            return actual.ToString();
        }

        protected override string FormatExpectedResult(string expected) => expected;

        [TestMethod]
        public void Empty() => ExecuteTest("empty");

        [TestMethod]
        public void Copy() => ExecuteTest("copy", true);

        [TestMethod]
        // To test Generated 01 (virtual node built by the parser) which is ignored in CSV output
        public void CopyWithout01() => ExecuteTest("copyWithout01", true);

        [TestMethod]
        public void SimplePgm() => ExecuteTest("simplePgm");

        [TestMethod]
        public void MainPgm() => ExecuteTest("stackedAndNestedPgm");

        [TestMethod]
        public void StackedPgm() => ExecuteTest("stackedAndNestedPgm");

        [TestMethod]
        public void NestedPgm() => ExecuteTest("stackedAndNestedPgm");
    }
}
