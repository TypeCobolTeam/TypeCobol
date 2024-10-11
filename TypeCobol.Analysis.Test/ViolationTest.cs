using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;
using System.Runtime.CompilerServices;
using System.Text;
using TypeCobol.Test;
using static TypeCobol.Analysis.Test.CfgTestUtils;
using static TypeCobol.Compiler.Symbols.Symbol;

namespace TypeCobol.Analysis.Test
{
    /// <summary>
    /// These are basic tests testing Violation creation.
    /// </summary>
    [TestClass]
    public class ViolationTest
    {
        private static void TestCreation([CallerMemberName] string testName = null)
        {
            // Define input file
            var fileName = nameof(TestCreation);
            var folder = PlatformUtils.GetPathForProjectFile(testName, "TypeCobol.Analysis.Test");
            var inputFileName = $"{fileName}-input.txt";

            // Read error messages in input file and append corresponding violations in result
            var result = new StringBuilder();
            using (StreamReader reader = new StreamReader(Path.Combine(folder, inputFileName)))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    var violation = new Violation("ruleID", Compiler.Diagnostics.Severity.Error, Compiler.Diagnostics.Diagnostic.Position.Default, line);
                    result.AppendLine($"{violation}");
                }
            }

            // Compare to expected
            var expectedPath = Path.Combine(folder, $"{fileName}-expected.txt");
            var expected = File.ReadAllText(expectedPath);
            TestUtils.CompareLines(testName, result.ToString(), expected, expectedPath);
        }

        [TestMethod]
        public void Violation()
        {
            TestCreation();
        }
    }
}
