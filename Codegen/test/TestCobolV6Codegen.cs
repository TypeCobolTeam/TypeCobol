using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.IO;

namespace TypeCobol.Codegen
{
    [TestClass]
    public class TestCobolV6Codegen
    {
        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TestDynamicAllocationStatements()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("CobolV6", "DynamicAllocationStatements") + ".rdz.cbl");
        }
    }
}
