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

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TestInitializeStatements()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("CobolV6", "InitializeStatements") + ".rdz.cbl");
        }

        // UNBOUNDED keyword in OCCURS clause is actually part of Cobol v5.1 specs not 6.1.
        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TestOccursUnbounded()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("CobolV6", "OccursUnbounded") + ".rdz.cbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TestJsonGenerate()
        {
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("CobolV6", "JsonGenerate") + ".rdz.cbl");
        }

        [TestMethod]
        [TestCategory("Codegen")]
        [TestProperty("Time", "fast")]
        public void TestJsonParse()
        {
	        CodegenTestUtils.ParseGenerateCompare(Path.Combine("CobolV6", "JsonParse") + ".rdz.cbl");
        }
	}
}
