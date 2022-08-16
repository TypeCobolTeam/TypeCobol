using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Misc
{
    [TestClass]
    public class TestFileConnectors
    {
        /// <summary>
        /// Issue #1853, check FileConnectors is set and FileControlEntry are matching with keys
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckFileConnectorsSet()
        {
            var expectedResults = new Dictionary<string, string>()
            {
                {"CUSTOMER-F01", "UT-CUSTOMER-F01"},
                {"CUSTOMER-F02", "UT-CUSTOMER-F02"},
                {"STUDENT-F01", "UT-STUDENT-F01"},
                {"STUDENT-F02", "UT-STUDENT-F02"}
            };

            var folder = Path.Combine("Parser", "Programs", "Cobol85");
            var compilationUnit = ParserUtils.ParseCobolFile("FileControl2", folder);
            var fileConnectors = compilationUnit.TemporaryProgramClassDocumentSnapshot.Root.MainProgram.FileConnectors;

            Assert.IsNotNull(fileConnectors);
            Assert.AreEqual(expectedResults.Count, fileConnectors.Count);
            foreach (var fileConnectorPair in fileConnectors)
            {
                var symbolReference = fileConnectorPair.Key;
                var fileControlEntry = fileConnectorPair.Value;
                Assert.IsNotNull(symbolReference);
                var isExpectedFileName = expectedResults.TryGetValue(symbolReference.Name, out var expectedDataSetName);
                Assert.IsTrue(isExpectedFileName);
                Assert.IsNotNull(fileControlEntry);
                Assert.AreEqual(symbolReference.Name, fileControlEntry.FileName?.Name);
                Assert.AreEqual(expectedDataSetName, fileControlEntry.ExternalDataSet?.Name);
            }
        }
    }
}