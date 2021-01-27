using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
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
            var folder = Path.Combine("Parser", "Programs", "Cobol85");
            var compilationUnit = ParserUtils.ParseCobolFile("FileControl2", DocumentFormat.RDZReferenceFormat, folder);
            var fileConnectors = compilationUnit.TemporaryProgramClassDocumentSnapshot.Root.MainProgram.FileConnectors;

            Assert.IsNotNull(fileConnectors);
            Assert.AreEqual(fileConnectors.Keys.Count, 4);
            // CUSTOMER-F01
            var customer1 = GetFileControlEntry("CUSTOMER-F01");
            Assert.IsNotNull(customer1);
            Assert.AreEqual(customer1.FileName?.Name, "CUSTOMER-F01");
            Assert.AreEqual(customer1.ExternalDataSet?.Name, "UT-CUSTOMER-F01");
            // CUSTOMER-F02
            var customer2 = GetFileControlEntry("CUSTOMER-F02");
            Assert.IsNotNull(customer2);
            Assert.AreEqual(customer2.FileName?.Name, "CUSTOMER-F02");
            Assert.AreEqual(customer2.ExternalDataSet?.Name, "UT-CUSTOMER-F02");
            // STUDENT-01
            var student1 = GetFileControlEntry("STUDENT-F01");
            Assert.IsNotNull(student1);
            Assert.AreEqual(student1.FileName?.Name, "STUDENT-F01");
            Assert.AreEqual(student1.ExternalDataSet?.Name, "UT-STUDENT-F01");
            // STUDENT-02
            var student2 = GetFileControlEntry("STUDENT-F02");
            Assert.IsNotNull(student2);
            Assert.AreEqual(student2.FileName?.Name, "STUDENT-F02");
            Assert.AreEqual(student2.ExternalDataSet?.Name, "UT-STUDENT-F02");

            FileControlEntry GetFileControlEntry(string keyName)
            {
                return fileConnectors.FirstOrDefault(pair => pair.Key.Name == keyName).Value;
            }
        }
    }
}