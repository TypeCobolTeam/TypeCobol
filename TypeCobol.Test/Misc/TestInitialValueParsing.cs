using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Misc
{
    [TestClass]
    public class TestInitialValueParsing
    {
        /// <summary>
        /// Issue #1791, parse 'InitialValuesContinuations.cbl' in Parser\Programs\Cobol85\ContinuationLine
        /// and check initial string values for all fields
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckInitialValuesContinuations()
        {
            string[] expectedInitialValues =
            {
                "start1                                 end1",
                "start2                                 end2",
                "start3                                 end3",
                "start4                                 end4",
                "start5                                 extension                                                   end5"
            };

            var folder = Path.Combine("Parser", "Programs", "Cobol85", "ContinuationLine");
            var compilationUnit = ParserUtils.ParseCobolFile("InitialValuesContinuations", folder);
            var workingStorage = compilationUnit.TemporaryProgramClassDocumentSnapshot.Root.MainProgram.Children[0].Children[0];

            for (int i = 0; i < workingStorage.ChildrenCount; i++)
            {
                var field = (DataDescription) workingStorage.Children[i];
                string expectedInitialValue = expectedInitialValues[i];
                string actualInitialValue = field.CodeElement.InitialValue.AlphanumericValue.Value;
                Assert.AreEqual(expectedInitialValue, actualInitialValue);
            }
        }
    }
}
