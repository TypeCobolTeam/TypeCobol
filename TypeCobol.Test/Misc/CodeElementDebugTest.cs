using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Misc
{
    [TestClass]
    public class CodeElementDebugTest
    {
        /// <summary>
        /// Issue #1878 : check the debug mode of CodeElement
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckCodeElementDebugPropertyWithDebugging()
        {
            var expectedDebugMode = new List<CodeElement.DebugType>()
            {
                CodeElement.DebugType.None, // CodeElement 0
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None, //5
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None, //10
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.None,
                CodeElement.DebugType.All,
                CodeElement.DebugType.All,
                CodeElement.DebugType.All,  //15
                CodeElement.DebugType.All,
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.Mix,  //20
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.All,
                CodeElement.DebugType.None,
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.None, //25
                CodeElement.DebugType.None,
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None  //30
            };

            CompareDebugTypes(expectedDebugMode, "DebugLinesWithDebugging");
        }

        /// <summary>
        /// Issue #1878 : check the debug mode of CodeElement
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckCodeElementDebugPropertyNotDebugging()
        {
            var expectedDebugMode = new List<CodeElement.DebugType>()
            {
                CodeElement.DebugType.None, // CodeElement 0
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None, //5
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None, //10
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None,
                CodeElement.DebugType.None
            };

            CompareDebugTypes(expectedDebugMode, "DebugLinesNotDebugging");
        }

        /// <summary>
        /// Issue #2140 : check the debug mode of CodeElement
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckInvalidCodeElementDebugProperty()
        {
            var expectedDebugMode = Enumerable.Repeat(CodeElement.DebugType.None, 10).ToList();
            CompareDebugTypes(expectedDebugMode, "DebugLinesInvalidCE");
        }

        private static void CompareDebugTypes(IReadOnlyList<CodeElement.DebugType> expectedDebugMode, string fileName)
        {
            var folder = Path.Combine("Parser", "Programs", "Cobol85");
            var compilationUnit = ParserUtils.ParseCobolFile(fileName, folder);

            var codeElements = compilationUnit.CodeElementsDocumentSnapshot.CodeElements.ToList();
            for (var i = 0; i < codeElements.Count; i++)
            {
                Assert.AreEqual(expectedDebugMode[i], codeElements[i].DebugMode, $"CodeElement number: {i}");
            }
        }
    }
}