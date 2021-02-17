using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
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
        public void CheckCodeElementDebugProperty()
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
                CodeElement.DebugType.Unset, //    .
                CodeElement.DebugType.All,  //15
                CodeElement.DebugType.Unset, //    .
                CodeElement.DebugType.Mix, 
                CodeElement.DebugType.Mix, 
                CodeElement.DebugType.Mix,
                CodeElement.DebugType.Mix, //20
                CodeElement.DebugType.Mix, 
                CodeElement.DebugType.All, 
                CodeElement.DebugType.Unset, //    GO BACK
                CodeElement.DebugType.Unset, //    .
                CodeElement.DebugType.None, //25
                CodeElement.DebugType.None, 
                CodeElement.DebugType.None, 
                CodeElement.DebugType.None, 
                CodeElement.DebugType.None, 
                CodeElement.DebugType.None, //30
                CodeElement.DebugType.None, 
                CodeElement.DebugType.None, 
                CodeElement.DebugType.None, 
                CodeElement.DebugType.None,
                CodeElement.DebugType.Mix,  //35
                CodeElement.DebugType.All, 
                CodeElement.DebugType.None, 
                CodeElement.DebugType.Unset, //    GO BACK
                CodeElement.DebugType.Unset, //    .
                CodeElement.DebugType.None, //40
            };

            var folder = Path.Combine("Parser", "Programs", "Cobol85");
            var compilationUnit = ParserUtils.ParseCobolFile("DebugNormalLines", DocumentFormat.RDZReferenceFormat, folder);

            var codeElements = compilationUnit.TemporaryProgramClassDocumentSnapshot.NodeCodeElementLinkers.Keys.ToList();
            codeElements.Sort(CodeElementPositionComparer);

            for (int i = 0; i < codeElements.Count; i++)
            {
                Assert.AreEqual(expectedDebugMode[i], codeElements[i].DebugMode, $"CodeElement number: {i}");
            }

            // Compare CodeElement based on line then column
            int CodeElementPositionComparer(CodeElement a, CodeElement b)
            {
                var comparison = a.Line.CompareTo(b.Line);
                if (comparison != 0)
                {
                    return comparison;
                }

                return a.Column.CompareTo(b.Column);
            }
        }
    }
}