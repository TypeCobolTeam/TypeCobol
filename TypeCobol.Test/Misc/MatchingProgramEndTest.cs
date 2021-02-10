using System;
using System.Globalization;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Misc
{
    [TestClass]
    public class MatchingProgramEndTest
    {
        /// <summary>
        /// Issues #1162 #1844 parse "ProgramEnd.rdz.cbl" in Parser\Programs\Cobol85\
        /// And check END PROGRAM is matched with its program declaration start
        /// </summary>
        [TestMethod]
        [TestCategory("Parsing")]
        [TestProperty("Time", "fast")]
        public void CheckMatchingEndProgram()
        {
            var folder = Path.Combine("Parser", "Programs", "Cobol85");
            var compilationUnit = ParserUtils.ParseCobolFile("ProgramEnd", DocumentFormat.RDZReferenceFormat, folder, ExecutionStep.CrossCheck);

            var diags = compilationUnit.AllDiagnostics();
            var astRoot = compilationUnit.TemporaryProgramClassDocumentSnapshot.Root;

            Assert.AreEqual(1, diags.Count);

            Assert.AreEqual(6, diags[0].Line);
            Assert.AreEqual("Warning: \"END PROGRAM\" is missing.", diags[0].Message);

            var mainProgram = (Program) astRoot.Children[0];
            var nested = mainProgram.Children[1];

            Assert.IsTrue(mainProgram.Children.Last().CodeElement is ProgramEnd);
            Assert.AreEqual(0, nested.Children.Count);

            foreach (var diagnostic in diags)
            {
                Console.Error.WriteLine(diagnostic);
            }
            Console.Error.WriteLine();
            Console.Error.WriteLine(astRoot.ToString());
        }
    }
}
