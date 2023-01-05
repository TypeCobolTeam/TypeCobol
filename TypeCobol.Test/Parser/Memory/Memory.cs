using System.IO;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Test.UtilsNew;

namespace TypeCobol.Test.Parser.Memory
{
    [TestClass]
    public class Memory
    {
        static readonly string _MemoryFolder = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar + "Memory";

        private void CheckOneFile(string input, DocumentFormat format, string output)
        {
            var testUnit = new TestUnit(input, format, antlrProfiling: true);
            testUnit.AddComparison(Comparisons.GetComparison(output));
            testUnit.Run();
        }

        /// <summary>
        /// Check variable size and position in memory
        /// Test with Clause OCCURS and REDEFINES
        /// </summary>
        [TestMethod]
        public void CheckMemoryOccursRedefines()
        {
            string input = Path.Combine(_MemoryFolder, "MemoryOccursRedefines.pgm");
            string output = Path.Combine(_MemoryFolder, "MemoryOccursRedefines.MEM.txt");
            CheckOneFile(input, DocumentFormat.FreeTextFormat, output);
        }

        /// <summary>
        /// Check variable size and position in memory
        /// Test with all variable types and usages possible
        /// </summary>
        [TestMethod]
        public void CheckMemoryTypeUsage()
        {
            string input = Path.Combine(_MemoryFolder, "MemoryTypeUsage.pgm");
            string output = Path.Combine(_MemoryFolder, "MemoryTypeUsage.MEM.txt");
            CheckOneFile(input, DocumentFormat.FreeTextFormat, output);
        }

        /// <summary>
        /// Check variable size and position in memory
        /// Test with all variable types and usages possible
        /// </summary>
        [TestMethod]
        public void CheckMemoryNational()
        {
            string input = Path.Combine(_MemoryFolder, "MemoryNational.pgm");
            string output = Path.Combine(_MemoryFolder, "MemoryNational.MEM.txt");
            CheckOneFile(input, DocumentFormat.FreeTextFormat, output);
        }

        /// <summary>
        /// Check variable size and position in memory
        /// Test with all variable types and usages possible
        /// </summary>
        [TestMethod]
        public void CheckCGM110_DFHCOMMAREA()
        {
            string input = Path.Combine(_MemoryFolder, "CGM110-DFHCOMMAREA.rdz.pgm");
            string output = Path.Combine(_MemoryFolder, "CGM110-DFHCOMMAREA.MEM.txt");
            CheckOneFile(input, DocumentFormat.RDZReferenceFormat, output);
        }
    }
}
