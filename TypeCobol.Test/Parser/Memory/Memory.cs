using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.Test.Compiler.Parser;
using TypeCobol.Test.Utils;

namespace TypeCobol.Test.Parser.Memory
{
    [TestClass]
    public class Memory
    {
        static readonly string MemoryFolder = PlatformUtils.GetPathForProjectFile("Parser") + Path.DirectorySeparatorChar + "Memory";

        /// <summary>
        /// Check variable size and position in memory
        /// Test with Clause OCCURS and REDEFINES
        /// </summary>
        [TestMethod]
        public void CheckMemoryOccursRedefines()
        {
            Paths paths = new Paths(MemoryFolder, MemoryFolder, MemoryFolder + Path.DirectorySeparatorChar + "MemoryOccursRedefines.pgm", new MemoryName());
            TestUnit unit = new TestUnit(new MemoryComparator(paths), antlrProfiler: true);
            unit.Parse();

            unit.Compare();
        }

        /// <summary>
        /// Check variable size and position in memory
        /// Test with all variable types and usages possible
        /// </summary>
        [TestMethod]
        public void CheckMemoryTypeUsage()
        {
            Paths paths = new Paths(MemoryFolder, MemoryFolder, MemoryFolder + Path.DirectorySeparatorChar + "MemoryTypeUsage.pgm", new MemoryName());
            TestUnit unit = new TestUnit(new MemoryComparator(paths), antlrProfiler: true);
            unit.Parse();

            unit.Compare();
        }

        /// <summary>
        /// Check variable size and position in memory
        /// Test with all variable types and usages possible
        /// </summary>
        [TestMethod]
        public void CheckMemoryNational()
        {
            Paths paths = new Paths(MemoryFolder, MemoryFolder, MemoryFolder + Path.DirectorySeparatorChar + "MemoryNational.pgm", new MemoryName());
            TestUnit unit = new TestUnit(new MemoryComparator(paths), antlrProfiler: true);
            unit.Parse();

            unit.Compare();
        }


        /// <summary>
        /// Check variable size and position in memory
        /// Test with all variable types and usages possible
        /// </summary>
        [TestMethod]
        public void CheckCGM110_DFHCOMMAREA()
        {
            Paths paths = new Paths(MemoryFolder, MemoryFolder, MemoryFolder + Path.DirectorySeparatorChar + "CGM110-DFHCOMMAREA.rdz.pgm", new MemoryName());
            TestUnit unit = new TestUnit(new MemoryComparator(paths), antlrProfiler: true);
            unit.Parse();

            unit.Compare();
        }
    }
}
