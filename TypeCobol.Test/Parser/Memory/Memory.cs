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
        /// </summary>
        [TestMethod]
        public void CheckMemory()
        {
            Paths paths = new Paths(MemoryFolder, MemoryFolder, MemoryFolder + Path.DirectorySeparatorChar + "Memory.pgm", new MemoryName());
            TestUnit unit = new TestUnit(new MemoryComparator(paths));
            unit.Init(new[] { ".pgm", ".cpy" }, false, true);
            unit.Parse();

            unit.Compare();
        }
    }
}
