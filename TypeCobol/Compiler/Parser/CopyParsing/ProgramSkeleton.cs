using System.Collections.Generic;
using System.IO;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Parser.CopyParsing
{
    /// <summary>
    /// Helper class to create a skeleton for direct copy parsing.
    /// Copy CodeElements are wrapped into a minimalistic fake program like this :
    ///
    /// IDENTIFICATION DIVISION.
    /// PROGRAM-ID. copyName.
    /// DATA DIVISION.
    /// WORKING-STORAGE SECTION.
    /// 01.
    /// --copy content--
    /// END PROGRAM copyName.
    ///
    /// The anonymous 01 level allow to include copies without knowing their own starting level,
    /// it is removed at the end of the node phase if it comes out without any child data.
    /// </summary>
    internal class ProgramSkeleton
    {
        private readonly SymbolDefinition _programName;

        public ProgramSkeleton([NotNull] TextSourceInfo textSourceInfo)
            : this(textSourceInfo.Name)
        {

        }

        public ProgramSkeleton(string programName)
        {
            _programName = new SymbolDefinition(new GeneratedAlphanumericValue(programName), SymbolType.ProgramName);
        }

        public IEnumerable<CodeElement> Before()
        {
            yield return new ProgramIdentification() { ProgramName = _programName };
            yield return new DataDivisionHeader();
            yield return new WorkingStorageSectionHeader();
            yield return new DataDescriptionEntry() { LevelNumber = new GeneratedIntegerValue(1) };
        }

        public IEnumerable<CodeElement> After()
        {
            yield return new ProgramEnd() { ProgramName = new SymbolReference(_programName) };
        }
    }
}
