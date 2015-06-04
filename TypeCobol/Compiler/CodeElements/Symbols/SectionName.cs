using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// A user-defined word that identifies a section. A referenced
    /// section-name, because it cannot be qualified, must be unique
    /// within the program in which it is defined.
    /// </summary>
    public class SectionName : Symbol
    {
        public SectionName(string userDefinedWord) :
            base(userDefinedWord, SymbolType.SectionName)
        { }
    }
}
