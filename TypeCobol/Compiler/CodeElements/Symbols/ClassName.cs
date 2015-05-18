using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// A user-defined word that identifies the class. 
    /// Can optionally have an entry in the REPOSITORY paragraph of the configuration section of the class definition.
    /// Must conform to the normal rules of formation for a COBOL user-defined word, using single-byte characters.
    /// </summary>
    public class ClassName : Symbol
    {
        public ClassName(string userDefinedWord) :
            base(userDefinedWord, SymbolType.ClassName)
        { }
    }
}
