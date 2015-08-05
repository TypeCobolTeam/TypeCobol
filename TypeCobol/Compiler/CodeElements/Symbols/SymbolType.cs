using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Types of symbols defined by the Cobol syntax
    /// </summary>
    public enum SymbolType
    {
        AlphabetName,
        AssignmentName,
        ClassName,
        CharsetClassName,
        ConditionName,
        DataName,
        IndexName,
        FileName,
        MethodName,
        MnemonicForEnvironmentName,
        MnemonicForUPSISwitchName,
        ParagraphName,
        ProgramName,
        SectionName,
        SymbolicCharacter,
        XmlSchemaName,
        EnvironmentName,

        /// <summary>
        /// Type to use when the parser can't determiner the exact SymbolType.
        /// The actual type will be resolved in a next compiler phase
        /// </summary>
        Unknown
    }
}
