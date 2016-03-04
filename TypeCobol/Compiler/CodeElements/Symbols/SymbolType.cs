using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Types of symbols defined by the Cobol syntax
    /// </summary>
    public enum SymbolType
    {
        // Symbols defined and referenced in the program

        AlphabetName,
        ClassName,
        CharsetClassName,
        ConditionName,
        ConditionForUPSISwitchName,
        DataName,
        IndexName,
        FileName,
        MethodName,
        MnemonicForEnvironmentName,
        MnemonicForUPSISwitchName,
        ParagraphName,
        ProgramEntry,
        ProgramName,
        SectionName,
        SpecialRegisterName, // implicitely defined
        SymbolicCharacter,
        XmlSchemaName,

        // External names referenced in the program

        AssignmentName,
        EnvironmentName,
        ExecTranslatorName,
        FunctionName,
        LibraryName,
        TextName,
        UPSISwitchName,

        /// <summary>
        /// Type to use when the parser can't determiner the exact SymbolType.
        /// The actual type will be resolved in a next compiler phase
        /// </summary>
        Unknown
    }
}
