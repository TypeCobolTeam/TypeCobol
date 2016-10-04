using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Types of symbols defined by the Cobol syntax
    /// </summary>
    public enum SymbolType
    {
        // Symbols defined in the program
        AlphabetName,
        ClassName,
        CharacterClassName,
        ConditionName,
        ConditionForUPSISwitchName,
        DataName,
        ExternalClassName,
        IndexName,
        FileName,
        MethodName,
        MnemonicForEnvironmentName,
        MnemonicForUPSISwitchName,
        ParagraphName,
        ProgramEntry,
        ProgramName,
        SectionName,
        SymbolicCharacter,
        XmlSchemaName,

        // External names defined by the environment
        AssignmentName,
        EnvironmentName,
        ExecTranslatorName,
        FunctionName,
        LibraryName,
        TextName,
        UPSISwitchName,

        // Used when the type of the symbol has not yet been resolved
        TO_BE_RESOLVED
    }
}
