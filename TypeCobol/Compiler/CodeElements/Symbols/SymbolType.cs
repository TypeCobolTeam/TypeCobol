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
        EnvironmentName
    }
}
