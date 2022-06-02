using JetBrains.Annotations;

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
        DataTypeName, // <= TYPECOBOL specific : user defined data types
        ExternalClassName,
        IndexName,
        FileName,
        TCFunctionName, // <= TYPECOBOL specific : user defined functions
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
        IntrinsicFunctionName,
        LibraryName,
        TextName,
        UPSISwitchName,

        // Used when the type of the symbol has not yet been resolved
        TO_BE_RESOLVED,
        // For SQL
        SqlIdentifier,
        SqlVariable
    }

    public static class SymbolTypeUtils {
        public static CobolLanguageLevel GetCobolLanguageLevel(SymbolType symbolType) {
            if (symbolType == SymbolType.DataTypeName) {
                return CobolLanguageLevel.Cobol2002;
            }
            if (symbolType == SymbolType.TCFunctionName) {
                return CobolLanguageLevel.TypeCobol;
            }
            return CobolLanguageLevel.Cobol85;
        }

        public static CobolLanguageLevel GetCobolLanguageLevel([CanBeNull] params SymbolType[] symbolTypes) {
            CobolLanguageLevel result = CobolLanguageLevel.Cobol85;

            if (symbolTypes != null) {
                foreach (var symbolType in symbolTypes) {
                    var current = GetCobolLanguageLevel(symbolType);
                    if (current == CobolLanguageLevel.TypeCobol) {
                        return CobolLanguageLevel.TypeCobol;
                    }
                    if (current > result) {
                        result = current;
                    }
                }
            }
            return result;
        }
    }
}
