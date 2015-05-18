using System;

namespace TypeCobol.Compiler.CodeElements
{
    /// <summary>
    /// Condition-names follow the rules for user-defined names. At least one
    /// character must be alphabetic. The value associated with the
    /// condition-name is considered to be alphanumeric. A condition-name can be
    /// associated with the on status or off status of each UPSI switch specified.
    /// In the PROCEDURE DIVISION, the UPSI switch status is tested through
    /// the associated condition-name. Each condition-name is the equivalent of a
    /// level-88 item; the associated mnemonic-name, if specified, is considered the
    /// conditional variable and can be used for qualification.
    /// Condition-names specified in the SPECIAL-NAMES paragraph of a
    /// containing program can be referenced in any contained program
    /// </summary>
    public class ConditionName : Symbol
    {
        public ConditionName(string userDefinedWord) :
            base(userDefinedWord, SymbolType.ConditionName)
        { }
    }
}
