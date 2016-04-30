grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

//qualifiedDataName:                                    ((dataNameReferenceOrFileNameReference                                     ColonSeparator ColonSeparator)* dataNameReferenceOrIndexNameReference)                                                                | legacyQualifiedDataNameOrIndexName;
//qualifiedConditionName:                               ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* conditionNameReferenceOrConditionForUPSISwitchNameReference)                                          | legacyQualifiedConditionName;

qualifiedDataName:                                    (qDataOrFile*       dataNameReference                                                                                  (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataName;
qualifiedConditionName:                               (qDataOrFileOrUPSI* conditionNameReferenceOrConditionForUPSISwitchNameReference                                        (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedConditionName;

qDataOrFile:       dataNameReferenceOrFileNameReference                                     (LeftParenthesisSeparator subscript RightParenthesisSeparator)? ColonSeparator ColonSeparator;
qDataOrFileOrUPSI: dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference (LeftParenthesisSeparator subscript RightParenthesisSeparator)? ColonSeparator ColonSeparator;


legacyQualifiedDataName:                                    dataNameReference                                                                                  ((IN | OF) dataNameReferenceOrFileNameReference)*;
legacyQualifiedConditionName:                               conditionNameReferenceOrConditionForUPSISwitchNameReference                                        ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;

// rule modified to support:
// - MOVE UNSAFE <custom type> TO <custom type>
// - MOVE TRUE  TO <boolean>
// - MOVE FALSE TO <boolean>
moveStatement:
    MOVE UNSAFE? corresponding? (booleanValue | anyVariable4) TO identifier+;
//         ^                      ^       ^
//          \                      \       \
//           \                      --------------  MOVE [TRUE|FALSE] TO <boolean>
//            ------------------------------------  MOVE UNSAFE <custom type> TO <custom type>


// rule modified to support:
// - SET <boolean> TO FALSE
setStatementForAssignationSending:
    identifier | IntegerLiteral
    | FALSE                         // <----- SET <boolean> TO FALSE
    | TRUE | (NULL | NULLS) | SELF
    | (ENTRY_ARG programNameOrProgramEntryVariable)
    ;
