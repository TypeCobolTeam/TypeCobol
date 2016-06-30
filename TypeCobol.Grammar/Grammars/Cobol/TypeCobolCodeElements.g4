grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

qualifiedDataName:                                    (qDataOrFile*       dataNameReference                                                                                  (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataName;
qualifiedDataNameOrIndexName:                         (qDataOrFile*       dataNameReferenceOrIndexNameReference                                                              (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataNameOrIndexName;
qualifiedConditionName:                               (qDataOrFileOrUPSI* conditionNameReferenceOrConditionForUPSISwitchNameReference                                        (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedConditionName;
qualifiedDataNameOrQualifiedConditionName:            (qDataOrFileOrUPSI* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference                     (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataNameOrConditionName;
qualifiedDataNameOrQualifiedConditionNameOrIndexName: (qDataOrFileOrUPSI* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName;
qualifiedDataNameOrQualifiedConditionNameOrFileName:  (qDataOrFileOrUPSI* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference  (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataNameOrQualifiedConditionNameOrFileName;
qualifiedDataNameOrQualifiedConditionNameOrClassName: (qDataOrFileOrUPSI* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataNameOrQualifiedConditionNameOrClassName;

qDataOrFile:       dataNameReferenceOrFileNameReference                                     (LeftParenthesisSeparator subscript RightParenthesisSeparator)? ColonSeparator ColonSeparator;
qDataOrFileOrUPSI: dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference (LeftParenthesisSeparator subscript RightParenthesisSeparator)? ColonSeparator ColonSeparator;

legacyQualifiedDataName:                                    dataNameReference                                                                                  ((IN | OF) dataNameReferenceOrFileNameReference)*;
legacyQualifiedDataNameOrIndexName:                         dataNameReferenceOrIndexNameReference                                                              ((IN | OF) dataNameReferenceOrFileNameReference)*;
legacyQualifiedConditionName:                               conditionNameReferenceOrConditionForUPSISwitchNameReference                                        ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrConditionName:                     dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference                     ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrQualifiedConditionNameOrFileName:  dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference  ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrQualifiedConditionNameOrClassName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;

// rule modified to support:
// - TYPE DATE (instead of TC-DATE or something)
cob2002TypeClause:    TYPE (UserDefinedWord | DATE);

// rule modified to support:
// - MOVE UNSAFE <custom type> TO <custom type>
// - MOVE TRUE  TO <boolean>
// - MOVE FALSE TO <boolean>
moveStatement:
    MOVE UNSAFE? corresponding? (TRUE | FALSE | identifierOrLiteral) TO identifier+;
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
    | (ENTRY_ARG (programNameReferenceOrProgramEntryReference | programNameFromDataOrProgramEntryFromData))
    ;

// rules modified to support custom-designed functions (of arity 0..n)
functionIdentifier: FUNCTION intrinsicFunctionName (LeftParenthesisSeparator argument* RightParenthesisSeparator)?;
intrinsicFunctionName: FunctionName | LENGTH | RANDOM | WHEN_COMPILED | UserDefinedWord;
