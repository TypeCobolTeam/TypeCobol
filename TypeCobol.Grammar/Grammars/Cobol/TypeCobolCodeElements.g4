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


moveStatement:
	MOVE UNSAFE? corresponding? identifierOrLiteral TO identifier+;
