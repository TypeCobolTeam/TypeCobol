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

moveStatement:
	MOVE UNSAFE? corresponding? identifierOrLiteral2 TO identifier+;
