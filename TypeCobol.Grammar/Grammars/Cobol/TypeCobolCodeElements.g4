grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

qualifiedDataName:                                    ((dataNameReferenceOrFileNameReference                                     ColonSeparator ColonSeparator)* dataNameReferenceOrIndexNameReference)                                                                | legacyQualifiedDataNameOrIndexName;
qualifiedConditionName:                               ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* conditionNameReferenceOrConditionForUPSISwitchNameReference)                                          | legacyQualifiedConditionName;

legacyQualifiedDataName:                                    dataNameReference                                                                                  ((IN | OF) dataNameReferenceOrFileNameReference)*;
legacyQualifiedConditionName:                               conditionNameReferenceOrConditionForUPSISwitchNameReference                                        ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;

moveStatement:
	MOVE UNSAFE? corresponding? identifierOrLiteral TO identifier+;
