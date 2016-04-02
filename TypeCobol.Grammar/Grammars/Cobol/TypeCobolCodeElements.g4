grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

qualifiedDataName:                                    ((dataNameReferenceOrFileNameReference                                     ColonSeparator ColonSeparator)* dataNameReference)                                                                                    | legacyQualifiedDataName;
qualifiedDataNameOrIndexName:                         ((dataNameReferenceOrFileNameReference                                     ColonSeparator ColonSeparator)* dataNameReferenceOrIndexNameReference)                                                                | legacyQualifiedDataNameOrIndexName;
qualifiedConditionName:                               ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* conditionNameReferenceOrConditionForUPSISwitchNameReference)                                          | legacyQualifiedConditionName;
qualifiedDataNameOrQualifiedConditionName:            ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference)                       | legacyQualifiedDataNameOrConditionName;
qualifiedDataNameOrQualifiedConditionNameOrIndexName: ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference)   | legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName;
qualifiedDataNameOrQualifiedConditionNameOrFileName:  ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference)    | legacyQualifiedDataNameOrQualifiedConditionNameOrFileName;
qualifiedDataNameOrQualifiedConditionNameOrClassName: ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference)   | legacyQualifiedDataNameOrQualifiedConditionNameOrClassName;

legacyQualifiedDataName:                                    dataNameReference                                                                                  ((IN | OF) dataNameReferenceOrFileNameReference)*;
legacyQualifiedDataNameOrIndexName:                         dataNameReferenceOrIndexNameReference                                                              ((IN | OF) dataNameReferenceOrFileNameReference)*;
legacyQualifiedConditionName:                               conditionNameReferenceOrConditionForUPSISwitchNameReference                                        ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrConditionName:                     dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference                     ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrQualifiedConditionNameOrIndexName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrQualifiedConditionNameOrFileName:  dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference  ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
legacyQualifiedDataNameOrQualifiedConditionNameOrClassName: dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;