grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

qualifiedDataName:
	  ((dataNameReferenceOrFileNameReference ColonSeparator ColonSeparator)* dataNameReference)
	| legacyQualifiedDataName
	;
legacyQualifiedDataName: dataNameReference ((IN | OF) dataNameReferenceOrFileNameReference)*;

qualifiedConditionName:
	  ((dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* conditionNameReferenceOrConditionForUPSISwitchNameReference)
	| legacyQualifiedConditionName
	;
legacyQualifiedConditionName:
		conditionNameReferenceOrConditionForUPSISwitchNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;