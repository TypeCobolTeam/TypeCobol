grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

qualifiedDataName:
	  ((dataName ColonSeparator ColonSeparator)* dataNameBase) // first dataName can be a fileName
	| legacyQualifiedName
	;

legacyQualifiedName: dataNameBase ((IN | OF) dataName)*;  // last dataName can be a fileName

qualifiedConditionName:
	  ((dataName ColonSeparator ColonSeparator)* conditionName) // first dataName can be a fileName
	| legacyQualifiedConditionName
	;

legacyQualifiedConditionName: conditionName ((IN | OF) dataName)*;  // last dataName can be a fileName