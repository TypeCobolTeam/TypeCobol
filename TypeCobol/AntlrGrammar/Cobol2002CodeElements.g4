grammar Cobol2002CodeElements;

import CobolCodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

// --- Custom rules ---

dataDescriptionEntry:
	( { CurrentToken.Text != "66" && CurrentToken.Text != "88" }? 

		levelNumber=integerValue2 (dataNameDefinition | FILLER)? redefinesClause? cobol2002TypedefClause?
		( pictureClause
		| blankWhenZeroClause
		| externalClause
		| globalClause
		| justifiedClause
		| groupUsageClause
		| occursClause
		| signClause
		| synchronizedClause
		| usageClause
		| valueClause
		| cobol2002TypeClause
		)* PeriodSeparator
	)
	| dataRenamesEntry
	| dataConditionEntry;

// When this clause is matched, dataNameDefinition above is also a dataTypeNameDefinition
cobol2002TypedefClause: TYPEDEF STRONG?;

cobol2002TypeClause:    TYPE dataTypeNameReference;

// Updated INITIALIZE statement using COBOL v6.1 specs
initializeStatement:
	INITIALIZE storageArea1+ (WITH? FILLER)?
	((ALL|categoryName=dataCategory) TO? VALUE)?
	(THEN? REPLACING initializeReplacingDirective+)?
	(THEN? TO? DEFAULT)?;
