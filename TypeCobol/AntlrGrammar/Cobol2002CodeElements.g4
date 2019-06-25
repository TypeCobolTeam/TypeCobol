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

cobol2002Statement:
    // Dynamic allocation statements
	allocateStatement |
	freeStatement;

// Updated INITIALIZE statement using COBOL v6.1 specs
initializeStatement:
	INITIALIZE storageArea1+ (WITH? FILLER)?
	((ALL|categoryName=dataCategory) TO? VALUE)?
	(THEN? REPLACING initializeReplacingDirective+)?
	(THEN? TO? DEFAULT)?;

// New Cobol v6.1 ALLOCATE statement to obtain dynamic storage.
// 'INITIALIZED' is defined here as a contextual keyword. The dataNameReference is therefore not allowed to be named 'INITIALIZED' in this statement.
allocateStatement:
	ALLOCATE ((arithmeticExpression CHARACTERS) | { !string.Equals(CurrentToken.Text, "INITIALIZED", System.StringComparison.InvariantCultureIgnoreCase) }? dataNameReference)
	({ string.Equals(CurrentToken.Text, "INITIALIZED", System.StringComparison.InvariantCultureIgnoreCase) }? KeywordINITIALIZED=UserDefinedWord)?
	(RETURNING dataItemReference)?;

// New Cobol v6.1 FREE statement that releases dynamic storage that was previously obtained with an ALLOCATE statement.
freeStatement:
	FREE dataItemReference+;
