grammar Cobol2002CodeElements;

import CobolCodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

// --- Custom rules ---

dataDescriptionEntry:
	( { CurrentToken.Text != "66" && CurrentToken.Text != "88" }? 

		levelNumber (dataNameDefinition | FILLER)? redefinesClause? cobol2002TypedefClause?
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

cobol2002TypedefClause: TYPEDEF STRONG?;

cobol2002TypeClause:    TYPE UserDefinedWord;