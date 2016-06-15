grammar Cobol2002CodeElements;

import CobolCodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

// --- Custom rules ---

dataDescriptionEntry:
	( { CurrentToken.Text != "66" && CurrentToken.Text != "88" }? 

		levelNumber (dataNameDefinition | FILLER)? redefinesClause? cob2002TypedefClause?
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
		| cob2002TypeClause
		)* PeriodSeparator
	)
	| dataRenamesEntry
	| dataConditionEntry;

cob2002TypedefClause: TYPEDEF STRONG?;

cob2002TypeClause:    TYPE UserDefinedWord;