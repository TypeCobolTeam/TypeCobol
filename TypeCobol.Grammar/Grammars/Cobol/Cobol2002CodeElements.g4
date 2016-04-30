grammar Cobol2002CodeElements;

import CobolCodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

// --- Custom rules ---

dataDescriptionEntry:
	( { CurrentToken.Text != "66" && CurrentToken.Text != "88" }? 

		levelNumber (dataNameDefinition | FILLER)? redefinesClause? tcExtTypedefClause?
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
		| tcExtTypeClause
		)* PeriodSeparator
	)
	| dataRenamesEntry
	| dataConditionEntry;

tcExtTypedefClause: TYPEDEF STRONG?;

tcExtTypeClause:    TYPE UserDefinedWord;