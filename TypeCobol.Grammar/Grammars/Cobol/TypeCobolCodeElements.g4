grammar TypeCobolCodeElements;

import CobolCodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

// --- Custom rules ---

dataDescriptionEntry:
	levelNumber (dataName | FILLER)?
	  renamesClause?
	  redefinesClause?
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
	| tcExtTypedefClause
	| tcExtTypeClause
	)* PeriodSeparator;

tcExtTypedefClause: TYPE;
tcExtTypeClause:    TYPE AlphanumericLiteral;