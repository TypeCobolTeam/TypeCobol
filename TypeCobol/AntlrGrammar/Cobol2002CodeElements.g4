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
	freeStatement |
	// JSON GENERATE
	jsonGenerateStatement |
	jsonStatementEnd |
	// JSON PARSE
	jsonParseStatement;

// Updated INITIALIZE statement using COBOL v6.1 specs
initializeStatement:
	INITIALIZE storageArea1+ (WITH? FILLER)?
	((ALL|categoryName=dataCategory) TO? VALUE)?
	(THEN? REPLACING initializeReplacingDirective+)?
	(THEN? TO? DEFAULT)?;

// New Cobol v6.1 ALLOCATE statement to obtain dynamic storage.
// 'INITIALIZED' is defined here as a contextual keyword. The storageArea2 is therefore not allowed to be named 'INITIALIZED' in this statement.
// LOC phrase, new in Cobol v6.2, controls how ALLOCATE acquires storage  
allocateStatement:
	ALLOCATE ((arithmeticExpression CHARACTERS) | { !string.Equals(CurrentToken.Text, "INITIALIZED", System.StringComparison.InvariantCultureIgnoreCase) }? storageArea2)
	({ string.Equals(CurrentToken.Text, "INITIALIZED", System.StringComparison.OrdinalIgnoreCase) }? KeywordINITIALIZED=UserDefinedWord)?
	({ string.Equals(CurrentToken.Text, "LOC", System.StringComparison.OrdinalIgnoreCase) }? KeywordLOC=UserDefinedWord integerVariable1)?
	(RETURNING pointerStorageArea)?;

// New Cobol v6.1 FREE statement that releases dynamic storage that was previously obtained with an ALLOCATE statement.
freeStatement:
	FREE pointerStorageArea+;

// New Cobol v6.1 JSON GENERATE statement. Allows generating JSON string from data item, similar to XML GENERATE.
jsonGenerateStatement:
	JSON GENERATE destination=storageArea1
	FROM source=variable1
	(COUNT IN? charactersCount=storageArea1)?
	(name OF? jsonNameMapping+)? // Re-use of contextual keyword NAME defined for XML GENERATE in CobolCodeElements.
	(SUPPRESS jsonSuppressDirective+)?
	(CONVERTING jsonGenerateConvertingPhrase)?;

jsonNameMapping:
	dataItem=variable1 IS? (outputName=alphanumericValue2 | OMITTED);

// Re-use of whenPhrase and nonnumeric defined for XML GENERATE in CobolCodeElements.
jsonSuppressDirective:
	(subordinateDataItem=variable1 whenPhrase?) | jsonGenericSuppressionPhrase;

jsonGenericSuppressionPhrase:
	(EVERY (NUMERIC | nonnumeric))? whenPhrase;

jsonGenerateConvertingPhrase:
	jsonGenerateConvertingDirective (ALSO jsonGenerateConvertingDirective)*;

jsonGenerateConvertingDirective:
	convertingDataItem=variable1 TO? JSON? (booleanWord | boolWord) USING? (conditionNameReference | alphanumericLiteralToken);

booleanWord:
	{string.Equals(CurrentToken.Text, "BOOLEAN", System.StringComparison.InvariantCultureIgnoreCase)}? BOOLEANKeyword=UserDefinedWord;
	
boolWord:
	{string.Equals(CurrentToken.Text, "BOOL", System.StringComparison.InvariantCultureIgnoreCase)}? BOOLKeyword=UserDefinedWord;

jsonStatementEnd:
	END_JSON;

// New Cobol v6.2 JSON PARSE statement. Converts JSON text to COBOL data formats.
jsonParseStatement:
	JSON parse source=storageArea1 // Re-use of contextual keyword PARSE defined for XML PARSE in CobolCodeElements.
	INTO destination=variable1
	(WITH? DETAIL)? 
	(name OF? jsonNameMapping+)? // Re-use of contextual keyword NAME defined for XML GENERATE in CobolCodeElements.
	(SUPPRESS excludedDataItem+)?
	(CONVERTING jsonParseConvertingPhrase)?;

excludedDataItem:
	variable1;
	
jsonParseConvertingPhrase:
	jsonParseConvertingDirective (ALSO jsonParseConvertingDirective)*;
	
jsonParseConvertingDirective:
	convertingDataItem=storageArea2 FROM? JSON? (booleanWord | boolWord) jsonParseUsingDirective;

jsonParseUsingDirective:
	USING? (jsonParseUsingDirective1 | jsonParseUsingDirective2 | jsonParseUsingDirective3);
	
jsonParseUsingDirective1:
	conditionNameReference;

jsonParseUsingDirective2: 
	conditionNameReference AND? conditionNameReference;
	
jsonParseUsingDirective3:
	alphanumericLiteralToken AND? alphanumericLiteralToken;