grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

tcCodeElement:
	  functionDeclarationHeader
	| functionDeclarationEnd
	;


qualifiedDataName1: cobolQualifiedDataName1 | tcQualifiedDataName1;
cobolQualifiedDataName1: // was qualifiedDataName1
	dataNameReference ((IN | OF) dataNameReferenceOrFileNameReference)+;
tcQualifiedDataName1: // new feature
	(ColonSeparator ColonSeparator dataNameReferenceOrFileNameReference)+ dataNameReference;

qualifiedConditionName: cobolQualifiedConditionName | tcQualifiedConditionName;
cobolQualifiedConditionName: // was qualifiedConditionName
	conditionNameReferenceOrConditionForUPSISwitchNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;
tcQualifiedConditionName: // new feature
	(dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference ColonSeparator ColonSeparator)* conditionNameReferenceOrConditionForUPSISwitchNameReference;



// rule modified to support:
// - TYPE DATE (instead of TC-DATE or something)
cobol2002TypeClause:    TYPE (UserDefinedWord | DATE);

// rule modified to support:
// - MOVE UNSAFE <custom type> TO <custom type>
// - MOVE TRUE  TO <boolean>
// - MOVE FALSE TO <boolean>
moveStatement:
    MOVE UNSAFE? (CORRESPONDING | CORR)? (booleanValue | variableOrFileName) TO identifier+;
//         ^                      ^       ^
//          \                      \       \
//           \                      --------------  MOVE [TRUE|FALSE] TO <boolean>
//            ------------------------------------  MOVE UNSAFE <custom type> TO <custom type>


// rule modified to support:
// - SET <boolean> TO FALSE
setStatementForConditions:
	SET conditionReference+ TO (TRUE | FALSE);

// rules modified to support custom-designed functions (of arity 0..n)
functionIdentifier: FUNCTION intrinsicFunctionName (LeftParenthesisSeparator argument* RightParenthesisSeparator)?;

// TCRFUN_NO_DEFAULT_ACCESS_MODIFIER
functionDeclarationHeader:
	DECLARE FUNCTION UserDefinedWord (PRIVATE | PUBLIC) PeriodSeparator;


// alternate PROCEDURE DIVISION to allow function declarations
// - TCRFUN_PARAMETER_DECLARATION_ORDER
// - TCRFUN_0_TO_N_PARAMETERS (possibly 0 parameters because of "?")
// - TCRFUN_0_TO_1_RETURNING_PARAMETER
//   - possibly 0 parameters because of "?" --> procedure or void-returning function
//   - returningPhrase only allows 1 parameter --> function
// - TCRFUN_DECLARATION_NO_USING
procedureDivisionHeader: PROCEDURE DIVISION ((usingPhrase? returningPhrase?) | (inputPhrase? inoutPhrase? outputPhrase? functionReturningPhrase?)) PeriodSeparator;

// TCRFUN_0_TO_N_PARAMETERS (1..N parameters because of "+")
//inputPhrase:  INPUT  programInputParameters+;
//outputPhrase: OUTPUT storageArea2+;
inputPhrase:  INPUT  parameterDescription+;
inoutPhrase:  INOUT  parameterDescription+;
outputPhrase: OUTPUT parameterDescription+;
functionReturningPhrase: RETURNING parameterDescription;

// parameterDescription is a rule created from dataDescriptionEntry
// and enforcing rules TCRFUN_PARAMETER_DESCRIPTION TCRFUN_LEVEL_88_PARAMETERS
parameterDescription: (functionDataParameter | functionConditionParameter) PeriodSeparator?;

functionDataParameter:
	dataNameDefinition (pictureClause|cobol2002TypeClause)
		( blankWhenZeroClause
		| justifiedClause
		| synchronizedClause
		| groupUsageClause
		| occursClause
		| signClause
		| usageClause
		| valueClause
		)*;

functionConditionParameter:
	levelNumber conditionNameDefinition valueClauseForCondition;

functionDeclarationEnd: END_DECLARE PeriodSeparator;
