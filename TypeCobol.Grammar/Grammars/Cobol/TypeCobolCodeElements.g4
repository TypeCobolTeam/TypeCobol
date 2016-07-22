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

functionDeclarationHeader:
	DECLARE FUNCTION UserDefinedWord (PRIVATE | PUBLIC) PeriodSeparator;


// alternate PROCEDURE DIVISION to allow function declarations
// - no USING
// - INPUT and OUTPUT phrases
// - no nested function declaration
// - no DECLARATIVES
procedureDivisionHeader: PROCEDURE DIVISION inputPhrase? (outputPhrase | returningPhrase)? PeriodSeparator;

inputPhrase:  INPUT  programInputParameters+;
outputPhrase: OUTPUT storageArea2+;

functionDeclarationEnd: END_DECLARE PeriodSeparator;
