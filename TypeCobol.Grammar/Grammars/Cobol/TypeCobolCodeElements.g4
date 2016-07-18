grammar TypeCobolCodeElements;

import Cobol2002CodeElements;

// --- Starting rule ---
cobolCodeElements: codeElement* EOF;

tcCodeElement:
	  functionDeclarationHeader
	| functionDeclarationEnd
	;

qualifiedDataName:                                    (qDataOrFile*       dataNameReference                                                                                  (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedDataName;
qualifiedConditionName:                               (qDataOrFileOrUPSI* conditionNameReferenceOrConditionForUPSISwitchNameReference                                        (LeftParenthesisSeparator subscript RightParenthesisSeparator)?)   | legacyQualifiedConditionName;

qDataOrFile:       dataNameReferenceOrFileNameReference                                     (LeftParenthesisSeparator subscript RightParenthesisSeparator)? ColonSeparator ColonSeparator;
qDataOrFileOrUPSI: dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference (LeftParenthesisSeparator subscript RightParenthesisSeparator)? ColonSeparator ColonSeparator;

legacyQualifiedDataName:                                    dataNameReference                                                                                  ((IN | OF) dataNameReferenceOrFileNameReference)*;
legacyQualifiedConditionName:                               conditionNameReferenceOrConditionForUPSISwitchNameReference                                        ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;

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
setStatementForAssignationSending:
    identifier | IntegerLiteral
    | FALSE                         // <----- SET <boolean> TO FALSE
    | TRUE | (NULL | NULLS) | SELF
    | (ENTRY_ARG programNameOrProgramEntryVariable)
    ;

// rules modified to support custom-designed functions (of arity 0..n)
functionIdentifier: FUNCTION intrinsicFunctionName (LeftParenthesisSeparator argument* RightParenthesisSeparator)?;

functionDeclarationHeader:
	DECLARE FUNCTION UserDefinedWord (PRIVATE | PUBLIC) PeriodSeparator;


// alternate PROCEDURE DIVISION to allow function declarations
// - no USING
// - INPUT and OUTPUT phrases
// - no nested function declaration
// - no DECLARATIVES
procedureDivisionHeader: PROCEDURE DIVISION usingPhrase? inputPhrase? (outputPhrase | returningPhrase)? PeriodSeparator;

inputPhrase:  INPUT  programInputParameters+;
outputPhrase: OUTPUT storageArea2+;

functionDeclarationEnd: END_DECLARE PeriodSeparator;
