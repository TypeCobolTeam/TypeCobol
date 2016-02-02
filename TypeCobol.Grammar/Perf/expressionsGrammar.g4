/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
parser grammar conditionalExpression;

// --------------------------------------
// SUMMARY of PERFORMANCE PROBLEMS
//
// 1) Conditional expressions
// 
// conditionalExpression/AdaptivePredict
// - simpleCondition | complexCondition
// 
// simpleCondition/AdaptivePredict
// - classCondition | conditionNameCondition | relationCondition | signCondition | switchStatusCondition
// 
// abbreviatedOr/AdaptivePredict
// - abbreviatedAND (OR abbreviatedAND)*
// 
// abbreviatedAnd/AdaptivePredict
// - abbreviatedNOT (AND abbreviatedNOT)*
// 
// 2) Identifiers and name references
// 
// identifier/AdaptivePredict
// - (dataNameReferenceOrSpecialRegisterOrFunctionIdentifier ...) | conditionNameReference
// 
// operand/AdaptivePredict
// - identifier | literal | arithmeticExpression | indexName
//
// callBy/AdaptivePredict
// - identifier | literal | fileName 
//
// 3) Name references and subscripts
//
// dataNameReference/AdaptivePredict
// - qualifiedDataName (LeftParenthesisSeparator subscript+ RightParenthesisSeparator)?
// 
// subscript/AdaptivePredict
// - (qualifiedDataName withRelativeSubscripting?) | (indexName withRelativeSubscripting?)
// -----------------------------------

// Referenced by :
// - evaluateStatement
expression:
              arithmeticExpression | conditionalExpression;    

// Referenced by :
// - expression (=> evaluateStatement)
// - ifStatement
// - performStatement
// - performProcedureStatement
// - whenConditionalExpression (=> evaluateStatement, searchStatement)
// - whenEvaluateCondition (=> evaluateStatement)
// - whenSearchCondition (=> searchStatement)

conditionalExpression:
                         LeftParenthesisSeparator conditionalExpression RightParenthesisSeparator
                      |  NOT conditionalExpression
                      |  conditionalExpression AND conditionalExpression
                      |  conditionalExpression OR conditionalExpression
                      |  classCondition        
                      |  conditionNameCondition
                      |  generalRelationCondition
                      |  pointerRelationCondition
                      // |  programPointerRelationCondition => impossible to distinguish at the parsing stage
                      // |  objectReferenceRelationCondition => impossible to distinguish at the parsing stage
                      |  signCondition
                      // |  switchStatusCondition => impossible to distinguish from conditionNameCondition at the parsing stage
	;

// Referenced by :
// - conditionalExpression
// - conditionBase
//INCLUDED complexCondition:
//INCLUDED                    andCondition (OR andCondition)*;

//INCLUDED andCondition:
//INCLUDED                notCondition (AND notCondition)*;

//INCLUDED notCondition:
//INCLUDED                NOT? conditionBase;

//INCLUDED conditionBase:
//INCLUDED                 simpleCondition | (LeftParenthesisSeparator complexCondition RightParenthesisSeparator);

// Referenced by :
// - conditionalExpression
// - conditionBase
//INCLUDED simpleCondition:
//INCLUDED                    classCondition         |
//INCLUDED                    conditionNameCondition |
//INCLUDED                    relationCondition      |
//INCLUDED                    signCondition          |
//INCLUDED                    switchStatusCondition;

classCondition :
                   identifier IS? NOT? (charsetClassName |
                                        (NUMERIC |
                                        ALPHABETIC |
                                        ALPHABETIC_LOWER |
                                        ALPHABETIC_UPPER |                                        
                                        DBCS |
                                        KANJI));

conditionNameCondition:
                        conditionNameReference;

// Referenced by :
// - conditionNameCondition
// - identifier
conditionNameReference:
                          qualifiedConditionName (LeftParenthesisSeparator subscript+ RightParenthesisSeparator)?;


signCondition:
                 operand IS? NOT? (POSITIVE | NEGATIVE |ZERO);

// p260: The subject of the relation condition. Can be an identifier, literal,
// function-identifier (already included in identifier), arithmetic expression, or index-name.

// Referenced by :
// - generalRelationCondition
// - abbreviatedOperand
// - signCondition
operand:
	identifier | literal | arithmeticExpression 
       // | indexName => can not be distinguished from identifier at the parsing stage
       ;

//INCLUDED switchStatusCondition: 
//INCLUDED                          qualifiedConditionName;


//INCLUDED relationCondition:
//INCLUDED                      generalRelationCondition |
//INCLUDED                      dataPointerRelationCondition |
//INCLUDED                      programPointerRelationCondition |
//INCLUDED                      objectReferenceRelationCondition;

generalRelationCondition:
	operand relationalOperator abbreviatedExpression;

//INCLUDED abbreviatedOR:  abbreviatedAND (OR abbreviatedAND)*;
//INCLUDED abbreviatedAND: abbreviatedNOT (AND abbreviatedNOT)*;
//INCLUDED abbreviatedNOT: NOT? abbreviatedExpression;
abbreviatedExpression:   (LeftParenthesisSeparator abbreviatedExpression RightParenthesisSeparator)
                       |  NOT abbreviatedExpression
                       |  abbreviatedExpression AND abbreviatedExpression
                       |  abbreviatedExpression OR abbreviatedExpression
                       |  relationalOperator operand
                       |  operand;

//INCLUDED abbreviatedOperand:
//INCLUDED 	relationalOperator? operand;

relationalOperator:
	IS? ((NOT? strictRelation) | simpleRelation);
	
strictRelation:
	  GREATER THAN?
	| GreaterThanOperator
	| LESS THAN?
	| LessThanOperator
	| EQUAL TO?
	| EqualOperator
	;

simpleRelation:
	  GREATER THAN? OR EQUAL TO?
	| GreaterThanOrEqualOperator
	| LESS THAN? OR EQUAL TO?
	| LessThanOrEqualOperator
	;

// Comparison of dataPointerRelationCondition / programPointerRelationCondition / objectReferenceRelationCondition
// ALL : pointer IS? NOT? ((EQUAL TO?) | EqualOperator) pointer
// dataPointer : (ADDRESS OF identifier) | identifier | NULL | NULLS
// programPointer : identifier | NULL | NULLS
// objectReference : identifier | SELF | NULL | NULLS
// => only difference : (ADDRESS OF identifier) / SELF
// identifier : already contains ADDRESS OF dataNameReference
// => add SELF / NULL / NULLS to identifier and all 3 rules are in fact exactly identical at the parsing stage
// generalRelationCondition : operand relationalOperator operand
//                        ==> operand IS? NOT? (EQUAL TO? | EqualOperator) operand
// operand : identifier ...
// => except for (ADDRESS OF identifier) | SELF | NULL | NULLS, generalRelationCondition is always matched first
// Conclusion : except for very special cases, these three rules are never matched

pointerRelationCondition:
                            specificPointerOperand relationConditionEquality specificPointerOperand;

specificPointerOperand:
                          (ADDRESS OF identifier) | identifier | (SELF | NULL | NULLS);

relationConditionEquality:
                             IS? NOT? ((EQUAL TO?) | EqualOperator);
                            
//INCLUDED dataPointerRelationCondition:
//INCLUDED 	dataPointer relationConditionEquality dataPointer;

// Referenced by :
// - dataPointerRelationCondition
//INCLUDED dataPointer:
//INCLUDED 	(ADDRESS OF identifier) | identifier | NULL | NULLS;

//INCLUDED programPointerRelationCondition:
//INCLUDED 	procedureOrFunctionPointer relationConditionEquality procedureOrFunctionPointer;

// Referenced by :
// - programPointerRelationCondition
//INCLUDED procedureOrFunctionPointer:
//INCLUDED 	identifier | NULL | NULLS;

//INCLUDED objectReferenceRelationCondition:
//INCLUDED 	objectReference relationConditionEquality objectReference;

// Referenced by :
// - objectReferenceRelationCondition
//INCLUDED objectReference:
//INCLUDED 	identifier | SELF | NULL | NULLS;

// Referenced by :
// - computeStatement
// - evaluateStatement
// - expression
// - whenSearchCondition
// - expressionBase
// - operand
// - argument
// - referenceModifier
arithmeticExpression:   (LeftParenthesisSeparator arithmeticExpression RightParenthesisSeparator)
                      | (PlusOperator | MinusOperator)? arithmeticExpression
                      |  arithmeticExpression PowerOperator<assoc=right> arithmeticExpression
                      |  arithmeticExpression (MultiplyOperator | DivideOperator) arithmeticExpression 
                      |  arithmeticExpression (PlusOperator | MinusOperator) arithmeticExpression 
                      | identifier 
                      | numericLiteral;

//INCLUDED multiplicationAndDivision:
//INCLUDED 	exponentiation arithEXPTail*;

//INCLUDED arithMADTail:
//INCLUDED 	(PlusOperator | MinusOperator) multiplicationAndDivision;

//INCLUDED arithEXPTail:
//INCLUDED 	(MultiplyOperator | DivideOperator) exponentiation;

//INCLUDED exponentiation:
//INCLUDED                   unaryOperator (PowerOperator unaryOperator)*;

//INCLUDED unaryOperator:
//INCLUDED                  (PlusOperator | MinusOperator)? expressionBase;

//INCLUDED expressionBase:
//INCLUDED                    identifier | numericLiteral | (LeftParenthesisSeparator arithmeticExpression RightParenthesisSeparator);


// Referenced by :
// - addStatement
// - divideStatement
// - multiplyStatement
// - subtractStatement

identifierOrNumericLiteral:
		identifier | numericLiteral;

// Referenced by :
// *** ALL STATEMENTS ***
// - identifierOrNumericLiteral
// - identifierRounded
// - identifierOrLiteral

identifier:
	  dataNameReferenceOrSpecialRegisterOrFunctionIdentifier (LeftParenthesisSeparator referenceModifier RightParenthesisSeparator)?
	// | conditionNameReference => can not be distinguished from dataNameReference at this stage
          ;

dataNameReferenceOrSpecialRegisterOrFunctionIdentifier:
            dataNameReference |
            specialRegister |
            addressOfSpecialRegisterDecl |
            lengthOfSpecialRegisterDecl |
            linageCounterSpecialRegisterDecl |
            functionIdentifier;
			
addressOfSpecialRegisterDecl:
			ADDRESS OF dataNameReference;
			
lengthOfSpecialRegisterDecl:						 
			LENGTH OF? dataNameReference;

linageCounterSpecialRegisterDecl:
			LINAGE_COUNTER OF fileName;

// ONLY Referenced by :
// - dataNameReferenceOrSpecialRegisterOrFunctionIdentifier
dataNameReference:
                     qualifiedDataName (LeftParenthesisSeparator subscript+ RightParenthesisSeparator)?;

// ONLY Referenced by :
// - dataNameReferenceOrSpecialRegisterOrFunctionIdentifier
functionIdentifier:
	FUNCTION intrinsicFunctionName (LeftParenthesisSeparator argument+ RightParenthesisSeparator)?;

// ONLY Referenced by :
// - functionIdentifier
argument:
            identifier | // an identifier can be a special register or a functionIdentifier
            literal | 
            arithmeticExpression;

// ONLY Referenced by :
// - identifier
referenceModifier:
                     leftMostCharacterPosition ColonSeparator length?;

leftMostCharacterPosition: arithmeticExpression;
length: arithmeticExpression;

// Referenced by :
// - dataNameReference
// - conditionNameReference
subscript:
	IntegerLiteral | ALL  |
	(qualifiedDataName withRelativeSubscripting?)
         // | (indexName withRelativeSubscripting?) => can not be distinguished from the previous line at the parsing stage
         ;

withRelativeSubscripting:
			(PlusOperator | MinusOperator) IntegerLiteral;

// Referenced by :
// - releaseStatement
// - rewriteStatement
// - sortStatement
// - startStatement
// - writeStatement
qualifiedDataName:
	dataNameBase ((IN | OF) dataName)* ((IN | OF) fileName)?;

dataNameBase: dataName;

// Referenced by :
// - conditionNameReference
// - switchStatusCondition
qualifiedConditionName:
	conditionName ((IN | OF) dataName)* ((IN | OF) fileName)?;

// --- Terminals ---

// Referenced by :
// - operand
// - argument
// - fdValue
// - valueOfClause
// - valueClause
// ...
literal:
           alphanumOrNationalLiteral | numericLiteral;

// Alphanumeric literals

// Referenced by :
// - methodName
// - paddingCharacterClause
// - invokeStatement
// - xmlGenerateStatement
alphanumOrNationalLiteral:
           alphanumOrNationalLiteralBase |
           NullTerminatedAlphanumericLiteral |
           (ALL alphanumOrNationalLiteralBase);

alphanumOrNationalLiteralBase:
                                 (AlphanumericLiteral |
                                  HexadecimalAlphanumericLiteral |                      
                                  NationalLiteral |
                                  HexadecimalNationalLiteral |
                                  DBCSLiteral) |
                                 figurativeConstant;

// ONLY Referenced by :
// - whenPhrase (xmlGenerateStatement)
figurativeConstant:
	(HIGH_VALUE | HIGH_VALUES |
	LOW_VALUE  | LOW_VALUES |
	NULL  | NULLS |
	QUOTE | QUOTES |
	SPACE | SPACES |
	ZERO  | ZEROS  | ZEROES |
	SymbolicCharacter);

// Numeric literals

// Referenced by :
// - identifierOrNumericLiteral
// - performStatement (identifier | numericLiteral)
// - performProcedureStatement (identifier | numericLiteral)
// - expressionBase
numericLiteral: 
                  (IntegerLiteral | DecimalLiteral | FloatingPointLiteral | ZERO | ZEROS | ZEROES);

// Special registers

// ONLY Referenced by :
// - dataNameReferenceOrSpecialRegisterOrFunctionIdentifier
specialRegister : 
   (DEBUG_CONTENTS |
    DEBUG_ITEM |
    DEBUG_LINE |
    DEBUG_NAME |
    DEBUG_SUB_1 |
    DEBUG_SUB_2 |
    DEBUG_SUB_3 |
    JNIENVPTR |
    LINAGE_COUNTER |
    RETURN_CODE |
    SHIFT_IN |
    SHIFT_OUT |
    SORT_CONTROL |
    SORT_CORE_SIZE |
    SORT_FILE_SIZE |
    SORT_MESSAGE |
    SORT_MODE_SIZE |
    SORT_RETURN |
    TALLY |
    WHEN_COMPILED |
    XML_CODE |
    XML_EVENT |
    XML_INFORMATION |
    XML_NAMESPACE |
    XML_NAMESPACE_PREFIX |
    XML_NNAMESPACE |
    XML_NNAMESPACE_PREFIX |
    XML_NTEXT |
    XML_TEXT);

// Instrinsic function names

// ONLY Referenced by :
// - functionIdentifier
intrinsicFunctionName: (FunctionName | LENGTH | RANDOM | WHEN_COMPILED);

// User defined words

// Referenced by :
// - classClause
// - classCondition 
charsetClassName : UserDefinedWord;

// Referenced by :
// - qualifiedConditionName
// - conditionNameReferenceInSpecialNamesParagraph
// - onConditionNameForUPSISwitch (specialNamesParagraph)
// - offConditionNameForUPSISwitch (specialNamesParagraph)
conditionName : UserDefinedWord;

// Referenced by :
// - paddingCharacterClause
// - recordKeyClause
// - passwordClause
// - fileStatusClause
// - fdRecord3
// - fdLabel1 / fdValue / fdData / fd* ...
// - MANY clauses
// - dataDescriptionEntry
// - inputParameters (procedureDivisionHeader)
// - returningPhrase (procedureDivisionHeader)
// - procedurePointer (callStatement)
// - functionPointer (callStatement)
// - readStatement
// - searchStatement
// - qualifiedDataName
// - qualifiedConditionName
dataName : UserDefinedWord;

// Referenced by :
// - selectClause
// - rerunClause / sameAreaClause / multipleFileTapeClause / applyWriteOnlyClause (ioControlEntry)
// - fdFormat*
// - fileDescriptionEntry
// - useStatementForExceptionDeclarative
// - callBy (callStatement)
// - closeStatement
// - deleteStatement
// - mergeStatement
// - openStatement
// - readStatement
// - returnStatement
// - sortStatement
// - startStatement
// - qualifiedDataName
// - qualifiedConditionName
// - linageCounterSpecialRegisterDecl
// - 
fileName: UserDefinedWord;

// Referenced by :
// - occursClause
// - performProcedureStatement
// - searchStatement
// - setStatement
// - operand
// - subscript
indexName: UserDefinedWord;

