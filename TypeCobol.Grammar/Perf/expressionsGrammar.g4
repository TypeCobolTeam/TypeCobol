
grammar Expressions;

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

expression: arithmeticExpression | conditionalExpression;


// p256: Conditional expressions
// A conditional expression causes the object program to select alternative paths of
// control, depending on the truth value of a test. Conditional expressions are
// specified in EVALUATE, IF, PERFORM, and SEARCH statements.
// A conditional expression can be specified in either simple conditions or complex
// conditions. Both simple and complex conditions can be enclosed within any
// number of paired parentheses; the parentheses do not change whether the
// condition is simple or complex.

// p270: Complex conditions
// A complex condition is formed by combining simple conditions, combined
// conditions, or complex conditions with logical operators, or negating those
// conditions with logical negation.
// Each logical operator must be preceded and followed by a space. The following
// table shows the logical operators and their meanings.
// Table 25. Logical operators and their meanings
// Logical operator | Name | Meaning
// AND | Logical conjunction | The truth value is true when both conditions are true.
// OR | Logical inclusive OR | The truth value is true when either or both conditions are true.
// NOT | Logical negation | Reversal of truth value (the truth value is true if the condition is false).
// Unless modified by parentheses, the following list is the order of precedence (from
// highest to lowest):
// 1. Arithmetic operations
// 2. Simple conditions
// 3. NOT
// 4. AND
// 5. OR
// The truth value of a complex condition (whether parenthesized or not) is the truth
// value that results from the interaction of all the stated logical operators on either of
// the following options:
// - The individual truth values of simple conditions
// - The intermediate truth values of conditions logically combined or logically
//   negated
// A complex condition can be either of the following options:
// - A negated simple condition
// - A combined condition (which can be negated)

// p272: Combined conditions
// Two or more conditions can be logically connected to form a combined condition.
// The condition to be combined can be any of the following ones:
// - A simple-condition
// - A negated simple-condition
// - A combined condition
// - A negated combined condition (that is, the NOT logical operator followed by a
//   combined condition enclosed in parentheses)
// - A combination of the preceding conditions that is specified according to the
//  rules in the following table

// ... p272: Table 26. Combined conditions�permissible element sequences ...

// p273: Order of evaluation of conditions
// Parentheses, both explicit and implicit, define the level of inclusiveness within a
// complex condition. Two or more conditions connected by only the logical operators
// AND or OR at the same level of inclusiveness establish a hierarchical level within
// a complex condition. Therefore an entire complex condition is a nested structure of
// hierarchical levels, with the entire complex condition being the most inclusive
// hierarchical level.
// Within this context, the evaluation of the conditions within an entire complex
// condition begins at the left of the condition. The constituent connected conditions
// within a hierarchical level are evaluated in order from left to right, and evaluation
// of that hierarchical level terminates as soon as a truth value for it is determined,
// regardless of whether all the constituent connected conditions within that
// hierarchical level have been evaluated.
// Values are established for arithmetic expressions and functions if and when the
// conditions that contain them are evaluated. Similarly, negated conditions are
// evaluated if and when it is necessary to evaluate the complex condition that they
// represent. For example:
// NOT A IS GREATER THAN B OR A + B IS EQUAL TO C AND D IS POSITIVE
// is evaluated as if parenthesized as follows:
// (NOT (A IS GREATER THAN B)) OR
// (((A + B) IS EQUAL TO C) AND (D IS POSITIVE))
// Order of evaluation:
// 1. (NOT (A IS GREATER THAN B)) is evaluated, giving some intermediate truth
//    value, t1. If t1 is true, the combined condition is true, and no further evaluation
//    takes place. If t1 is false, evaluation continues as follows.
// 2. (A + B) is evaluated, giving some intermediate result, x.
// 3. (x IS EQUAL TO C) is evaluated, giving some intermediate truth value, t2. If t2 is
//    false, the combined condition is false, and no further evaluation takes place. If
//    t2 is true, the evaluation continues as follows.
//4. (D IS POSITIVE) is evaluated, giving some intermediate truth value, t3. If t3 is
//   false, the combined condition is false. If t3 is true, the combined condition is
//   true.

// p271: Negated simple conditions
// A simple condition is negated through the use of the logical operator NOT.
// The negated simple condition gives the opposite truth value of the simple
// condition. That is, if the truth value of the simple condition is true, then the truth
// value of that same negated simple condition is false, and vice versa.
// Placing a negated simple condition within parentheses does not change its truth
// value. That is, the following two statements are equivalent:
// NOT A IS EQUAL TO B.
// NOT (A IS EQUAL TO B).

// p272: Parentheses are never needed when either ANDs or ORs (but not both) are used
// exclusively in one combined condition. However, parentheses might be needed to
// modify the implicit precedence rules to maintain the correct logical relation of
// operators and operands.
// There must be a one-to-one correspondence between left and right parentheses,
// with each left parenthesis to the left of its corresponding right parenthesis.

conditionalExpression:
// Complex conditions
	   LeftParenthesisSeparator conditionalExpression RightParenthesisSeparator
	|  NOT conditionalExpression
	|  conditionalExpression AND conditionalExpression
	|  conditionalExpression OR conditionalExpression
// Simple conditions:
	|  classCondition
	|  conditionNameCondition
	|  generalRelationCondition
	|  pointerRelationCondition
//	|  programPointerRelationCondition  // impossible to distinguish at the parsing stage
//	|  objectReferenceRelationCondition // impossible to distinguish at the parsing stage
	|  signCondition
//	|  switchStatusCondition // impossible to distinguish from conditionNameCondition at the parsing stage
	;

// p256: There are five simple conditions.
// The simple conditions are:
// - Class condition
// - Condition-name condition
// - Relation condition
// - Sign condition
// - Switch-status condition
// A simple condition has a truth value of either true or false.

// p256: Class condition
// The class condition determines whether the content of a data item is alphabetic,
// alphabetic-lower, alphabetic-upper, numeric, DBCS, KANJI, or contains only the
// characters in the set of characters specified by the CLASS clause as defined in the
// SPECIAL-NAMES paragraph of the ENVIRONMENT DIVISION.
// identifier-1
// Must reference a data item described with one of the following usages:
// - DISPLAY, NATIONAL, COMPUTATIONAL-3, or PACKED-DECIMAL
//   when NUMERIC is specified
// - DISPLAY-1 when DBCS or KANJI is specified
// - DISPLAY or NATIONAL when ALPHABETIC, ALPHABETIC-UPPER, or
//   ALPHABETIC-LOWER is specified
// - DISPLAY when class-name is specified
// Must not be of class alphabetic when NUMERIC is specified.
// Must not be of class numeric when ALPHABETIC, ALPHABETIC-UPPER,
// or ALPHABETIC-LOWER is specified.
// Table 19 on page 258 lists the forms of class condition that are valid for
// each type of identifier.
// If identifier-1 is a function-identifier, it must reference an alphanumeric or
// national function.
// An alphanumeric group item can be used in a class condition where an
// elementary alphanumeric item can be used, except that the NUMERIC class
// condition cannot be used if the group contains one or more signed
// elementary items.
// When identifier-1 is described with usage NATIONAL, the class-condition
// tests for the national character representation of the characters associated
// with the specified character class. For example, specifying a class condition
// of the form IF national-item IS ALPHABETIC is a test for the lowercase
// and uppercase letters Latin capital letter A through Latin capital letter Z
// and the space, as represented in national characters. Specifying IF
// national-item is NUMERIC is a test for the characters 0 through 9. 

// ... more details on the evaluation of classCondition p257/258 ...

classCondition:
	identifier IS? NOT? (charsetClassName | (NUMERIC | ALPHABETIC | ALPHABETIC_LOWER | ALPHABETIC_UPPER | DBCS | KANJI));

// p258: Condition-name condition
// A condition-name condition tests a conditional variable to determine whether its
// value is equal to any values that are associated with the condition-name.
// A condition-name is used in conditions as an abbreviation for the relation
// condition. The rules for comparing a conditional variable with a condition-name
// value are the same as those specified for relation conditions.
// If condition-name-1 has been associated with a range of values (or with several
// ranges of values), the conditional variable is tested to determine whether its value
// falls within the ranges, including the end values. The result of the test is true if one
// of the values that corresponds to the condition-name equals the value of its
// associated conditional variable.
// Condition-names are allowed for alphanumeric, DBCS, national, and floating-point
// data items, as well as others, as defined for the condition-name format of the
// VALUE clause.
// The following example illustrates the use of conditional variables and
// condition-names:
// 01 AGE-GROUP PIC 99.
// 88 INFANT VALUE 0.
// 88 BABY VALUE 1, 2.
// 88 CHILD VALUE 3 THRU 12.
// 88 TEENAGER VALUE 13 THRU 19.
// AGE-GROUP is the conditional variable; INFANT, BABY, CHILD, and TEENAGER
// are condition-names. For individual records in the file, only one of the values
// specified in the condition-name entries can be present.
// The following IF statements can be added to the above example to determine the
// age group of a specific record:
// IF INFANT... (Tests for value 0)
// IF BABY... (Tests for values 1, 2)
// IF CHILD... (Tests for values 3 through 12)
// IF TEENAGER... (Tests for values 13 through 19)
// Depending on the evaluation of the condition-name condition, alternative paths of
// execution are taken by the object program.

conditionNameCondition: conditionNameReference;



// p259: Relation conditions
// A relation condition specifies the comparison of two operands. The relational
// operator that joins the two operands specifies the type of comparison. The relation
// condition is true if the specified relation exists between the two operands; the
// relation condition is false if the specified relation does not exist.
// Comparisons are defined for the following cases:
// - Two operands of class alphabetic
// - Two operands of class alphanumeric
// - Two operands of class DBCS
// - Two operands of class national
// - Two operands of class numeric
// - Two operands of different classes where each operand is one of the classes
//   alphabetic, alphanumeric, or national
// - Two operands where one is a numeric integer and the other is class
//   alphanumeric or national
// - Two operands where one is class DBCS and the other is class national
// - Comparisons involving indexes or index data items
// - Two data pointer operands
// - Two procedure pointer operands
// - Two function pointer operands
// - Two object reference operands
// - An alphanumeric group and any operand that has usage DISPLAY, DISPLAY-1,
//   or NATIONAL
// The following relation condition formats are defined:
// - A general relation condition, for comparisons that involve only data items,
//   literals, index-names, or index data items. For details, see �General relation
//   conditions.�
// - A data pointer relation condition. For details, see �Data pointer relation
//   conditions� on page 267.
// - A program pointer relation condition, for comparison of procedure pointers or
//   function pointers. For details, see �Procedure-pointer and function-pointer
//   relation conditions� on page 268.
// - An object-reference relation condition. For details, see �Object-reference relation
//   conditions� on page 269.

// p260: General relation conditions
// A general relation condition compares two operands, either of which can be an
// identifier, literal, arithmetic expression, or index-name.
// operand-1
// The subject of the relation condition. Can be an identifier, literal,
// function-identifier, arithmetic expression, or index-name.
// operand-2
// The object of the relation condition. Can be an identifier, literal,
// function-identifier, arithmetic expression, or index-name.
// An alphanumeric literal can be enclosed in parentheses within a relation condition.
// The relational operators, shown in Table 20 on page 261, specify the type of
// comparison to be made. Each relational operator must be preceded and followed
// by a space. The two characters of the relational operators >= and <= must not have
// a space between them.
// Table 20. Relational operators and their meanings
// Relational operator | Can be written | Meaning
// IS GREATER THAN | IS > | Greater than
// IS NOT GREATER THAN |IS NOT > | Not greater than
// IS LESS THAN | IS < | Less than
// IS NOT LESS THAN | IS NOT < | Not less than
// IS EQUAL TO | IS = | Equal to
// IS NOT EQUAL TO | IS NOT = |Not equal to
// IS GREATER THAN OR EQUAL TO | IS >= | Is greater than or equal to
// IS LESS THAN OR EQUAL TO | IS <= | Is less than or equal to
// In a general relation condition, data items, literals, and figurative constants of class
// alphabetic, alphanumeric, DBCS, national, and numeric are compared using the
// following comparison types:
// Comparison type | Meaning
// Alphanumeric | Comparison of the alphanumeric character value of two
// operands
// DBCS | Comparison of the DBCS character value of two operands
// National | Comparison of the national character value of two operands
// Numeric | Comparison of the algebraic value of two operands
// Group | Comparison of the alphanumeric character value of two
// operands, where one or both operands is an alphanumeric
// group item
// Table 21 on page 262 and Table 22 on page 263 show the permissible pairs for
// comparisons with different types of operands. The comparison type is indicated at
// the row and column intersection for permitted comparisons, using the following
// key:
// Alph Comparison of alphanumeric characters (further described in
// �Alphanumeric comparisons� on page 263)
// DBCS Comparison of DBCS characters (further described in �DBCS comparisons�
// on page 265)
// Nat Comparison of national characters (further described in �National
// comparisons� on page 265)
// Num Comparison of algebraic value (further described in �Numeric
// comparisons� on page 266)
// Group Comparison of alphanumeric characters involving an alphanumeric group
// (further described in �Group comparisons� on page 266)
// (Int) Integer items only (combined with comparison type Alph, Nat, Num, or
// Group)
// Blank Comparison is not allowed
// For rules and restrictions for comparisons involving index-names and index data
// items, see �Comparison of index-names and index data items� on page 267. 

// ... p262 to p267 : many more details on comparisons ...

generalRelationCondition: operand relationalOperator abbreviatedExpression;


// p274: Abbreviated combined relation conditions

abbreviatedExpression:
	  (LeftParenthesisSeparator abbreviatedExpression RightParenthesisSeparator)
	|  NOT abbreviatedExpression
	|  abbreviatedExpression AND abbreviatedExpression
	|  abbreviatedExpression OR abbreviatedExpression
	|  relationalOperator operand
	|  operand;

// p260: The subject of the relation condition. Can be an identifier, literal,
// function-identifier (already included in identifier), arithmetic expression, or index-name.

operand: identifier | literal | arithmeticExpression; // | indexName cannot be distinguished from identifier at the parsing stage

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



// p267: Data pointer relation conditions
// Only EQUAL and NOT EQUAL are allowed as relational operators when
// specifying pointer data items.
// Pointer data items are items defined explicitly as USAGE POINTER, or are
// ADDRESS OF special registers, which are implicitly defined as USAGE POINTER.
// The operands are equal if the two addresses used in the comparison would both
// result in the same storage location.
// This relation condition is allowed in IF, PERFORM, EVALUATE, and SEARCH
// format-1 statements. It is not allowed in SEARCH format-2 (SEARCH ALL)
// statements because there is no meaningful ordering that can be applied to pointer
// data items.
// identifier-1 , identifier-3
// Can specify any level item defined in the LINKAGE SECTION, except 66
// and 88.
// identifier-2 , identifier-4
// Must be described as USAGE POINTER.
// NULL, NULLS
// Can be used only if the other operand is defined as USAGE POINTER.
// That is, NULL=NULL is not allowed.
// The following table summarizes the permissible comparisons for USAGE
// POINTER, NULL, and ADDRESS OF.

// ... p268: Table 24. Permissible comparisons for USAGE POINTER, NULL, and ADDRESS OF ...
                
// p268: Procedure-pointer and function-pointer relation conditions
// Only EQUAL and NOT EQUAL are allowed as relational operators when
// specifying procedure-pointer or function-pointer data items in a relation condition.
// Procedure-pointer data items are defined explicitly as USAGE
// PROCEDURE-POINTER. Function-pointer data items are defined explicitly as
// USAGE FUNCTION-POINTER.
// The operands are equal if the two addresses used in the comparison would both
// result in the same storage location.
// This relation condition is allowed in IF, PERFORM, EVALUATE, and SEARCH
// format-1 statements. It is not allowed in SEARCH format-2 (SEARCH ALL)
// statements, because there is no meaningful ordering that can be applied to
// procedure-pointer data items.
// identifier-1, identifier-2
// Must be described as USAGE PROCEDURE-POINTER or USAGE
// FUNCTION-POINTER. identifier-1 and identifier-2 need not be described the
// same.
// NULL, NULLS
// Can be used only if the other operand is defined as USAGE
// FUNCTION-POINTER or USAGE PROCEDURE-POINTER. That is,
// NULL=NULL is not allowed.

// p269: Object-reference relation conditions
// A data item of usage OBJECT REFERENCE can be compared for equality or
// inequality with another data item of usage OBJECT REFERENCE or with NULL,
// NULLS, or SELF.
// A comparison with SELF is allowed only in a method.
// Two object-references compare equal only if the data items identify the same
// object.

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

pointerRelationCondition: specificPointerOperand relationConditionEquality specificPointerOperand;

specificPointerOperand: (ADDRESS OF identifier) | identifier | (SELF | NULL | NULLS);

relationConditionEquality: IS? NOT? ((EQUAL TO?) | EqualOperator);

// p269: Sign condition
// The sign condition determines whether the algebraic value of a numeric operand is
// greater than, less than, or equal to zero.
// operand-1
// Must be defined as a numeric identifier, or as an arithmetic expression that
// contains at least one reference to a variable. operand-1 can be defined as a
// floating-point identifier.
// The operand is:
// - POSITIVE if its value is greater than zero
// - NEGATIVE if its value is less than zero
// - ZERO if its value is equal to zero
// An unsigned operand is either POSITIVE or ZERO.
// NOT 
// One algebraic test is executed for the truth value of the sign condition. For
// example, NOT ZERO is regarded as true when the operand tested is
// positive or negative in value.
// The results of the sign condition test depend on the setting of the NUMPROC
// compiler option. For details, see NUMPROC in the Enterprise COBOL Programming
// Guide.

signCondition: operand IS? NOT? (POSITIVE | NEGATIVE |ZERO);

// p270: Switch-status condition
// The switch-status condition determines the on or off status of a UPSI switch.
// condition-name
// Must be defined in the special-names paragraph as associated with the on
// or off value of an UPSI switch. (See �SPECIAL-NAMES paragraph� on
// page 112.)
// The switch-status condition tests the value associated with condition-name. (The
// value is considered to be alphanumeric.) The result of the test is true if the UPSI
// switch is set to the value (0 or 1) corresponding to condition-name.
                 
// switchStatusCondition: qualifiedConditionName;










// p254: Arithmetic expressions
// Arithmetic expressions are used as operands of certain conditional and arithmetic
// statements.
// An arithmetic expression can consist of any of the following items:
// 1. An identifier described as a numeric elementary item (including numeric
// functions)
// 2. A numeric literal
// 3. The figurative constant ZERO
// 4. Identifiers and literals, as defined in items 1, 2, and 3, separated by arithmetic
//    operators
// 5. Two arithmetic expressions, as defined in items 1, 2, 3, or 4, separated by an
//    arithmetic operator
// 6. An arithmetic expression, as defined in items 1, 2, 3, 4, or 5, enclosed in
//    parentheses
// Any arithmetic expression can be preceded by a unary operator.
// Identifiers and literals that appear in arithmetic expressions must represent either
// numeric elementary items or numeric literals on which arithmetic can be
// performed.

// p254: Arithmetic operators
// Five binary arithmetic operators and two unary arithmetic operators can be used in
// arithmetic expressions. These operators are represented by specific characters that
// must be preceded and followed by a space.
// These binary and unary arithmetic operators are shown in Table 17.
// Table 17. Binary and unary operators
// Binary operator | Meaning
// + Addition
// - Subtraction
// * Multiplication
// / Division
// ** Exponentiation
// Unary operator | Meaning
// + Multiplication by +1
// - Multiplication by -1

// p254: Exponentiation
// If an exponential expression is evaluated as both a positive and a negative number,
// the result is always the positive number. For example, the square root of 4:
// 4 ** 0.5
// is evaluated as +2 and -2. Enterprise COBOL always returns +2.
// If the value of an expression to be raised to a power is zero, the exponent must
// have a value greater than zero. Otherwise, the size error condition exists. In any
// case where no real number exists as the result of an evaluation, the size error
// condition exists.
// Limitation: Exponents in fixed-point exponential expressions cannot contain more
// than nine digits. The compiler will truncate any exponent with more than nine
// digits. In the case of truncation, the compiler will issue a diagnostic message if the
// exponent is a literal or constant; if the exponent is a variable or data-name, a
// diagnostic message is issued at run time.

// p254: Parentheses can be used in arithmetic expressions to specify the order in which
// elements are to be evaluated.
// Expressions within parentheses are evaluated first. When expressions are contained
// within nested parentheses, evaluation proceeds from the least inclusive to the most
// inclusive set.
// When parentheses are not used, or parenthesized expressions are at the same level
// of inclusiveness, the following hierarchic order is implied:
// 1. Unary operator
// 2. Exponentiation
// 3. Multiplication and division
// 4. Addition and subtraction
//Parentheses either eliminate ambiguities in logic where consecutive operations
//appear at the same hierarchic level, or modify the normal hierarchic sequence of
//execution when this is necessary. When the order of consecutive operations at the
//same hierarchic level is not completely specified by parentheses, the order is from
//left to right.
//An arithmetic expression can begin only with a left parenthesis, a unary operator,
//or an operand (that is, an identifier or a literal). It can end only with a right
//parenthesis or an operand. An arithmetic expression must contain at least one
//reference to an identifier or a literal.
//There must be a one-to-one correspondence between left and right parentheses in
//an arithmetic expression, with each left parenthesis placed to the left of its
//corresponding right parenthesis.
//If the first operator in an arithmetic expression is a unary operator, it must be
//immediately preceded by a left parenthesis if that arithmetic expression
//immediately follows an identifier or another arithmetic expression.

// p255: The following table shows permissible arithmetic symbol pairs. An arithmetic
// symbol pair is the combination of two such symbols in sequence. In the table:
// Yes Indicates a permissible pairing.
// No Indicates that the pairing is not permitted.
// Table 18. Valid arithmetic symbol pairs
// ...
arithmeticExpression:
	  (LeftParenthesisSeparator arithmeticExpression RightParenthesisSeparator)
	| (PlusOperator | MinusOperator) arithmeticExpression
	|<assoc=right> arithmeticExpression PowerOperator arithmeticExpression
	|  arithmeticExpression (MultiplyOperator | DivideOperator) arithmeticExpression
	|  arithmeticExpression (PlusOperator | MinusOperator) arithmeticExpression
	|  identifier
	|  numericLiteral;





// p68: Identifiers
// When used in a syntax diagram in this information, the term identifier refers to a
// valid combination of a data-name or function-identifier with its qualifiers,
// subscripts, and reference-modifiers as required for uniqueness of reference.
// Rules for identifiers associated with a format can however specifically prohibit
// qualification, subscripting, or reference modification.
// The term data-name refers to a name that must not be qualified, subscripted, or
// reference modified unless specifically permitted by the rules for the format.
// - For a description of qualification, see �Qualification� on page 65.
// - For a description of subscripting, see �Subscripting� on page 71.
// - For a description of reference modification, see �Reference modification� on
//   page 74.
// p69: Duplication of data-names must not occur in those places where the data-names
// cannot be made unique by qualification.
// In the same program, the data-name specified as the subject of the entry whose
// level-number is 01 that includes the EXTERNAL clause must not be the same
// data-name specified for any other data description entry that includes the
// EXTERNAL clause.
// In the same DATA DIVISION, the data description entries for any two data items
// for which the same data-name is specified must not include the GLOBAL clause.
// DATA DIVISION names that are explicitly referenced must either be uniquely
// defined or made unique through qualification. Unreferenced data items need not
// be uniquely defined. The highest level in a data hierarchy (a data item associated
// with a level indicator (FD or SD in the FILE SECTION) or with level-number 01)
// must be uniquely named if referenced. Data items associated with level-numbers
// 02 through 49 are successively lower levels of the hierarchy.

identifier:
	  dataNameReferenceOrSpecialRegisterOrFunctionIdentifier (LeftParenthesisSeparator referenceModifier RightParenthesisSeparator)?
	// | conditionNameReference // cannot be distinguished from dataNameReference at this stage
          ;

// p74: Reference modification
// Reference modification defines a data item by specifying a leftmost character and
// optional length for the data item.
// data-name-1
// Must reference a data item described explicitly or implicitly with usage
// DISPLAY, DISPLAY-1, or NATIONAL. A national group item is processed
// as an elementary data item of category national.
// data-name-1 can be qualified or subscripted.
// function-name-1
// Must reference an alphanumeric or national function.
// leftmost-character-position
// Must be an arithmetic expression. The evaluation of leftmost-characterposition
// must result in a positive nonzero integer that is less than or equal
// to the number of characters in the data item referenced by data-name-1.
// length
// Must be an arithmetic expression.
// The evaluation of length must result in a positive nonzero integer.
// The sum of leftmost-character-position and length minus the value 1 must be
// less than or equal to the number of character positions in data-name-1. If
// length is omitted, the length used will be equal to the number of character
// positions in data-name-1 plus 1, minus leftmost-character-position.
// For usages DISPLAY-1 and NATIONAL, each character position occupies 2 bytes.
// Reference modification operates on whole character positions and not on the
// individual bytes of the characters in usages DISPLAY-1 and NATIONAL. For usage
// DISPLAY, reference modification operates as though each character were a
// single-byte character.
// Unless otherwise specified, reference modification is allowed anywhere an
// identifier or function-identifier that references a data item or function with the
// same usage as the reference-modified data item is permitted.
// Each character position referenced by data-name-1 or function-name-1 is assigned an
// ordinal number incrementing by one from the leftmost position to the rightmost
// position. The leftmost position is assigned the ordinal number one. If the data
// description entry for data-name-1 contains a SIGN IS SEPARATE clause, the sign
// position is assigned an ordinal number within that data item.
// If data-name-1 is described with usage DISPLAY and category numeric,
// numeric-edited, alphabetic, alphanumeric-edited, or external floating-point,
// data-name-1 is operated upon for purposes of reference modification as if it were
// redefined as a data item of category alphanumeric with the same size as the data
// item referenced by data-name-1.
// If data-name-1 is described with usage NATIONAL and category numeric,
// numeric-edited, national-edited, or external floating-point, data-name-1 is operated
// upon for purposes of reference modification as if it were redefined as a data item
// of category national with the same size as the data item referenced by data-name-1.
// If data-name-1 is a national group item, data-name-1 is processed as an elementary
// data item of category national.
// Reference modification creates a unique data item that is a subset of data-name-1 or
// a subset of the value referenced by function-name-1 and its arguments, if any. This
// unique data item is considered an elementary data item without the JUSTIFIED
// clause.
// When a function is reference-modified, the unique data item has class, category,
// and usage national if the type of the function is national; otherwise, it has class
// and category alphanumeric and usage display.
// When data-name-1 is reference-modified, the unique data item has the same class,
// category, and usage as that defined for the data item referenced by data-name-1
// except that:
// - If data-name-1 has category national-edited, the unique data item has category
//   national.
// - If data-name-1 has usage NATIONAL and category numeric-edited, numeric, or
//   external floating-point, the unique data item has category national.
// - If data-name-1 has usage DISPLAY, and category numeric-edited,
//   alphanumeric-edited, numeric, or external floating-point, the unique data item
//   has category alphanumeric.
// - If data-name-1 references an alphanumeric group item, the unique data item is
//   considered to have usage DISPLAY and category alphanumeric.
// - If data-name-1 references a national group item, the unique data item has usage
//   NATIONAL and category national.
// If length is not specified, the unique data item created extends from and includes
// the character position identified by leftmost-character-position up to and including
// the rightmost character position of the data item referenced by data-name-1.

// p75: Evaluation of operands
// Reference modification for an operand is evaluated as follows:
// - If subscripting is specified for the operand, the reference modification is
//   evaluated immediately after evaluation of the subscript.
// - If subscripting is not specified for the operand, the reference modification is
//   evaluated at the time subscripting would be evaluated if subscripts had been
//   specified.

referenceModifier: leftMostCharacterPosition ColonSeparator length?;
leftMostCharacterPosition: arithmeticExpression;
length: arithmeticExpression;

dataNameReferenceOrSpecialRegisterOrFunctionIdentifier:
	  dataNameReference
	| specialRegister
	| addressOfSpecialRegisterDecl
	| lengthOfSpecialRegisterDecl
	| linageCounterSpecialRegisterDecl
	| functionIdentifier;

// p77: A function-identifier is a sequence of character strings and separators that uniquely
// references the data item that results from the evaluation of a function.
// A function-identifier that makes reference to an alphanumeric or national function
// can be specified anywhere that a data item of category alphanumeric or category
// national, respectively, can be referenced and where references to functions are not
// specifically prohibited, except as follows:
// - As a receiving operand of any statement
// - Where a data item is required to have particular characteristics (such as class
//   and category, size, sign, and permissible values) and the evaluation of the
//   function according to its definition and the particular arguments specified would
//   not have these characteristics
// A function-identifier that makes reference to an integer or numeric function can be
// used wherever an arithmetic expression can be used.

// p77: A function-identifier that makes reference to an alphanumeric or national function
// can be specified anywhere that a data item of category alphanumeric or category
// national, respectively, can be referenced and where references to functions are not
// specifically prohibited.
// A function-identifier that makes reference to an integer or numeric function can be
// used wherever an arithmetic expression can be used.

// p447 : Intrinsic functions
// An intrinsic function is a function that performs a mathematical, character, or logical
// operation. You can use intrinsic functions to make reference to a data item whose
// value is derived automatically during execution.
// Data processing problems often require the use of values that are not directly
// accessible in the data storage associated with the object program, but instead must
// be derived through performing operations on other data. An intrinsic function is a
// function that performs a mathematical, character, or logical operation, and thereby
// allows you to make reference to a data item whose value is derived automatically
// during execution.
// The intrinsic functions can be grouped into six categories, based on the type of
// service performed:
// - Mathematical
// - Statistical
// - Date/time
// - Financial
// - Character-handling
// - General
// You can reference a function by specifying its name, along with any required
// arguments, in a PROCEDURE DIVISION statement.
// Functions are elementary data items, and return alphanumeric character, national
// character, numeric, or integer values. Functions cannot serve as receiving operands.

// p477: The general format of a function-identifier is:
// function-name-1
// function-name-1 must be one of the intrinsic function names.
// argument-1
// argument-1 must be an identifier, a literal (other than a figurative constant),
// or an arithmetic expression that satisfies the argument requirements for the
// specified function.
// reference-modifier
// Can be specified only for functions of type alphanumeric or national.
// A function-identifier can be specified wherever a data item of the type of the
// function is allowed. The argument to a function can be any function or an
// expression containing a function, including another evaluation of the same
// function, whose result meets the requirements for the argument.
// Within a PROCEDURE DIVISION statement, each function-identifier is evaluated
// at the same time as any reference modification or subscripting associated with an
// identifier in that same position would be evaluated.

// ... more detail on functions (types, usage rules, arguments ...) p478 to p484 ...

functionIdentifier: FUNCTION intrinsicFunctionName (LeftParenthesisSeparator argument+ RightParenthesisSeparator)?;
intrinsicFunctionName: (FunctionName | LENGTH | RANDOM | WHEN_COMPILED);

// p478: argument-1 must be an identifier, a literal (other than a figurative constant),
// or an arithmetic expression that satisfies the argument requirements for the
// specified function.
// p480: An argument must be one of the following items:
// - A data item identifier
// - An arithmetic expression
// - A function-identifier
// - A literal other than a figurative constant
// - A special-register

argument:
	  identifier // an identifier can be a special register or a functionIdentifier
	| literal
	| arithmeticExpression;

// NB : Because FunctionNames are not reserved words,
// and because the exact list of the instrinsic functions, their types and their arguments are more a library matter than a language matter,
// we do not try to check the validity of the number of arguments, the types of arguments alowed, and the referenceModifier pertinence
// at the grammar level.                      
// All these rules will be check at a later time by looking at an independent table of instrinsic functions.

// p484: Function definitions
// This section provides an overview of the argument type, function type, and value
// returned for each of the intrinsic functions.

// ... detailed description of each intrinsic function p484 -> p524 ...

//Function names
//	ACOS | ANNUITY | ASIN | ATAN |
//	CHAR | COS | CURRENT_DATE |
//	DATE_OF_INTEGER | DATE_TO_YYYYMMDD | DAY_OF_INTEGER | DAY_TO_YYYYDDD |
//	DISPLAY_OF | FACTORIAL |
//	INTEGER | INTEGER_OF_DATE | INTEGER_OF_DAY | INTEGER_PART |
//	LENGTH | LOG | LOG10 | LOWER_CASE |
//	MAX | MEAN | MEDIAN | MIDRANGE | MIN | MOD |
//	NATIONAL_OF | NUMVAL | NUMVAL_C |
//	ORD | ORD_MAX | ORD_MIN |
//	PRESENT_VALUE |
//	RANDOM | RANGE | REM | REVERSE |
//	SIN | SQRT | STANDARD_DEVIATION | SUM |
//	TAN |
//	ULENGTH | UPOS | UPPER_CASE | USUBSTR | USUPPLEMENTARY | UVALID | UWIDTH |
//	VARIANCE |
//	WHEN_COMPILED |
//	YEAR_TO_YYYY;


// --- DATA REFERENCES ---

// p16: Unless otherwise explicitly restricted, a special register can be used wherever a
// data-name or identifier that has the same definition as the implicit definition of the
// special register can be used.

addressOfSpecialRegisterDecl: ADDRESS OF dataNameReference;
lengthOfSpecialRegisterDecl:  LENGTH OF? dataNameReference;
linageCounterSpecialRegisterDecl: LINAGE_COUNTER OF fileName;

dataNameReference:
	qualifiedDataName (LeftParenthesisSeparator subscript+ RightParenthesisSeparator)?;

// p71: Subscripting
// Subscripting is a method of providing table references through the use of
// subscripts. A subscript is a positive integer whose value specifies the occurrence
// number of a table element.
// condition-name-1
// The conditional variable for condition-name-1 must contain an OCCURS
// clause or must be subordinate to a data description entry that contains an
// OCCURS clause.
// data-name-1
// Must contain an OCCURS clause or must be subordinate to a data
// description entry that contains an OCCURS clause.
// integer-1
// Can be signed. If signed, it must be positive.
// data-name-3
// Must be a numeric elementary item representing an integer.
// data-name-3 can be qualified.
// index-name-1
// Corresponds to a data description entry in the hierarchy of the table being
// referenced that contains an INDEXED BY phrase that specifies that name.
// integer-2 , integer-3
// Cannot be signed.
// The subscripts, enclosed in parentheses, are written immediately following any
// qualification for the name of the table element. The number of subscripts in such a
// reference must equal the number of dimensions in the table whose element is
// being referenced. That is, there must be a subscript for each OCCURS clause in the
// hierarchy that contains the data-name including the data-name itself.
// When more than one subscript is required, they are written in the order of
// successively less inclusive dimensions of the data organization. If a
// multidimensional table is thought of as a series of nested tables and the most
// inclusive or outermost table in the nest is considered to be the major table with the
// innermost or least inclusive table being the minor table, the subscripts are written
// from left to right in the order major, intermediate, and minor.
// For example, if TABLE-THREE is defined as:
// 01 TABLE-THREE.
// 05 ELEMENT-ONE OCCURS 3 TIMES.
// 10 ELEMENT-TWO OCCURS 3 TIMES.
// 15 ELEMENT-THREE OCCURS 2 TIMES PIC X(8).
// a valid subscripted reference to TABLE-THREE is:
// ELEMENT-THREE (2 2 1)
// Subscripted references can also be reference modified. See the third example under
// �Reference modification examples� on page 76. A reference to an item must not be
// subscripted unless the item is a table element or an item or condition-name
// associated with a table element.
// Each table element reference must be subscripted except when such reference
// appears:
// - In a USE FOR DEBUGGING statement
// - As the subject of a SEARCH statement
// - In a REDEFINES clause
// - In the KEY is phrase of an OCCURS clause
// The lowest permissible occurrence number represented by a subscript is 1. The
// highest permissible occurrence number in any particular case is the maximum
// number of occurrences of the item as specified in the OCCURS clause. 

// p73: Subscripting using data-names
// When a data-name is used to represent a subscript, it can be used to reference
// items within different tables. These tables need not have elements of the same size.
// The same data-name can appear as the only subscript with one item and as one of
// two or more subscripts with another item. A data-name subscript can be qualified;
// it cannot be subscripted or indexed. For example, valid subscripted references to
// TABLE-THREE, assuming that SUB1, SUB2, and SUB3 are all items subordinate to
// SUBSCRIPT-ITEM, include:
// ELEMENT-THREE (SUB1 SUB2 SUB3)
// ELEMENT-THREE IN TABLE-THREE (SUB1 OF SUBSCRIPT-ITEM,
// SUB2 OF SUBSCRIPT-ITEM, SUB3 OF SUBSCRIPT-ITEM)

// p73: Subscripting using index-names (indexing)
// Indexing allows such operations as table searching and manipulating specific
// items. To use indexing, you associate one or more index-names with an item
// whose data description entry contains an OCCURS clause.
// An index associated with an index-name acts as a subscript, and its value
// corresponds to an occurrence number for the item to which the index-name is
// associated.
// The INDEXED BY phrase, by which the index-name is identified and associated
// with its table, is an optional part of the OCCURS clause. There is no separate entry
// to describe the index associated with index-name. At run time, the contents of the
// index corresponds to an occurrence number for that specific dimension of the table
// with which the index is associated.
// The initial value of an index at run time is undefined, and the index must be
// initialized before it is used as a subscript. An initial value is assigned to an index
// with one of the following statements:
// - The PERFORM statement with the VARYING phrase
// - The SEARCH statement with the ALL phrase
// - The SET statement
// The use of an integer or data-name as a subscript that references a table element or
// an item within a table element does not cause the alteration of any index
// associated with that table.
// An index-name can be used to reference any table. However, the element length of
// the table being referenced and of the table that the index-name is associated with
// should match. Otherwise, the reference will not be to the same table element in
// each table, and you might get runtime errors.
// Data that is arranged in the form of a table is often searched. The SEARCH
// statement provides facilities for producing serial and nonserial searches. It is used
// to search for a table element that satisfies a specific condition and to adjust the
// value of the associated index to indicate that table element.
// To be valid during execution, an index value must correspond to a table element
// occurrence of neither less than one, nor greater than the highest permissible
// occurrence number.
// For more information about index-names, see �Index-name� on page 71 and
// �INDEXED BY phrase� on page 194.

// p74: Relative subscripting
// In relative subscripting, the name of a table element is followed by a subscript of the
// form data-name or index-name followed by the operator + or -, and a positive or
// unsigned integer literal.
// The operators + and - must be preceded and followed by a space. The value of the
// subscript used is the same as if the index-name or data-name had been set up or
// down by the value of the integer. The use of relative indexing does not cause the
// program to alter the value of the index.

subscript:
	  IntegerLiteral | ALL
	| (qualifiedDataName withRelativeSubscripting?)
//	| (indexName withRelativeSubscripting?) // cannot be distinguished from the previous line at the parsing stage
	;

withRelativeSubscripting: (PlusOperator | MinusOperator) IntegerLiteral;

// p65: Uniqueness of reference
// Every user-defined name in a COBOL program is assigned by the user to name a
// resource for solving a data processing problem. To use a resource, a statement in a
// COBOL program must contain a reference that uniquely identifies that resource.
// To ensure uniqueness of reference, a user-defined name can be qualified. A
// subscript is required for unique reference to a table element, except as specified in
// �Subscripting� on page 71. A data-name or function-name, any subscripts, and the
// specified reference-modifier uniquely reference a data item defined by reference
// modification.
// When the same name has been assigned in separate programs to two or more
// occurrences of a resource of a given type, and when qualification by itself does not
// allow the references in one of those programs to differentiate between the
// identically named resources, then certain conventions that limit the scope of names
// apply. The conventions ensure that the resource identified is that described in the
// program containing the reference. For more information about resolving
// program-names, see �Resolution of names� on page 62.
// Unless otherwise specified by the rules for a statement, any subscripts and
// reference modification are evaluated only once as the first step in executing that
// statement.

// p65: Qualification
// A name that exists within a hierarchy of names can be made unique by specifying
// one or more higher-level names in the hierarchy. The higher-level names are called
// qualifiers, and the process by which such names are made unique is called
// qualification.
// Qualification is specified by placing one or more phrases after a user-specified
// name, with each phrase made up of the word IN or OF followed by a qualifier. (IN
// and OF are logically equivalent.)
// In any hierarchy, the data-name associated with the highest level must be unique if
// it is referenced, and cannot be qualified.
// You must specify enough qualification to make the name unique; however, it is not
// always necessary to specify all the levels of the hierarchy. For example, if there is
// more than one file whose records contain the field EMPLOYEE-NO, but only one of the
// files has a record named MASTER-RECORD:
// - EMPLOYEE-NO OF MASTER-RECORD sufficiently qualifies EMPLOYEE-NO.
// - EMPLOYEE-NO OF MASTER-RECORD OF MASTER-FILE is valid but unnecessary.
// Qualification rules
// The rules for qualifying a name are:
// - A name can be qualified even though it does not need qualification except in a
//   REDEFINES clause, in which case it must not be qualified.
// - Each qualifier must be of a higher level than the name it qualifies and must be
//   within the same hierarchy.
// - If there is more than one combination of qualifiers that ensures uniqueness, any
//   of those combinations can be used.
// Identical names
// When programs are directly or indirectly contained within other programs, each
// program can use identical user-defined words to name resources.
// A program references the resources that program describes rather than the
// same-named resources described in another program, even if the names are
// different types of user-defined words.
// These same rules apply to classes and their contained methods.

// p67: References to DATA DIVISION names
// This section discusses the following types of references.
// - �Simple data reference�
// - �Identifiers� on page 68
// Simple data reference
// The most basic method of referencing data items in a COBOL program is simple
// data reference, which is data-name-1 without qualification, subscripting, or reference
// modification. Simple data reference is used to reference a single elementary or
// group item.
// data-name-1
// Can be any data description entry.
// data-name-1 must be unique in a program.

qualifiedDataName: dataNameBase ((IN | OF) dataName)* ((IN | OF) fileName)?;

dataNameBase: dataName;

// p70: Condition-name
// condition-name-1
// Can be referenced by statements and entries either in the program that
// contains the definition of condition-name-1, or in a program contained
// within that program.
// If explicitly referenced, a condition-name must be unique or be made
// unique through qualification or subscripting (or both) except when the
// scope of names by itself ensures uniqueness of reference.
// If qualification is used to make a condition-name unique, the associated
// conditional variable can be used as the first qualifier. If qualification is
// used, the hierarchy of names associated with the conditional variable itself
// must be used to make the condition-name unique.
// If references to a conditional variable require subscripting, reference to any
// of its condition-names also requires the same combination of subscripting.
// In this information, condition-name refers to a condition-name qualified or
// subscripted, as necessary.
// data-name-1
// Can be a record-name.
// file-name-1
// Must be identified by an FD or SD entry in the DATA DIVISION.
// file-name-1 must be unique within this program.
// mnemonic-name-1
// For information about acceptable values for mnemonic-name-1, see
// �SPECIAL-NAMES paragraph� on page 112.

// p70: Format 1: condition-name in data division
conditionNameReference: qualifiedConditionName (LeftParenthesisSeparator subscript+ RightParenthesisSeparator)?;

// p70: Format 2: condition-name in SPECIAL-NAMES paragraph
//conditionNameReferenceInSpecialNamesParagraph: conditionName ((IN | OF) mnemonicForUPSISwitchName)*;


qualifiedConditionName: conditionName ((IN | OF) dataName)* ((IN | OF) fileName)?;





// --- Terminals ---
//
//literal: alphanumOrNationalLiteral | numericLiteral;
//
//alphanumOrNationalLiteral:
//	  alphanumOrNationalLiteralBase
//	| NullTerminatedAlphanumericLiteral
//	| (ALL alphanumOrNationalLiteralBase);
//
//alphanumOrNationalLiteralBase:
//	( AlphanumericLiteral
//	| HexadecimalAlphanumericLiteral
//	| NationalLiteral
//	| HexadecimalNationalLiteral
//	| DBCSLiteral )
//	| figurativeConstant;
//
//figurativeConstant:
//	( HIGH_VALUE | HIGH_VALUES
//	| LOW_VALUE  | LOW_VALUES
//	| NULL  | NULLS
//	| QUOTE | QUOTES
//	| SPACE | SPACES
//	| ZERO  | ZEROS  | ZEROES
//	| SymbolicCharacter);
//
//numericLiteral: (IntegerLiteral | DecimalLiteral | FloatingPointLiteral | ZERO | ZEROS | ZEROES);
//
//specialRegister:
//	( DEBUG_CONTENTS | DEBUG_ITEM | DEBUG_LINE | DEBUG_NAME | DEBUG_SUB_1 | DEBUG_SUB_2 | DEBUG_SUB_3
//	| SORT_CONTROL | SORT_CORE_SIZE | SORT_FILE_SIZE | SORT_MESSAGE | SORT_MODE_SIZE | SORT_RETURN
//	| XML_CODE | XML_EVENT | XML_INFORMATION | XML_NAMESPACE | XML_NAMESPACE_PREFIX | XML_NNAMESPACE | XML_NNAMESPACE_PREFIX | XML_NTEXT | XML_TEXT
//	| JNIENVPTR | LINAGE_COUNTER | RETURN_CODE | SHIFT_IN | SHIFT_OUT | TALLY | WHEN_COMPILED );



	  ////////////////////////
	 // User defined words //
	////////////////////////

// p118 : CLASS class-name-1 IS
// Provides a means for relating a name to the specified set of characters
// listed in that clause. class-name-1 can be referenced only in a class
// condition. The characters specified by the values of the literals in this
// clause define the exclusive set of characters of which this class consists.
// The class-name in the CLASS clause can be a DBCS user-defined word.

charsetClassName: UserDefinedWord;

// p115 : condition-1, condition-2
// Condition-names follow the rules for user-defined names. At least one
// character must be alphabetic. The value associated with the
// condition-name is considered to be alphanumeric. A condition-name can be
// associated with the on status or off status of each UPSI switch specified.
// In the PROCEDURE DIVISION, the UPSI switch status is tested through
// the associated condition-name. Each condition-name is the equivalent of a
// level-88 item; the associated mnemonic-name, if specified, is considered the
// conditional variable and can be used for qualification.
// Condition-names specified in the SPECIAL-NAMES paragraph of a
// containing program can be referenced in any contained program

conditionName: UserDefinedWord;

dataName: UserDefinedWord;

// p130: file-name-1
// Must be identified by an FD or SD entry in the DATA DIVISION.
// A file-name must conform to the rules for a COBOL user-defined name, must contain at least one alphabetic character, and must be unique within this program.

fileName: UserDefinedWord;

// p194: index-name-1
// Each index-name specifies an index to be created by the compiler for use
// by the program. These index-names are not data-names and are not
// identified elsewhere in the COBOL program; instead, they can be regarded
// as private special registers for the use of this object program only. They are
// not data and are not part of any data hierarchy.
// Unreferenced index names need not be uniquely defined.
// In one table entry, up to 12 index-names can be specified.
// If a data item that possesses the global attribute includes a table accessed
// with an index, that index also possesses the global attribute. Therefore, the
// scope of an index-name is the same as that of the data-name that names
// the table in which the index is defined.
// Indexes specified in an external data record do not possess the external attribute.

// p71: Index-name
// An index-name identifies an index. An index can be regarded as a private special
// register that the compiler generates for working with a table. You name an index
// by specifying the INDEXED BY phrase in the OCCURS clause that defines a table.
// You can use an index-name in only the following language elements:
// - SET statements
// - PERFORM statements
// - SEARCH statements
// - Subscripts
// - Relation conditions
// An index-name is not the same as the name of an index data item, and an
// index-name cannot be used like a data-name.

indexName: UserDefinedWord;

mnemonicForUPSISwitchName : UserDefinedWord;
