// IBM Enterprise Cobol 5.1 for zOS

// -----------------------------------------------------------------------
// Grammar for the FIRST step of the Cobol parser : flat view of the Cobol
// syntax, useful for incremental parsing. The goal here is to produce a 
// LIST of syntax nodes instead of TREE (because it is easier to reuse).
// -----------------------------------------------------------------------

grammar CobolCodeElements;

import CobolExpressions;

// --- Starting parser rule for PHASE 1 of parsing ---

cobolCodeElements:
                     codeElement* EOF;

// --- List of Cobol Code Elements --
// (see namespace :  TypeCobol.Compiler.CodeElements)

codeElement:

    // -- Cobol source program --
    programIdentification |
        environmentDivisionHeader |
            configurationSectionHeader |
                sourceComputerParagraph |
                objectComputerParagraph |
                specialNamesParagraph |
                repositoryParagraph |
            inputOutputSectionHeader |
                fileControlParagraphHeader |
                    fileControlEntry |
                ioControlParagraphHeader |
                    ioControlEntry |
        dataDivisionHeader |
            fileSectionHeader |
                fileDescriptionEntry |
                dataDescriptionEntry |
            workingStorageSectionHeader |
                // dataDescriptionEntry
            localStorageSectionHeader |
                // dataDescriptionEntry
            linkageSectionHeader |
                // dataDescriptionEntry
        procedureDivisionHeader |
        declarativesHeader |
            sectionHeader | useStatement |
                paragraphHeader |
                    // ... statements ...
                    sentenceEnd |
        declarativesEnd |
            // sectionHeader
                // paragraphName
                    // statement
                    // sentenceEnd
        // ... nested source program ...
    programEnd |              
		         
    // -- Cobol class definition --
    classIdentification |
        // environmentDivisionHeader |
            // configurationSectionHeader
                // sourceComputerParagraph
                // objectComputerParagraph
                // specialNamesParagraph
                // repositoryParagraph
        factoryIdentification |
            // dataDivisionHeader 
                // workingStorageSectionHeader 
                    // dataDescriptionEntry
            // procedureDivisionHeader |
                methodIdentification |
                    // environmentDivisionHeader 
                        // inputOutputSectionHeader
                            // fileControlParagraph
                            // ioControlParagraph
                    // dataDivisionHeader 
                        // fileSectionHeader
                            // fileDescriptionEntry
                            // dataDescriptionEntry
                        // workingStorageSectionHeader 
                            // dataDescriptionEntry
                        // localStorageSectionHeader 
                            // dataDescriptionEntry
                        // linkageSectionHeader 
                            // dataDescriptionEntry
                    // procedureDivisionHeader
                        // ... procedure division content ...
                methodEnd |
        factoryEnd |
		objectIdentification |
			//dataDivisionHeader 
				// workingStorageSectionHeader 
					// dataDescriptionEntry
			// procedureDivisionHeader 
				// ... method definitions ...
		objectEnd |
	classEnd |

	// -- Procedure division Statements --

	// --- Decision statements ---
	evaluateStatement |
		whenSearchCondition |
		whenCondition |
		whenOtherCondition |
	evaluateStatementEnd |
	ifStatement |
		// ... statements ...
		nextSentenceStatement |
	elseCondition |
		// ... statements ...
	ifStatementEnd |

	// --- Control flow statements ---
	continueStatement |
    entryStatement |
    execStatement |
    exitMethodStatement |
    exitProgramStatement |	
	gobackStatement |
	stopStatement |

	// --- Procedure branching statements ---
	alterStatement |
	exitStatement |
	gotoStatement |
	performStatement |
	performStatementEnd |

	// --- Program or method linkage statements ---
	callStatement |
	callStatementEnd |
	cancelStatement |
	invokeStatement |
	invokeStatementEnd |

	// --- Arithmetic statements ---
	addStatement |
	addStatementEnd |
	computeStatement |
	computeStatementEnd |
	divideStatement |
	divideStatementEnd |
	multiplyStatement |
	multiplyStatementEnd |
	subtractStatement |
	subtractStatementEnd |

	// --- Data manipulation statements ---
	initializeStatement |
	inspectStatement |
	moveStatement |
	setStatement |
	stringStatement |
	stringStatementEnd |
	unstringStatement |
	unstringStatementEnd |
	xmlGenerateStatement |
	xmlParseStatement |
	xmlStatementEnd |

	// --- Table handling statements ---
	searchStatement |
		// atEndCondition ... imperative statements ...
		// whenSearchCondition ...
	searchStatementEnd |

	// --- I/O statements ---
	acceptStatement |
	closeStatement |
	deleteStatement |
	deleteStatementEnd |
	displayStatement |
	openStatement |
	readStatement |
	readStatementEnd |
	rewriteStatement |
	rewriteStatementEnd |
	startStatement |
	startStatementEnd |
	writeStatement |
	writeStatementEnd |

	// --- Ordering statements ---
	mergeStatement |
	releaseStatement |
	returnStatement |
	returnStatementEnd |
	sortStatement |
	
	// --- Conditions for conditional statements  ---
	atEndCondition |
	notAtEndCondition |
	atEndOfPageCondition |
	notAtEndOfPageCondition |
	invalidKeyCondition |
	notInvalidKeyCondition |
	onExceptionCondition |
	notOnExceptionCondition |
	onOverflowCondition |
	notOnOverflowCondition |
	onSizeErrorCondition |
	notOnSizeErrorCondition

//	 [TYPECOBOL]
	| tcCodeElement;

// what is here is not important as long as it is not epsilon
tcCodeElement: PUBLIC | PRIVATE | UNSAFE;
//	[/TYPECOBOL]


// --- Individual code elements syntax ---

// p97 : Program IDENTIFICATION DIVISION
// For a program, the first paragraph of the IDENTIFICATION DIVISION
// must be the PROGRAM-ID paragraph. The other paragraphs are optional
// and can appear in any order.

// p100 : The PROGRAM-ID paragraph specifies the name by which the program is known
// and assigns selected program attributes to that program. It is required and must be
// the first paragraph in the IDENTIFICATION DIVISION.

// p 101 : RECURSIVE
// An optional clause that allows COBOL programs to be recursively
// reentered.
// You can specify the RECURSIVE clause only on the outermost program of
// a compilation unit. Recursive programs cannot contain nested
// subprograms.
// If the RECURSIVE clause is specified, program-name can be recursively
// reentered while a previous invocation is still active. If the RECURSIVE
// clause is not specified, an active program cannot be recursively reentered.
// The WORKING-STORAGE SECTION of a recursive program defines
// storage that is statically allocated and initialized on the first entry to a
// program and is available in a last-used state to any of the recursive
// invocations.
// The LOCAL-STORAGE SECTION of a recursive program (as well as a
// nonrecursive program) defines storage that is automatically allocated,
// initialized, and deallocated on a per-invocation basis.
// Internal file connectors that correspond to an FD in the FILE SECTION of a
// recursive program are statically allocated. The status of internal file
// connectors is part of the last-used state of a program that persists across
// invocations.
// The following language elements are not supported in a recursive
// program:
// - ALTER
// - GO TO without a specified procedure-name
// - RERUN
// - SEGMENT-LIMIT
// - USE FOR DEBUGGING
// The RECURSIVE clause is required for programs compiled with the
// THREAD option.

// p102 : INITIAL
// Specifies that when program-name is called, program-name and any programs
// contained (nested) within it are placed in their initial state. The initial
// attribute is not supported for programs compiled with the THREAD
// option.
// A program is in the initial state:
// - The first time the program is called in a run unit
// - Every time the program is called, if it possesses the initial attribute
// - The first time the program is called after the execution of a CANCEL
//   statement that references the program or a CANCEL statement that
//   references a program that directly or indirectly contains the program
// - The first time the program is called after the execution of a CALL
//   statement that references a program that possesses the initial attribute
//   and that directly or indirectly contains the program
// When a program is in the initial state, the following occur:
// - The program's internal data contained in the WORKING-STORAGE
//   SECTION is initialized. If a VALUE clause is used in the description of
//   the data item, the data item is initialized to the defined value. If a
//   VALUE clause is not associated with a data item, the initial value of the
//   data item is undefined.
// - Files with internal file connectors associated with the program are not in
//   the open mode.
// - The control mechanisms for all PERFORM statements contained in the
//   program are set to their initial states.
// - An altered GO TO statement contained in the program is set to its initial
//   state.

// p85 : Nested programs can be directly or indirectly contained in the containing program.
//       Nested programs are not supported for programs compiled with the THREAD option.

// p101 : You can specify the RECURSIVE clause only on the outermost program of
// a compilation unit. Recursive programs cannot contain nested
// subprograms.

// p102 : COMMON
// Specifies that the program named by program-name is contained (that is,
// nested) within another program and can be called from siblings of the
// common program and programs contained within them. The COMMON
// clause can be used only in nested programs.

programIdentification:
	(IDENTIFICATION | ID) DIVISION PeriodSeparator 
	PROGRAM_ID PeriodSeparator? programNameDefinition
	(IS? (RECURSIVE | INITIAL | (COMMON INITIAL?) | (INITIAL COMMON?)) PROGRAM?)? PeriodSeparator?
	authoringProperties;
                       
// p83 : The end of a COBOL source program is indicated by the END PROGRAM marker.
//       If there are no nested programs, the absence of additional source program lines
//       also indicates the end of a COBOL program.

// p85 : An end program marker separates each program in the sequence of programs. 
//       program-name must be identical to a program-name declared in a preceding program-ID paragraph.

// p85 : An end program marker is optional for the last program in the sequence only if that program does not contain any nested source programs.

programEnd:
	END PROGRAM programNameReference2 PeriodSeparator;
			  
// p97 : Class IDENTIFICATION DIVISION
// For a class, the first paragraph of the IDENTIFICATION DIVISION must
// be the CLASS-ID paragraph. The other paragraphs are optional and can
// appear in any order.

// p103 : The CLASS-ID paragraph specifies the name by which the class is known and
// assigns selected attributes to that class. The CLASS-ID paragraph is required and
// must be the first paragraph in a class IDENTIFICATION DIVISION.

// p103 : INHERITS
// A clause that defines class-name-1 to be a subclass (or derived class) of
// class-name-2 (the parent class). class-name-1 cannot directly or indirectly
// inherit from class-name-1.
// class-name-2
// The name of a class inherited by class-name-1. You must specify class-name-2
// in the REPOSITORY paragraph of the configuration section of the class
// definition.

// p103 : Inheritance
// Every method available on instances of a class is also available on instances of any
// subclass directly or indirectly derived from that class.
// A subclass can introduce new methods that do not exist in the parent or ancestor
// class and can override a method from the parent or ancestor class. When a
// subclass overrides an existing method, it defines a new implementation for that
// method, which replaces the inherited implementation.
// The instance data of class-name-1 is the instance data declared in class-name-2
// together with the data declared in the WORKING-STORAGE SECTION of
// class-name-1. Note, however, that instance data is always private to the class that
// introduces it.
// The semantics of inheritance are as defined by Java. All classes must be derived
// directly or directly from the java.lang.Object class.
// Java supports single inheritance; that is, no class can inherit directly from more
// than one parent. Only one class-name can be specified in the INHERITS phrase of
// a class definition.

classIdentification:
	(IDENTIFICATION | ID) DIVISION PeriodSeparator 
	CLASS_ID PeriodSeparator classNameDefinition INHERITS inheritsFromClassName=classNameReference PeriodSeparator
	authoringProperties;

// p91 : The end of a COBOL class definition is indicated by the END CLASS marker.

classEnd:
	END CLASS classNameReference PeriodSeparator;

// p97 : Factory IDENTIFICATION DIVISION
// A factory IDENTIFICATION DIVISION contains only a factory paragraph
// header.

// p103 : The factory IDENTIFICATION DIVISION introduces the factory definition, which
// is the portion of a class definition that defines the factory object of the class.
// A factory object is the single common object that is shared by all object instances of
// the class. The factory definition contains factory data and factory methods.

factoryIdentification:
	(IDENTIFICATION | ID) DIVISION PeriodSeparator 
	FACTORY PeriodSeparator;

factoryEnd:
	END FACTORY PeriodSeparator;

// p97 : Object IDENTIFICATION DIVISION
// An object IDENTIFICATION DIVISION contains only an object paragraph
// header.

// p103 : The object IDENTIFICATION DIVISION introduces the object definition, which is
// the portion of a class definition that defines the instance objects of the class.
// The object definition contains object data and object methods.

objectIdentification:
	(IDENTIFICATION | ID) DIVISION PeriodSeparator 
	OBJECT PeriodSeparator;

objectEnd:
	END OBJECT PeriodSeparator;

// p93 : A COBOL method definition describes a method. 
//       You can specify method definitions only within the factory paragraph and the object paragraph of a class definition.

// p97 : Method IDENTIFICATION DIVISION
// For a method, the first paragraph of the IDENTIFICATION DIVISION
// must be the METHOD-ID paragraph. The other paragraphs are optional
// and can appear in any order.

// p104 : The METHOD-ID paragraph specifies the name by which a method is known and
// assigns selected attributes to that method. The METHOD-ID paragraph is required
// and must be the first paragraph in a method identification division.

// p94 : Methods defined in an object definition are instance methods. An instance method in
//      a given class can access:
// - Data defined in the DATA DIVISION of the object paragraph of that class
//   (instance data)
// - Data defined in the DATA DIVISION of that instance method (method data)
// An instance method cannot directly access instance data defined in a parent class,
// factory data defined in its own class, or method data defined in another method of
// its class. It must invoke a method to access such data.

// p94 : Methods defined in a factory definition are factory methods. A factory method in a
//      given class can access:
// - Data defined in the DATA DIVISION of the factory paragraph of that class
//   (factory data)
// - Data defined in the DATA DIVISION of that factory method (method data)
// A factory method cannot directly access factory data defined in a parent class,
// instance data defined in its own class, or method data defined in another method
// of its class. It must invoke a method to access such data.

// p94 : Methods can be invoked from COBOL programs and methods, and they can be
// invoked from Java programs. A method can execute an INVOKE statement that
// directly or indirectly invokes itself. Therefore, COBOL methods are implicitly
// recursive (unlike COBOL programs, which support recursion only if the
// RECURSIVE attribute is specified in the program-ID paragraph.)

methodIdentification:
	(IDENTIFICATION | ID) DIVISION PeriodSeparator 
	METHOD_ID PeriodSeparator? methodNameDefinition PeriodSeparator?
	authoringProperties;

// p93 : The end of a COBOL method definition is indicated by the END METHOD marker.

methodEnd:
    END METHOD methodNameReference PeriodSeparator;

// p104 : Method signature
// The signature of a method consists of the name of the method and the number and
// types of the formal parameters to the method as specified in the PROCEDURE
// DIVISION USING phrase.

// p104 : Method overloading, overriding, and hiding
// COBOL methods can be overloaded, overridden, or hidden, based on the rules of the
// Java language.
// * Method overloading
// Method names that are defined for a class are not required to be unique.
// (The set of methods defined for a class includes the methods introduced by
// the class definition and the methods inherited from parent classes.)
// Method names defined for a class must have unique signatures. Two
// methods defined for a class and that have the same name but different
// signatures are said to be overloaded.
// The type of the method return value, if any, is not included in the method
// signature.
// A class must not define two methods with the same signature but different
// return value types, or with the same signature but where one method
// specifies a return value and the other does not.
// The rules for overloaded method definitions and resolution of overloaded
// method invocations are based on the corresponding rules for Java.
// * Method overriding (for instance methods)
// An instance method in a subclass overrides an instance method with the
// same name that is inherited from a parent class if the two methods have
// the same signature.
// When a method overrides an instance method defined in a parent class,
// the presence or absence of a method return value (the PROCEDURE
// DIVISION RETURNING data-name) must be consistent in the two
// methods. Further, when method return values are specified, the return
// values in the overridden method and the overriding method must have
// identical data types.
// An instance method must not override a factory method in a COBOL
// parent class, or a static method in a Java parent class.
// * Method hiding (for factory methods)
// A factory method is said to hide any and all methods with the same
// signature in the superclasses of the method definition that would otherwise
// be accessible. A factory method must not hide an instance method.

// --- Authoring properties common to all identification divisions ---

// p105 : Some optional paragraphs in the IDENTIFICATION DIVISION can be omitted.
// The optional paragraphs are:
// AUTHOR
// Name of the author of the program.
// INSTALLATION
// Name of the company or location.
// DATE-WRITTEN
// Date the program was written.
// DATE-COMPILED
// The DATE-COMPILED paragraph provides the compilation date in the
// source listing. If a comment-entry is specified, the entire entry is replaced
// with the current date, even if the entry spans lines. If the comment entry is
// omitted, the compiler adds the current date to the line on which
// DATE-COMPILED is printed. For example:
// DATE-COMPILED. 06/30/10.
// SECURITY
// Level of confidentiality of the program.

// p98 : Format: program identification division
// p99 : Format: class identification division 
// p100 : Format: method identification division
// !! p117 : The other paragraphs are optional and can appear in any order.

authoringProperties:
	(authorParagraph       |
	 installationParagraph |
	 dateWrittenParagraph  |
	 dateCompiledParagraph |
	 securityParagraph     )*;

authorParagraph:
    AUTHOR PeriodSeparator? alphanumericValue6*;

installationParagraph:
    INSTALLATION PeriodSeparator? alphanumericValue6*;

dateWrittenParagraph:
    DATE_WRITTEN PeriodSeparator? alphanumericValue6*;

dateCompiledParagraph:
    DATE_COMPILED PeriodSeparator? alphanumericValue6*;

securityParagraph:
     SECURITY PeriodSeparator? alphanumericValue6*;

// p105 : The comment-entry in any of the optional paragraphs can be any combination of
// characters from the character set of the computer. The comment-entry is written in
// Area B on one or more lines.
// Comment-entries serve only as documentation; they do not affect the meaning of
// the program. A hyphen in the indicator area (column 7) is not permitted in
// comment-entries.
// You can include DBCS character strings as comment-entries in the
// IDENTIFICATION DIVISION of your program. Multiple lines are allowed in a
// comment-entry that contains DBCS character strings.
// A DBCS character string must be preceded by a shift-out control character and
// followed by a shift-in control character. For example:
// AUTHOR. <.A.U.T.H.O.R.-.N.A.M.E>, XYZ CORPORATION
// DATE-WRITTEN. <.D.A.T.E>
// When a comment-entry that is contained on multiple lines uses DBCS characters,
// shift-out and shift-in characters must be paired on a line.

// ==> Lexer token type : CommentEntry

// ---

// p107 :  Environment division

environmentDivisionHeader:
    ENVIRONMENT DIVISION PeriodSeparator;

// p109 : The configuration section is an optional section for programs and classes, and can
// describe the computer environment on which the program or class is compiled and
// executed.
// The configuration section can:
// - Relate IBM-defined environment-names to user-defined mnemonic names
// - Specify the collating sequence
// - Specify a currency sign value, and the currency symbol used in the PICTURE
//   clause to represent the currency sign value
// - Exchange the functions of the comma and the period in PICTURE clauses and
//   numeric literals
// - Relate alphabet-names to character sets or collating sequences
// - Specify symbolic characters
// - Relate class-names to sets of characters
// - Relate object-oriented class names to external class-names and identify
//   class-names that can be used in a class definition or program
// - Relate xml-schema-names to ddnames or environment variable names
//   identifying files containing XML schemas

// p109 : Program configuration section
// The configuration section can be specified only in the ENVIRONMENT
// DIVISION of the outermost program of a COBOL source program.
// You should not specify the configuration section in a program that is
// contained within another program. The entries specified in the
// configuration section of a program apply to any program contained within
// that program.

// p109 : Class configuration section
// Specify the configuration section in the ENVIRONMENT DIVISION of a
// class definition. The repository paragraph can be specified in the
// ENVIRONMENT DIVISION of a class definition.
// Entries in a class configuration section apply to the entire class definition,
// including all methods introduced by that class.

// p109 : Method configuration section
// The input-output section can be specified in a method configuration
// section. The entries apply only to the method in which the configuration
// section is specified.

configurationSectionHeader:
    CONFIGURATION SECTION PeriodSeparator;

// p110 : The SOURCE-COMPUTER paragraph describes the computer on which the source
// text is to be compiled.

// p110 : WITH DEBUGGING MODE
// Activates a compile-time switch for debugging lines written in the source
// text.
// A debugging line is a statement that is compiled only when the
// compile-time switch is activated. Debugging lines allow you, for example,
// to check the value of a data-name at certain points in a procedure.
// To specify a debugging line in your program, code a D in column 7
// (indicator area). You can include successive debugging lines, but each must
// have a D in column 7, and you cannot break character strings across lines.
// All your debugging lines must be written so that the program is
// syntactically correct, whether the debugging lines are compiled or treated
// as comments.
// The presence or absence of the DEBUGGING MODE clause is logically
// determined after all COPY and REPLACE statements have been processed.
// You can code debugging lines in the ENVIRONMENT DIVISION (after the
// OBJECT-COMPUTER paragraph), and in the data and procedure divisions.
// If a debugging line contains only spaces in Area A and in Area B, the
// debugging line is treated the same as a blank line.

// p110 : All of the SOURCE-COMPUTER paragraph is syntax checked, but only the WITH
// DEBUGGING MODE clause has an effect on the execution of the program.

// p110 : computer-name
// A system-name. For example:
// IBM-system

// p110 : The SOURCE-COMPUTER Format

sourceComputerParagraph: 
    SOURCE_COMPUTER PeriodSeparator
    (computerName=alphanumericValue4 
     (WITH? DEBUGGING MODE)? 
     PeriodSeparator)?;

// p110 : The OBJECT-COMPUTER paragraph specifies the system for which the object
// program is designated.

// p111 : MEMORY SIZE integer
// integer specifies the amount of main storage needed to run the object
// program, in words, characters or modules. The MEMORY SIZE clause is
// syntax checked but has no effect on the execution of the program.

// p111 : PROGRAM COLLATING SEQUENCE IS alphabet-name
// The collating sequence used in this program is the collating sequence
// associated with the specified alphabet-name.
// The collating sequence pertains to this program and to any programs that
// this program might contain.
// PROGRAM COLLATING SEQUENCE determines the truth value of the
// following alphanumeric comparisons:
// - Those explicitly specified in relation conditions
// - Those explicitly specified in condition-name conditions
// The PROGRAM COLLATING SEQUENCE clause also applies to any
// merge or sort keys described with usage DISPLAY, unless the COLLATING
// SEQUENCE phrase is specified in the MERGE or SORT statement.
// The PROGRAM COLLATING SEQUENCE clause does not apply to DBCS
// data items or data items of usage NATIONAL.
// If the PROGRAM COLLATING SEQUENCE clause is omitted, the EBCDIC
// collating sequence is used. (See Appendix C, “EBCDIC and ASCII collating
// sequences,” on page 569.)

// p111 : SEGMENT-LIMIT IS
// The SEGMENT-LIMIT clause is syntax checked but has no effect on the
// execution of the program.
// All sections with priority-numbers 0
// through 49 are fixed permanent segments. See “Procedures” on page 252
// for a description of priority-numbers and segmentation support.
// Segmentation is not supported for programs compiled with the THREAD
// option.

// p112 : All of the OBJECT-COMPUTER paragraph is syntax checked, but only the
// PROGRAM COLLATING SEQUENCE clause has an effect on the execution of the
// program.

// p111 : OBJECT-COMPUTER Format

objectComputerParagraph:
    OBJECT_COMPUTER PeriodSeparator
    (computerName=alphanumericValue4 
     memorySizeClause?
     programCollatingSequenceClause?
     segmentLimitClause?
     PeriodSeparator)?;

memorySizeClause:
    MEMORY SIZE? integerValue (WORDS | CHARACTERS | MODULES);

programCollatingSequenceClause:
    PROGRAM? COLLATING? SEQUENCE IS? alphabetName;

segmentLimitClause:
    SEGMENT_LIMIT IS? priorityNumber;

// p111 : priority-number
// An integer ranging from 1 through 49. 

// p252: Priority-number
// An integer or a positive signed numeric literal ranging in value
// from 0 through 99. Priority-number identifies a fixed segment or an
// independent segment that is to contain the section.
// Sections in the declaratives portion must contain priority numbers in the
// range of 0 through 49.
// You cannot specify priority-numbers:
// - In a method definition
// - In a program that is declared with the RECURSIVE attribute
// - In a program compiled with the THREAD compiler option

priorityNumber: integerValue;

// p112 : The SPECIAL-NAMES paragraph is the name of an ENVIRONMENT DIVISION
// paragraph in which environment-names are related to user-specified
// mnemonic-names.
// The SPECIAL-NAMES paragraph:
// - Relates IBM-specified environment-names to user-defined mnemonic-names
// - Relates alphabet-names to character sets or collating sequences
// - Specifies symbolic characters
// - Relates class names to sets of characters
// - Specifies one or more currency sign values and defines a picture symbol to
//   represent each currency sign value in PICTURE clauses
// - Specifies that the functions of the comma and decimal point are to be
//   interchanged in PICTURE clauses and numeric literals
// - Relates xml-schema-names to ddnames or environment variable names
//   identifying files containing XML schemas

// p113 : Format: SPECIAL-NAMES paragraph

// !! p112 : The clauses in the SPECIAL-NAMES paragraph can appear in any order.

specialNamesParagraph: 
    SPECIAL_NAMES PeriodSeparator
    ((upsiSwitchNameClause |
      environmentNameClause |                        
      alphabetClause |
      symbolicCharactersClause |
      classClause |
      currencySignClause |
      decimalPointClause |
      xmlSchemaClause
     )+ PeriodSeparator)?;

// p115 : upsiSwitchName
// A 1-byte user-programmable status indicator (UPSI) switch.
// Valid specifications for environment-name-2 are UPSI-0 through UPSI-7.

                        // !! Impossible to avoid a TARGET LANGUAGE DEPENDENT semantic predicate here 
                        //    (... ambiguity with environmentNameClause ...)
upsiSwitchNameClause:   { CurrentToken.Text.StartsWith("UPSI-", System.StringComparison.OrdinalIgnoreCase) }? 
    upsiSwitchName ( (IS? mnemonicForUPSISwitchNameDefinition conditionNamesForUPSISwitch?) | 
					  conditionNamesForUPSISwitch );

conditionNamesForUPSISwitch:
    (onConditionNameForUPSISwitch offConditionNameForUPSISwitch?) |
    (offConditionNameForUPSISwitch onConditionNameForUPSISwitch?);

onConditionNameForUPSISwitch:
    ON STATUS? IS? conditionForUPSISwitchNameDefinition;

offConditionNameForUPSISwitch:
    OFF STATUS? IS? conditionForUPSISwitchNameDefinition;
							
// p114 : environment-name-1 
// System devices or standard system actions taken by the compiler.
								 
environmentNameClause: 
    environmentName IS? mnemonicForEnvironmentNameDefinition;

// p 115 : The ALPHABET clause provides a means of relating an alphabet-name to a
// specified character code set or collating sequence.
// The related character code set or collating sequence can be used for alphanumeric
// data, but not for DBCS or national data.

alphabetClause: 
    ALPHABET alphabetNameDefinition IS? (intrinsicAlphabetNameReference | userDefinedCollatingSequence+);

userDefinedCollatingSequence:
    (charactersInCollatingSequence | charactersRange | charactersEqualSet);

// In the rule below, if characterInCollatingSequence is an alphanumeric literal, 
// it may contain SEVERAL characters

charactersInCollatingSequence:
	alphanumericValue1 | ordinalPositionInCollatingSequence;

// In the two rules below, if characterInCollatingSequence is an alphanumeric literal, 
// it can contain ONLY ONE characters

charactersRange:
    startCharacter=characterInCollatingSequence (THROUGH | THRU) endCharacter=characterInCollatingSequence;

charactersEqualSet:
    characterInCollatingSequence (ALSO characterInCollatingSequence)+;

// p116 : literal-1, literal-2, literal-3
// Specifies that the collating sequence for alphanumeric data is
// determined by the program, according to the following rules:
// - The order in which literals appear specifies the ordinal number,
// in ascending sequence, of the characters in this collating
// sequence.
// - Each numeric literal specified must be an unsigned integer.
// - Each numeric literal must have a value that corresponds to a
// valid ordinal position within the collating sequence in effect.
// See Appendix C, “EBCDIC and ASCII collating sequences,” on
// page 569 for the ordinal numbers for characters in the
// single-byte EBCDIC and ASCII collating sequences.
// - Each character in an alphanumeric literal represents that actual
// character in the character set. (If the alphanumeric literal
// contains more than one character, each character, starting with
// the leftmost, is assigned a successively ascending position within
// this collating sequence.)
// - Any characters that are not explicitly specified assume positions
// in this collating sequence higher than any of the explicitly
// specified characters. The relative order within the collating
// sequence of these unspecified characters is their relative order in
// the collating sequence indicated by the COLLSEQ compiler
// option.
// - Within one alphabet-name clause, a given character must not be
// specified more than once.
// - Each alphanumeric literal associated with a THROUGH or ALSO
// phrase must be one character in length.
// - When the THROUGH phrase is specified, the contiguous
// characters in the native character set beginning with the
// character specified by literal-1 and ending with the character
// specified by literal-2 are assigned successively ascending
// positions in this collating sequence.
// This sequence can be either ascending or descending within the
// original native character set. That is, if "Z" THROUGH "A" is
// specified, the ascending values, left-to-right, for the uppercase
// letters are:
// ZYXWVUTSRQPONMLKJIHGFEDCBA
// - When the ALSO phrase is specified, the characters specified as
// literal-1, literal-3, ... are assigned to the same position in this
// collating sequence. For example, if you specify:
// "D" ALSO "N" ALSO "%"
// the characters D, N, and % are all considered to be in the same
// position in the collating sequence.
// - When the ALSO phrase is specified and alphabet-name-1 is
// referenced in a SYMBOLIC CHARACTERS clause, only literal-1
// is used to represent the character in the character set.
// - The character that has the highest ordinal position in this
// collating sequence is associated with the figurative constant
// HIGH-VALUE. If more than one character has the highest
// position because of specification of the ALSO phrase, the last
// character specified (or defaulted to when any characters are not
// explicitly specified) is considered to be the HIGH-VALUE
// character for procedural statements such as DISPLAY and as the
// sending field in a MOVE statement. (If the ALSO phrase
// example given above were specified as the high-order characters
// of this collating sequence, the HIGH-VALUE character would be
// %.)
// - The character that has the lowest ordinal position in this
// collating sequence is associated with the figurative constant
// LOW-VALUE. If more than one character has the lowest position
// because of specification of the ALSO phrase, the first character
// specified is the LOW-VALUE character. (If the ALSO phrase
// example given above were specified as the low-order characters
// of the collating sequence, the LOW-VALUE character would be
// D.)
// When literal-1, literal-2, or literal-3 is specified, the alphabet-name
// must not be referred to in a CODE-SET clause (see “CODE-SET
// clause” on page 183).
// literal-1, literal-2, and literal-3 must be alphanumeric or numeric
// literals. All must have the same category. A floating-point literal, a
// national literal, a DBCS literal, or a symbolic-character figurative
// constant must not be specified.

characterInCollatingSequence: 
	characterValue2 | ordinalPositionInCollatingSequence;

// p117 : The SYMBOLIC CHARACTERS clause is applicable only to single-byte character
// sets. Each character represented is an alphanumeric character.
// SYMBOLIC CHARACTERS symbolic-character-1
// Provides a means of specifying one or more symbolic characters.

symbolicCharactersClause:
	SYMBOLIC CHARACTERS? symbolicCharactersOrdinalPositions+ (IN alphabetNameReference)?;

symbolicCharactersOrdinalPositions:
    symbolicCharacterDefinition+ (ARE|IS)? ordinalPositionInCollatingSequence+;

// p116 : literal-1, literal-2, literal-3
// - Each numeric literal specified must be an unsigned integer.
// - Each numeric literal must have a value that corresponds to a
// valid ordinal position within the collating sequence in effect.

ordinalPositionInCollatingSequence: integerValue;

// p118: The CLASS clause provides a means for relating a name to the specified set of
// characters listed in that clause.

// p118 : THROUGH, THRU
// THROUGH and THRU are equivalent. If THROUGH is specified,
// class-name includes those characters that begin with the value of
// literal-4 and that end with the value of literal-5. In addition, the
// characters specified by a THROUGH phrase can be in either
// ascending or descending order.

classClause: 
    CLASS characterClassNameDefinition IS? userDefinedCharacterClass+;

userDefinedCharacterClass: 
	(charactersInCollatingSequence | charactersRange);
				
// p118 : literal-4, literal-5
// Must be category numeric or alphanumeric, and both must be of the same
// category.
// If numeric, literal-4 and literal-5 must be unsigned integers and must have
// a value that is greater than or equal to 1 and less than or equal to the
// number of characters in the alphabet specified. Each number corresponds
// to the ordinal position of each character in the single-byte EBCDIC or
// ASCII collating sequence.
// If alphanumeric, literal-4 and literal-5 are an actual single-byte EBCDIC
// character.
// literal-4 and literal-5 must not specify a symbolic-character figurative
// constant. If the value of the alphanumeric literal contains multiple
// characters, each character in the literal is included in the set of characters
// identified by class-name.
// Floating-point literals cannot be used in the CLASS clause.
// If the alphanumeric literal is associated with a THROUGH phrase, the
// literal must be one character in length.

// => charactersLiteral / charactersRange

// p118 : The CURRENCY SIGN clause affects numeric-edited data items whose PICTURE
// character-strings contain a currency symbol.
// A currency symbol represents a currency sign value that is:
// - Inserted in such data items when they are used as receiving items
// - Removed from such data items when they are used as sending items for a
// numeric or numeric-edited receiver
// Typically, currency sign values identify the monetary units stored in a data item.
// For example: '$', 'EUR', 'CHF', 'JPY', 'HK$', 'HKD', or X'9F' (hexadecimal code point
// in some EBCDIC code pages for €, the Euro currency sign). For details on
// programming techniques for handling the Euro, see Using currency signs in the
// Enterprise COBOL Programming Guide.
// The CURRENCY SIGN clause specifies a currency sign value and the currency
// symbol used to represent that currency sign value in a PICTURE clause.
// The SPECIAL-NAMES paragraph can contain multiple CURRENCY SIGN clauses.
// Each CURRENCY SIGN clause must specify a different currency symbol. Unlike all
// other PICTURE clause symbols, currency symbols are case sensitive. For example,
// 'D' and 'd' specify different currency symbols.

// p119: CURRENCY SIGN IS literal-6
// literal-6 must be an alphanumeric literal. literal-6 must not be a figurative
// constant or a null-terminated literal. literal-6 must not contain a DBCS
// character.
// If the PICTURE SYMBOL phrase is not specified, literal-6:
// - Specifies both a currency sign value and the currency symbol for this
// currency sign value
// - Must be a single character
// - Must not contain any of the following digits or characters:
//   – Digits 0 through 9
//   – Alphabetic characters A, B, C, D, E, G, N, P, R, S, V, X, Z, their
//     lowercase equivalents, or the space
//   – Special characters + - , . * / ; ( ) " = ’ (plus sign, minus sign, comma,
//     period, asterisk, slash, semicolon, left parenthesis, right parenthesis,
//     quotation mark, equal sign, apostrophe)
// - Can be one of the following lowercase alphabetic characters: f, h, i, j, k, l,
// m, o, q, t, u, w, y
// If the PICTURE SYMBOL phrase is specified, literal-6:
// - Specifies a currency sign value. literal-7 in the PICTURE SYMBOL phrase
// specifies the currency symbol for this currency sign value.
// - Can consist of one or more characters.
// - Must not contain any of the following digits or characters:
// – Digits 0 through 9
// – Special characters + - . ,

// p119: PICTURE SYMBOL literal-7
// Specifies a currency symbol that can be used in a PICTURE clause to
// represent the currency sign value specified by literal-6.
// literal-7 must be an alphanumeric literal consisting of one single-byte
// character. literal-7 must not contain any of the following digits or
// characters:
// - A figurative constant
// - Digits 0 through 9
// - Alphabetic characters A, B, C, D, E, G, N, P, R, S, V, X, Z, their lowercase
// equivalents, or the space
// - Special characters + - , . * / ; ( ) " = ’
// If the CURRENCY SIGN clause is specified, the CURRENCY and NOCURRENCY
// compiler options are ignored. If the CURRENCY SIGN clause is not specified and
// the NOCURRENCY compiler option is in effect, the dollar sign ($) is used as the
// default currency sign value and currency symbol. For more information about the
// CURRENCY and NOCURRENCY compiler options, see CURRENCY in the
// Enterprise COBOL Programming Guide.

// p119: The SPECIAL-NAMES paragraph can contain multiple CURRENCY SIGN clauses.

currencySignClause:
    CURRENCY SIGN? IS? alphanumericValue1 (WITH? PICTURE SYMBOL characterValue1)?;

// p120: The DECIMAL-POINT IS COMMA clause exchanges the functions of the period
// and the comma in PICTURE character-strings and in numeric literals.

decimalPointClause:
     DECIMAL_POINT IS? COMMA;

// p120: The XML-SCHEMA clause provides the means of relating xml-schema-name-1 to an
// external file identifier: a ddname or environment variable that identifies the actual
// external file that contains the optimized XML schema.
// The external file identifier can be specified as a user-defined word external-fileid-1
// or as an alphanumeric literal literal-8, and identifies an existing external z/OSUNIX
// file or MVS data set that contains the optimized XML schema.
// The external file identifier must be either the name specified in the DD statement
// for the file or the name of an environment variable that contains the file
// identification information.
// For details on specifying an environment variable, see “Environment variable
// contents for an XML schema file.” => p120

// p120 : literal-8
// Specifies an alphanumeric literal that must conform to the following rules:
// - The literal can contain one to eight characters.
// - The literal can contain the characters, A-Z, a-z, 0-9, @, #, and $.
// - The leading character must be alphabetic, @, #, and $.

// p120 : The compiler folds external-fileid-1 or literal-8 to uppercase to form the ddname or
//environment variable name for the file.

xmlSchemaClause:
    XML_SCHEMA xmlSchemaNameDefinition IS? assignmentName;

// p121: external-fileid-1
// Specifies a user-defined word that must conform to the following rules:
// - The user-defined word can contain one to eight characters.
// - The user-defined word can contain the characters, A-Z, a-z, 0-9.
// - The leading character must be alphabetic.

// Looks a lot like an assignment name ...
// externalFileId : UserDefinedWord;

// p121: The REPOSITORY paragraph is used in a program or class definition to identify all
// the object-oriented classes that are intended to be referenced in that program or
// class definition. Optionally, the REPOSITORY paragraph defines associations
// between class-names and external class-names.

// p122: See the general rules of the REPOSITORY paragraph.
// 1. All referenced class-names must have an entry in the repository paragraph of
// the COBOL program or class definition that contains the reference. You can
// specify a given class-name only once in a given repository paragraph.
// 2. In program definitions, the repository paragraph can be specified only in the
// outermost program.
// 3. The repository paragraph of a COBOL class definition can optionally contain an
// entry for the name of the class itself, but this entry is not required. Such an
// entry can be used to specify an external class-name that uses non-COBOL
// characters or that specifies a fully package-qualified class-name when a COBOL
// class is to be part of a Java package.
// 4. Entries in a class repository paragraph apply to the entire class definition,
// including all methods introduced by that class. Entries in a program repository
// paragraph apply to the entire program, including its contained programs.

// p122 : Identifying and referencing a class
// An external-class-name is used to identify and reference a given class from outside
// the class definition that defines the class.
// The external class-name is determined by using the contents of
// external-class-name-1, external-class-name-2, or class-name-1 (as specified in the
// repository paragraph of a class), as described below:
// 1. external-class-name-1 and external-class-name-2 are used directly, without
// translation. They are processed in a case-sensitive manner.
// 2. class-name-1 is used if external-class-name-1 or java-array-class-reference is not
// specified. To create an external name that identifies the class and conforms to
// Java rules of formation, class-name-1 is processed as follows:
// - The name is converted to uppercase.
// - Hyphens are translated to zero.
// - Underscores are not translated.
// - If the first character of the name is a digit, it is converted as follows:
//    – Digits 1 though 9 are changed to A through I.
//    – 0 is changed to J.
// The class can be implemented in Java or COBOL.
// When referencing a class that is part of a Java package, external-class-name-1 must
// be specified and must give the fully qualified Java class-name.
// For example, the repository entry
// Repository.
// Class JavaException is "java.lang.Exception"
// defines local class-name JavaException for referring to the fully qualified
// external-class-name "java.lang.Exception."
// When defining a COBOL class that is to be part of a Java package, specify an entry
// in the repository paragraph of that class itself, giving the full Java
// package-qualified name as the external class-name.

// p122: Format: REPOSITORY paragraph
// class-name-1
// A user-defined word that identifies the class.

// NB: externalClassName and javaArrayClassReference are both alphanumeric literals.
// AFTER the parsing phase, if the alphanumeric literal starts with :
// jbooleanArray / jbyteArray / jshortArray / jintArray / jlongArray / jcharArray / jobjectArray 
// then it is a javaArrayClassReference.
// The literal can then contain an optional colon character and an externalClassName.

// Sample : Programming guide p615
// Repository. 
// Class jobjectArray is "jobjectArray" 
// Class Employee is "com.acme.Employee" 
// Class Department is "jobjectArray:com.acme.Employee". 

repositoryParagraph: 
    REPOSITORY PeriodSeparator 
    ( repositoryClassDeclaration+ PeriodSeparator )?;

repositoryClassDeclaration:
	CLASS classNameDefOrRef (IS? externalClassNameDefOrRef)?;

// p122: java-array-class-reference
// A reference that enables a COBOL program to access a class that represents
// an array object, where the elements of the array are themselves objects.
// java-array-class-reference must be an alphanumeric literal with content in the
// following format: 

// p122: jobjectArray
// Specifies a Java object array class.

// p122: 
// : A required separator when external-class-name-2 is specified. The
// colon must not be preceded or followed by space characters.

// p122: external-class-name-2
// The external class-name of the type of the elements of the array.
// external-class-name-2 must follow the same rules of formation as
// external-class-name-1.
// When the repository entry specifies jobjectArray without the colon
// separator and external-class-name-2, the elements of the object array are of
// type java.lang.Object.

// javaArrayClassReference : "jobjectArray" | "jobjectArray:externalClassName";

// p125: The input-output section of the ENVIRONMENT DIVISION contains
// FILE-CONTROL paragraph and I-O-CONTROL paragraph.
// The exact contents of the input-output section depend on the file organization and
// access methods used. See “ORGANIZATION clause” on page 135 and “ACCESS
// MODE clause” on page 138.

// p125: 
// Program input-output section
//   The same rules apply to program and method I-O sections.
// Class input-output section
//   The input-output section is not valid for class definitions.
// Method input-output section
//   The same rules apply to program and method I-O sections.

inputOutputSectionHeader:
    INPUT_OUTPUT SECTION PeriodSeparator;

// p125: FILE-CONTROL
// The keyword FILE-CONTROL identifies the file-control paragraph. This
// keyword can appear only once, at the beginning of the FILE-CONTROL
// paragraph. It must begin in Area A and be followed by a separator period.
// The keyword FILE-CONTROL and the period can be omitted if no
// file-control-paragraph is specified and there are no files defined in the
// program.

// p125: file-control-paragraph
// Names the files and associates them with the external data sets.
// Must begin in Area B with a SELECT clause. It must end with a separator
// period.
// file-control-paragraph can be omitted if there are no files defined in the
// program, even if the FILE-CONTROL keyword is specified.

// p126: The FILE-CONTROL paragraph associates each file in the COBOL program with
// an external data set, and specifies file organization, access mode, and other
// information.
// The following formats are for the FILE-CONTROL paragraph:
// - Sequential file entries
// - Indexed file entries
// - Relative file entries
// - Line-sequential file entries
// The table below lists the different type of files available to programs and methods.
// File organization - Access method
// Sequential - QSAM, VSAM1
// Relative - VSAM1
// Indexed - VSAM1
// Line sequential2 - Text stream I-O
// 1. VSAM does not support z/OS UNIX files.
// 2. Line-sequential support is limited to z/OS UNIX files.

fileControlParagraphHeader:
    FILE_CONTROL PeriodSeparator;

// p130: The SELECT clause identifies a file in the COBOL program to be associated with an external data set. 

// !! p126 : Within each entry, the SELECT clause must appear first. 
// The other clauses can appear in any order, 
// except that the PASSWORD clause for indexed files, if specified, must immediately follow the RECORD KEY or ALTERNATE RECORD KEY data-name with which it is associated.

fileControlEntry:          
	selectClause
	(assignClause |
	 reserveClause |
	 organizationClause |
	 paddingCharacterClause |
	 recordDelimiterClause |
	 accessModeClause |
	 recordKeyClause |
	 alternateRecordKeyClause |                     
	 relativeKeyClause |
	 passwordClause |
	 fileStatusClause)+
	PeriodSeparator;
                     
// --- SUMMARY ---
// The organizationClause tells which one of the formats below must be controlled.
// If organization clause is absent, then check format 1 : sequential file.
// p127: Format 1: sequential-file-control-entry
// - organizationClause is optional
// - paddingCharacterClause can exist only in format 1
// - recordDelimiterClause can exist only in format 1
// - accessMode can only be SEQUENTIAL
// p128: Format 2: indexed-file-control-entry
// - accessMode can be SEQUENTIAL | RANDOM | DYNAMIC
// - recordKeyClause can exist only in format 2
// - recordKeyClause is mandatory in format 2
// - alternateRecordKeyClause can exist only in format 2
// p129: Format 3: relative-file-control-entry
// - accessMode can be SEQUENTIAL | RANDOM | DYNAMIC
// - relativeKeyClause can exist only in format 3
// - relativeKeyClause is mandatory for accessMode RANDOM | DYNAMIC
// p129: Format 4: line-sequential-file-control-entry
// - accessMode can only be SEQUENTIAL
// - passwordClause is not supported in format 4

// p130: SELECT OPTIONAL 
// Can be specified only for files opened in the input, I-O, or extend mode. 
// You must specify SELECT OPTIONAL for those input files that are not necessarily available each time the object program is executed. 
// For more information, see “OPEN statement notes” on page 382.
// p130: If the file connector referenced by file-name-1 is an external file connector, all file-control entries in the run unit that reference this file connector must have the same specification for the OPTIONAL phrase.

selectClause:
    SELECT OPTIONAL? fileNameDefinition;

// p130: ASSIGN clause
// The ASSIGN clause associates the name of a file in a program with the actual external name of the data file.
// p130: When file-name-1 specifies a sort or a merge file, only the ASSIGN clause can follow the SELECT clause.

assignClause:
    ASSIGN TO? assignmentName+;

// p130: assignment-name-1 Identifies the external data file. 
// It can be specified as a name or as an alphanumeric literal. 
// assignment-name-1 is not the name of a data item, and assignment-name-1 cannot be contained in a data item. 
// It is just a character string. It cannot contain an underscore character. 
// Any assignment-name after the first is syntax checked, but has no effect on the execution of the program. 
// p126: The name component of assignment-name-1 cannot contain an underscore.

// p130: Format: assignment-name for QSAM files
// label-? S-? name
// p131: Format: assignment-name for VSAM sequential file
// label-? AS- name
// p131: Format: assignment-name for line-sequential, VSAM indexed, or VSAM relative file
// label-? name

// p131: label- Documents (for the programmer) the device and device class to which a file is assigned. 
// It must end in a hyphen; the specified value is not otherwise checked. It has no effect on the execution of the program. 
// If specified, it must end with a hyphen. 
// S- For QSAM files, the S- (organization) field can be omitted. 
// AS- For VSAM sequential files, the AS- (organization) field must be specified. 
// For VSAM indexed and relative files, the organization field must be omitted. 
// name A required field that specifies the external name for this file. 
// It must be either the name specified in the DD statement for this file or the name of an environment variable that contains file allocation information. 
// For details on specifying an environment variable, see “Assignment name for environment variable” on page 132. 
// name must conform to the following rules of formation: 
// * If assignment-name-1 is a user-defined word: 
// – The name can contain from one to eight characters. 
// – The name can contain the characters A-Z, a-z, and 0-9. 
// – The leading character must be alphabetic. 
// – The name cannot contain an underscore. 
// * If assignment-name-1 is a literal: 
// – The name can contain from one to eight characters. 
// – The name can contain the characters A-Z, a-z, 0-9, @, #, and $. 
// – The leading character must be alphabetic. 
// – The name cannot contain an underscore. 
// For both user-defined words and literals, the compiler folds name to uppercase to form the ddname for the file. 
// In a sort or merge file, name is treated as a comment.

// p131: If the file connector referenced by file-name-1 in the SELECT clause is an external file connector, 
// all file-control entries in the run unit that reference this file connector must have a consistent specification for assignment-name-1 in the ASSIGN clause.
// For QSAM files and VSAM indexed and relative files, the name specified on the first assignment-name-1 must be identical.
// For VSAM sequential files, it must be specified as AS-name. 

// p132: Assignment name for environment variable
// The name component of assignment-name-1 is initially treated as a ddname. 
// If no file has been allocated using this ddname, then name is treated as an environment variable

// ... + a lot more details on the environment variable p132 -> 134 ...

// p135: RESERVE clause 
// The RESERVE clause allows the user to specify the number of input/output buffers to be allocated at run time for the files.
// The RESERVE clause is not supported for line-sequential files.
// If the RESERVE clause is omitted, the number of buffers at run time is taken from the DD statement. If none is specified, the system default is taken.
// If the file connector referenced by file-name-1 in the SELECT clause is an external file connector, all file-control entries in the run unit that reference this file connector must have the same value for the integer specified in the RESERVE clause.

reserveClause:
    RESERVE integerValue (AREA | AREAS)?;

// p135: ORGANIZATION clause
// The ORGANIZATION clause identifies the logical structure of the file. 
// The logical structure is established at the time the file is created and cannot subsequently be changed.
// You can find a discussion of the different ways in which data can be organized and of the different access methods that you can use to retrieve the data under “File organization and access modes” on page 139. 
// ORGANIZATION IS SEQUENTIAL (format 1) 
// A predecessor-successor relationship among the records in the file is established by the order in which records are placed in the file when it is created or extended. 
// ORGANIZATION IS INDEXED (format 2) 
// The position of each logical record in the file is determined by indexes created with the file and maintained by the system. 
// The indexes are based on embedded keys within the file's records. 
// ORGANIZATION IS RELATIVE (format 3) 
// The position of each logical record in the file is determined by its relative record number. 
// ORGANIZATION IS LINE SEQUENTIAL (format 4)
// A predecessor-successor relationship among the records in the file is established by the order in which records are placed in the file when it is created or extended. 
// A record in a LINE SEQUENTIAL file can consist only of printable characters.

// p135: If you omit the ORGANIZATION clause, the compiler assumes ORGANIZATION IS SEQUENTIAL.
// If the file connector referenced by file-name-1 in the SELECT clause is an external file connector, the same organization must be specified for all file-control entries in the run unit that reference this file connector.

// ... + a lot more details on file organization p135 -> 137 ...

organizationClause:
    (ORGANIZATION IS?)? ((SEQUENTIAL | INDEXED | RELATIVE) | (LINE SEQUENTIAL));

// p137: The PADDING CHARACTER clause specifies a character to be used for block padding on sequential files. 
// data-name-5 Must be defined in the DATA DIVISION as a one-character data item of category alphabetic, alphanumeric, or national, and must not be defined in the FILE SECTION. data-name-5 can be qualified. 
// literal-2 Must be a one-character alphanumeric literal or national literal.
// For external files, data-name-5, if specified, must reference an external data item.
// The PADDING CHARACTER clause is syntax checked, but has no effect on the execution of the program.

paddingCharacterClause:
    PADDING CHARACTER? IS? characterVariable;

// p138: The RECORD DELIMITER clause indicates the method of determining the length of a variable-length record on an external medium. 
// It can be specified only for variable-length records. 
// STANDARD-1 
// If STANDARD-1 is specified, the external medium must be a magnetic tape file. 
// assignment-name-2 Can be any COBOL word.
// The RECORD DELIMITER clause is syntax checked, but has no effect on the execution of the program.

recordDelimiterClause:
    RECORD DELIMITER IS? (STANDARD_1 | literalOrUserDefinedWordOReservedWordExceptCopy);

// p138: The ACCESS MODE clause defines the manner in which the records of the file are made available for processing. 
// If the ACCESS MODE clause is not specified, sequential access is assumed.
// For sequentially accessed relative files, the ACCESS MODE clause does not have to precede the RELATIVE KEY clause.

// ... + more details on access modes p138 -> 140 ...
// File organization is the permanent logical structure of the file. 
// You tell the computer how to retrieve records from the file by specifying the access mode (sequential, random, or dynamic).

// p139: For external files, every file-control entry in the run unit that is associated with that external file must specify the same access mode. 
// In addition, for relative file entries, data-name-4 must reference an external data item, and the RELATIVE KEY phrase in each associated file-control entry must reference that same external data item. 

accessModeClause:                    
    ACCESS MODE? IS? (SEQUENTIAL | RANDOM | DYNAMIC);

// p140: The RECORD KEY clause (format 2) specifies the data item within the record that is the prime RECORD KEY for an indexed file. 
// The values contained in the prime RECORD KEY data item must be unique among records in the file. 
// data-name-2 The prime RECORD KEY data item. 
// data-name-2 must be described within a record description entry associated with the file. 
// The key can have any of the following data categories: v Alphanumeric v Numeric v Numeric-edited (with usage DISPLAY or NATIONAL) v Alphanumeric-edited v Alphabetic v External floating-point (with usage DISPLAY or NATIONAL) v Internal floating-point v DBCS v National v National-edited 
// Regardless of the category of the key data item, the key is treated as an alphanumeric item. 
// The collation order of the key is determined by the item's binary value order when the key is used for locating a record or for setting the file position indicator associated with the file.
// data-name-2 must not reference a group item that contains a variable-occurrence data item. 
// data-name-2 can be qualified. 
// If the indexed file contains variable-length records, data-name-2 need not be contained within the minimum record size specified for the file. That is, data-name-2 can exceed the minimum record size, but this is not recommended. 
// The data description of data-name-2 and its relative location within the record must be the same as those used when the file was defined.
// If the file has more than one record description entry, data-name-2 need be described in only one of those record description entries. The identical character positions referenced by data-name-2 in any one record description entry are implicitly referenced as keys for all other record description entries for that file.
// For files defined with the EXTERNAL clause, all file description entries in the run unit that are associated with the file must have data description entries for data-name-2 that specify the same relative location in the record and the same length.

recordKeyClause:
    RECORD KEY? IS? dataNameReference;

// p141: The ALTERNATE RECORD KEY clause (format 2) specifies a data item within the record that provides an alternative path to the data in an indexed file. 
// data-name-3 An ALTERNATE RECORD KEY data item. 
// data-name-3 must be described within a record description entry associated with the file. The key can have any of the following data categories: v Alphanumeric v Numeric v Numeric-edited (with usage DISPLAY or NATIONAL) v Alphanumeric-edited v Alphabetic v External floating-point (with usage DISPLAY or NATIONAL) v Internal floating-point v DBCS v National v National-edited 
// Regardless of the category of the key data item, the key is treated as an alphanumeric item. 
// The collation order of the key is determined by the item's binary value order when the key is used for locating a record or for setting the file position indicator associated with the file. 
// data-name-3 must not reference a group item that contains a variable-occurrence data item. 
// data-name-3 can be qualified. 
// If the indexed file contains variable-length records, data-name-3 need not be contained within the minimum record size specified for the file. That is, data-name-3 can exceed the minimum record size, but this is not recommended. 
// If the file has more than one record description entry, data-name-3 need be described in only one of these record description entries. The identical character positions referenced by data-name-3 in any one record description entry are implicitly referenced as keys for all other record description entries of that file. 
// The data description of data-name-3 and its relative location within the record must be the same as those used when the file was defined. The number of alternate record keys for the file must also be the same as that used when the file was created. 
// The leftmost character position of data-name-3 must not be the same as the leftmost character position of the prime RECORD KEY or of any other ALTERNATE RECORD KEY.
// If the DUPLICATES phrase is not specified, the values contained in the ALTERNATE RECORD KEY data item must be unique among records in the file.
// If the DUPLICATES phrase is specified, the values contained in the ALTERNATE RECORD KEY data item can be duplicated within any records in the file. In sequential access, the records with duplicate keys are retrieved in the order in which they were placed in the file. In random access, only the first record written in a series of records with duplicate keys can be retrieved.
// For files defined with the EXTERNAL clause, all file description entries in the run unit that are associated with the file must have data description entries for data-name-3 that specify the same relative location in the record and the same length. The file description entries must specify the same number of alternate record keys and the same DUPLICATES phrase.

alternateRecordKeyClause:
    ALTERNATE RECORD? KEY? IS? recordKey=dataNameReference (WITH? DUPLICATES)? (PASSWORD IS? password=dataNameReference)?;

// p142: The RELATIVE KEY clause (format 3) identifies a data-name that specifies the relative record number for a specific logical record within a relative file. 
// data-name-4 Must be defined as an unsigned integer data item whose description does not contain the PICTURE symbol P. 
// data-name-4 must not be defined in a record description entry associated with this relative file. That is, the RELATIVE KEY is not part of the record. 
// data-name-4 can be qualified. 
// data-name-4 is required for ACCESS IS SEQUENTIAL only when the START statement is to be used. It is always required for ACCESS IS RANDOM and ACCESS IS DYNAMIC. When the START statement is executed, the system uses the contents of the RELATIVE KEY data item to determine the record at which sequential processing is to begin.
// If a value is placed in data-name-4, and a START statement is not executed, the value is ignored and processing begins with the first record in the file. 
// If a relative file is to be referenced by a START statement, you must specify the RELATIVE KEY clause for that file. 
// For external files, data-name-4 must reference an external data item, and the RELATIVE KEY phrase in each associated file-control entry must reference that same external data item in each case.
// The ACCESS MODE IS RANDOM clause must not be specified for file-names specified in the USING or GIVING phrase of a SORT or MERGE statement.

relativeKeyClause:
    RELATIVE KEY? IS? dataNameReference;
            
// p143: The PASSWORD clause controls access to files. 
// data-name-6 , data-name-7 Password data items. 
// Each must be defined in the WORKING-STORAGE SECTION of the DATA DIVISION as a data item of category alphabetic, alphanumeric, or alphanumeric-edited.
// The first eight characters are used as the password; a shorter field is padded with blanks to eight characters. 
// Each password data item must be equivalent to one that is externally defined.
// When the PASSWORD clause is specified, at object time the PASSWORD data item must contain a valid password for this file before the file can be successfully opened.
// Format 1 considerations:
// The PASSWORD clause is not valid for QSAM sequential files.
// Format 2 and 3 considerations:
// The PASSWORD clause, if specified, must immediately follow the RECORD KEY or ALTERNATE RECORD KEY data-name with which it is associated.
// For indexed files that have been completely predefined to VSAM, only the PASSWORD data item for the RECORD KEY need contain the valid password before the file can be successfully opened at file creation time.
// For any other type of file processing (including the processing of dynamic calls at file creation time through a COBOL runtime subroutine), every PASSWORD data item for the file must contain a valid password before the file can be successfully opened, regardless of whether all paths to the data are used in this object program.
// For external files, data-name-6 and data-name-7 must reference external data items. The PASSWORD clauses in each associated file-control entry must reference the same external data items.

passwordClause:
    PASSWORD IS? dataNameReference;

// p143: The FILE STATUS clause monitors the execution of each input-output operation for the file.
// When the FILE STATUS clause is specified, the system moves a value into the file status key data item after each input-output operation that explicitly or implicitly refers to this file. 
// The value indicates the status of execution of the statement. (See the file status key description under “Common processing facilities” on page 286.) 
// data-name-1 The file status key data item can be defined in the WORKING-STORAGE, LOCAL-STORAGE, or LINKAGE SECTION as one of the following items: v A two-character data item of category alphanumeric v A two-character data item of category national v A two-digit data item of category numeric with usage DISPLAY or NATIONAL (an external decimal data item) 
// data-name-1 must not contain the PICTURE symbol 'P'.
// data-name-1 can be qualified.
// The file status key data item must not be variably located; that is, the data item cannot follow a data item that contains an OCCURS DEPENDING ON clause. 
// data-name-8 Must be defined as an alphanumeric group item of 6 bytes in the WORKING-STORAGE SECTION or LINKAGE SECTION of the DATA DIVISION. 
// Specify data-name-8 only if the file is a VSAM file (that is, ESDS, KSDS, RRDS). 
// data-name-8 holds the 6-byte VSAM return code, which is composed as follows: v The first 2 bytes of data-name-8 contain the VSAM return code in binary format. The value for this code is defined (by VSAM) as 0, 8, or 12. v The next 2 bytes of data-name-8 contain the VSAM function code in binary format. The value for this code is defined (by VSAM) as 0, 1, 2, 3, 4, or 5. v The last 2 bytes of data-name-8 contain the VSAM feedback code in binary format. The code value is 0 through 255. If VSAM returns a nonzero return code, data-name-8 is set. If FILE STATUS is returned without having called VSAM, data-name-8 is zero. 
// If data-name-1 is set to zero, the content of data-name-8 is undefined. VSAM status return code information is available without transformation in the currently defined COBOL FILE STATUS code. User identification and handling of exception conditions are allowed at the same level as that defined by VSAM. 
// Function code and feedback code are set if and only if the return code is set to a nonzero value. If they are referenced when the return code is set to zero, the contents of the fields are not dependable.
// Values in the return code, function code, and feedback code fields are defined by VSAM. There are no COBOL additions, deletions, or modifications to the VSAM definitions. 
// For more information, see DFSMS Macro Instructions for Data Sets.

fileStatusClause:
    FILE? STATUS IS? fileStatus=storageArea2 vsamReturnCode=storageArea2?;

// p125 : I-O-CONTROL
// The keyword I-O-CONTROL identifies the I-O-CONTROL paragraph.

// p126 : i-o-control-paragraph
// Specifies information needed for efficient transmission of data between the
// external data set and the COBOL program. The series of entries must end
// with a separator period.

// p144: The I-O-CONTROL paragraph of the input-output section specifies when checkpoints are to be taken and the storage areas to be shared by different files. 
// This paragraph is optional in a COBOL program.
// The keyword I-O-CONTROL can appear only once, at the beginning of the paragraph. The word I-O-CONTROL must begin in Area A and must be followed by a separator period.
// The order in which I-O-CONTROL paragraph clauses are written is not significant. 
// !! The I-O-CONTROL paragraph ends with a separator period.

ioControlParagraphHeader:
    I_O_CONTROL PeriodSeparator;

// --- SUMMARY ---
// 4 different formats can be controlled for an ioControlEntry depending on the type of file.
// p145: Format 1: QSAM- i-o-control-entry
// - rerunClause mandatory
// - only SAME **RECORD** AREA supported
// - multipleFileTapeClause and applyWriteOnlyClause supported
// p145: Format 2: VSAM- i-o-control-entry
// - rerunClause mandatory
// - rerun EVERY ... END OF not supported
// - only SAME **RECORD** AREA supported
// - multipleFileTapeClause and applyWriteOnlyClause NOT supported
// p146: Format 3: line-sequential-i-o-control-entry
// - rerunClause not supported
// - only SAME **RECORD** AREA supported
// - multipleFileTapeClause and applyWriteOnlyClause NOT supported
// p146: Format 4: sort/merge-i-o-control-entry
// - rerunClause optional
// - rerun ON fileName not supported (assignmentName only)
// - rerun EVERY ... OF not supported
// - rerun EVERY ... END OF not supported
// - SAME (SORT | SORT_MERGE AREA) supported
// - multipleFileTapeClause and applyWriteOnlyClause NOT supported

ioControlEntry:
    rerunClause |
    sameAreaClause |
    multipleFileTapeClause |
    applyWriteOnlyClause;
          
// p146: The RERUN clause specifies that checkpoint records are to be taken. 
// Subject to the restrictions given with each phrase, more than one RERUN clause can be specified.
// For information regarding the checkpoint data set definition and the checkpoint method required for complete compliance to Standard COBOL 85, see DD statements for defining checkpoint data sets in the Enterprise COBOL Programming Guide.
// Do not use the RERUN clause: 
// - For files described with the EXTERNAL clause 
// - In programs with the RECURSIVE clause specified 
// - In programs compiled with the THREAD option 
// - In methods 
// file-name-1 Must be a sequentially organized file. 
// VSAM and QSAM considerations: 
// The file named in the RERUN clause must be a file defined in the same program as the I-O-CONTROL paragraph, even if the file is defined as GLOBAL. 
// assignment-name-1 The external data set for the checkpoint file. It must not be the same assignment-name as that specified in any ASSIGN clause throughout the entire program, including contained and containing programs. 
// For QSAM files, assignment-name-1 has the format: label-? S-? name 
// The QSAM file must reside on a tape or direct access device. See also Appendix F, “ASCII considerations,” on page 595. 
// SORT/MERGE considerations: 
// When the RERUN clause is specified in the I-O-CONTROL paragraph, checkpoint records are written at logical intervals determined by the sort/merge program during execution of each SORT or MERGE statement in the program. When the RERUN clause is omitted, checkpoint records are not written. 
// There can be only one SORT/MERGE I-O-CONTROL paragraph in a program, and it cannot be specified in contained programs. It will have a global effect on all SORT and MERGE statements in the program unit. 
// EVERY integer-1 RECORDS 
// A checkpoint record is to be written for every integer-1 records in file-name-1 that are processed. 
// When multiple integer-1 RECORDS phrases are specified, no two of them can specify the same value for file-name-1. 
// If you specify the integer-1 RECORDS phrase, you must specify assignment-name-1. 
// EVERY END OF REEL/UNIT
// A checkpoint record is to be written whenever end-of-volume for file-name-1 occurs. The terms REEL and UNIT are interchangeable. 
// When multiple END OF REEL/UNIT phrases are specified, no two of them can specify the same value for file-name-1. 
// The END OF REEL/UNIT phrase can be specified only if file-name-1 is a sequentially organized file.

rerunClause:
    RERUN ON? assignmentNameOrFileNameReference 
	(EVERY? ( (integerValue RECORDS) | (END OF? (REEL | UNIT)) ) OF? fileNameReference)?;
   
// p147: The SAME AREA clause is syntax checked, but has no effect on the execution of the program.
// The SAME AREA clause specifies that two or more files that do not represent sort or merge files are to use the same main storage area during processing.
// The files named in a SAME AREA clause need not have the same organization or access. 
// file-name-3 , file-name-4 Must be specified in the file-control paragraph of the same program. file-name-3 and file-name-4 must not reference a file that is defined with the EXTERNAL clause. 
// - For QSAM files, the SAME clause is treated as documentation. 
// - For VSAM files, the SAME clause is treated as if equivalent to the SAME RECORD AREA clause.
// More than one SAME AREA clause can be included in a program. However: 
// - A specific file-name must not appear in more than one SAME AREA clause. 
// - If one or more file-names of a SAME AREA clause appear in a SAME RECORD AREA clause, all the file-names in that SAME AREA clause must appear in that SAME RECORD AREA clause. However, the SAME RECORD AREA clause can contain additional file-names that do not appear in the SAME AREA clause. 
// - The rule that in the SAME AREA clause only one file can be open at one time takes precedence over the SAME RECORD AREA rule that all the files can be open at the same time.

// p148: The SAME RECORD AREA clause specifies that two or more files are to use the same main storage area for processing the current logical record.
// The files named in a SAME RECORD AREA clause need not have the same organization or access. 
// file-name-3 , file-name-4 Must be specified in the file-control paragraph of the same program. file-name-3 and file-name-4 must not reference a file that is defined with the EXTERNAL clause.
// All of the files can be opened at the same time. A logical record in the shared storage area is considered to be both of the following ones: 
// - A logical record of each opened output file in the SAME RECORD AREA clause 
// - A logical record of the most recently read input file in the SAME RECORD AREA clause
// More than one SAME RECORD AREA clause can be included in a program. However: 
// - A specific file-name must not appear in more than one SAME RECORD AREA clause. 
// - If one or more file-names of a SAME AREA clause appear in a SAME RECORD AREA clause, all the file-names in that SAME AREA clause must appear in that SAME RECORD AREA clause. However, the SAME RECORD AREA clause can contain additional file-names that do not appear in the SAME AREA clause. 
// - The rule that in the SAME AREA clause only one file can be open at one time takes precedence over the SAME RECORD AREA rule that all the files can be open at the same time. 
// - If the SAME RECORD AREA clause is specified for several files, the record description entries or the file description entries for these files must not include the GLOBAL clause.
// - The SAME RECORD AREA clause must not be specified when the RECORD CONTAINS 0 CHARACTERS clause is specified.

// p149: The SAME SORT AREA clause is syntax checked but has no effect on the execution of the program. 
/// file-name-3, file-name-4 Must be specified in the file-control paragraph of the same program.  file-name-3 and file-name-4 must not reference a file that is defined with the EXTERNAL clause.
// When the SAME SORT AREA clause is specified, at least one file-name specified must name a sort file. Files that are not sort files can also be specified. 
// The following rules apply: 
// - More than one SAME SORT AREA clause can be specified. However, a given sort file must not be named in more than one such clause. 
// - If a file that is not a sort file is named in both a SAME AREA clause and in one or more SAME SORT AREA clauses, all the files in the SAME AREA clause must also appear in that SAME SORT AREA clause. 
// - Files named in a SAME SORT AREA clause need not have the same organization or access. 
// - Files named in a SAME SORT AREA clause that are not sort files do not share storage with each other unless they are named in a SAME AREA or SAME RECORD AREA clause. 
// - During the execution of a SORT or MERGE statement that refers to a sort or merge file named in this clause, any nonsort or nonmerge files associated with file-names named in this clause must not be in the open mode.

// p149: The SAME SORT-MERGE AREA clause is equivalent to the SAME SORT AREA clause.

sameAreaClause:
    SAME (RECORD | SORT | SORT_MERGE)? AREA? FOR? fileNameReference+;

// p149: The MULTIPLE FILE TAPE clause (format 1) specifies that two or more files share the same physical reel of tape.
// This clause is syntax checked, but has no effect on the execution of the program. The function is performed by the system through the LABEL parameter of the DD statement.

multipleFileTapeClause:
    MULTIPLE FILE TAPE? CONTAINS? physicalReelOfTape+;

physicalReelOfTape:
	fileNameReference (POSITION integerValue)?;

// p149: The APPLY WRITE-ONLY clause optimizes buffer and device space allocation for files that have standard sequential organization, have variable-length records, and are blocked.
// If you specify this phrase, the buffer is truncated only when the space available in the buffer is smaller than the size of the next record. 
// Otherwise, the buffer is truncated when the space remaining in the buffer is smaller than the maximum record size for the file.
// APPLY WRITE-ONLY is effective only for QSAM files. 
// file-name-2 Each file must have standard sequential organization.
// APPLY WRITE-ONLY clauses must agree among corresponding external file description entries. 
// For an alternate method of achieving the APPLY WRITE-ONLY results, see the description of the compiler option, AWO in the Enterprise COBOL Programming Guide.

applyWriteOnlyClause:
    APPLY WRITE_ONLY ON? fileNameReference+;

// p153: Each section in the DATA DIVISION has a specific logical function within a COBOL program, object definition, factory definition, or method and can be omitted when that logical function is not needed. 
// !! If included, the sections must be written in the order shown. 
// The DATA DIVISION is optional. 
// Program data division 
// The DATA DIVISION of a COBOL source program describes, in a structured manner, all the data to be processed by the program. 
// Object data division 
// The object data division contains data description entries for instance object data (instance data). Instance data is defined in the WORKING-STORAGE SECTION of the object paragraph of a class definition. 
// Factory data division 
// The factory data division contains data description entries for factory object data (factory data). Factory data is defined in the WORKING-STORAGE SECTION of the factory paragraph of a class definition. 
// Method data division 
// A method data division contains data description entries for data accessible within the method. A method data division can contain a LOCAL-STORAGE SECTION or a WORKING-STORAGE SECTION, or both. The term method data applies to both. Method data in LOCAL-STORAGE is dynamically allocated and initialized on each invocation of the method; method data in WORKING-STORAGE is static and persists across invocations of the method.

// p154: Format: program and method data division
// p154: Format: object and factory data division => WORKING-STORAGE SECTION only

dataDivisionHeader:
    DATA DIVISION PeriodSeparator;
	
// p154: The FILE SECTION defines the structure of data files.
// The FILE SECTION must begin with the header FILE SECTION, followed by a separator period.
// file-description-entry 
// Represents the highest level of organization in the FILE SECTION. It provides information about the physical structure and identification of a file, and gives the record-names associated with that file. 
// For the format and the clauses required in a file description entry, see Chapter 17, “DATA DIVISION--file description entries,” on page 169. 
// record-description-entry 
// A set of data description entries (described in Chapter 18, “DATA DIVISION--data description entry,” on page 185) that describe the particular records contained within a particular file. 
// A record in the FILE SECTION must be described as an alphanumeric group item, a national group item, or an elementary data item of class alphabetic, alphanumeric, DBCS, national, or numeric.
// More than one record description entry can be specified; each is an alternative description of the same record storage area.
// Data areas described in the FILE SECTION are not available for processing unless the file that contains the data area is open.
// A method FILE SECTION can define external files only. A single run-unit-level file connector is shared by all programs and methods that contain a declaration of a given external file.

fileSectionHeader:
    FILE SECTION PeriodSeparator;

// p155: The WORKING-STORAGE SECTION describes data records that are not part of data files but are developed and processed by a program or method. 
// The WORKING-STORAGE SECTION also describes data items whose values are assigned in the source program or method and do not change during execution of the object program.
// The WORKING-STORAGE SECTION must begin with the section header WORKING-STORAGE SECTION, followed by a separator period

// ... more details on WORKING-STORAGE for Program/Method/Object/Factory p155 -> 156 ...

// p156: The WORKING-STORAGE SECTION contains record description entries and data description entries for independent data items, called data item description entries. 
// record-description-entry 
// Data entries in the WORKING-STORAGE SECTION that bear a definite hierarchic relationship to one another must be grouped into records structured by level number. 
// See Chapter 18, “DATA DIVISION--data description entry,” on page 185 for more information. 
// data-item-description-entry 
// Independent items in the WORKING-STORAGE SECTION that bear no hierarchic relationship to one another need not be grouped into records provided that they do not need to be further subdivided. Instead, they are classified and defined as independent elementary items. 
// Each is defined in a separate data-item description entry that begins with either the level number 77 or 01. 
// See Chapter 18, “DATA DIVISION--data description entry,” on page 185 for more information.

workingStorageSectionHeader:
    WORKING_STORAGE SECTION PeriodSeparator;

// p156: The LOCAL-STORAGE SECTION defines storage that is allocated and freed on a per-invocation basis.
// On each invocation, data items defined in the LOCAL-STORAGE SECTION are reallocated. Each data item that has a VALUE clause is initialized to the value specified in that clause.

// ... more details on LOCAL-STORAGE p156 ...

// p156: Data items defined in the LOCAL-STORAGE SECTION cannot specify the EXTERNAL clause.
// The LOCAL-STORAGE SECTION must begin with the header LOCAL-STORAGE SECTION, followed by a separator period.
// You can specify the LOCAL-STORAGE SECTION in recursive programs, in nonrecursive programs, and in methods.
// Method LOCAL-STORAGE content is the same as program LOCAL-STORAGE content except that the GLOBAL clause has no effect (because methods cannot be nested).

localStorageSectionHeader:
    LOCAL_STORAGE SECTION PeriodSeparator;                             

// p157: The LINKAGE SECTION describes data made available from another program or method. 
// Record description entries and data item description entries in the LINKAGE SECTION provide names and descriptions, but storage within the program or method is not reserved because the data area exists elsewhere.
// Any data description clause can be used to describe items in the LINKAGE SECTION with the following exceptions: 
// -You cannot specify the VALUE clause for items other than level-88 items. 
// - You cannot specify the EXTERNAL clause.
// You can specify the GLOBAL clause in the LINKAGE SECTION. The GLOBAL clause has no effect for methods, however.

// p250: References to items in the LINKAGE SECTION
// Data items defined in the LINKAGE SECTION of the called program or invoked
// method can be referenced within the PROCEDURE DIVISION of that program if
// and only if they satisfy one of the conditions as listed in the topic.
// - They are operands of the USING phrase of the PROCEDURE DIVISION header
//   or the ENTRY statement.
// - They are operands of SET ADDRESS OF, CALL ... BY REFERENCE ADDRESS
//   OF, or INVOKE ... BY REFERENCE ADDRESS OF.
// - They are defined with a REDEFINES or RENAMES clause, the object of which
//   satisfies the above conditions.
// - They are items subordinate to any item that satisfies the condition in the rules
//   above.
// - They are condition-names or index-names associated with data items that satisfy
//   any of the above conditions.

linkageSectionHeader:
    LINKAGE SECTION PeriodSeparator;

// p157: Data is grouped into the conceptual units as listed in the topic. 
// - File data 
// - Program data - Method data - Factory data - Instance data 

// ... more details on data units p157 -> 158 ...

// p158: File description entries specify the physical aspects of the data (such as the size relationship between physical and logical records, the size and names of the logical records, labeling information, and so forth).

// p169: In a COBOL program, the File Description (FD) Entry (or Sort File Description (SD) Entry for sort/merge files) represents the highest level of organization in the FILE SECTION. 
// !! The order in which the optional clauses follow the FD or SD entry is not important.

// --- SUMMARY ---
// 4 different formats can be controlled for an fileDescriptionEntry depending on the type of file.
// p170: Format 1: sequential file description entry
// - FD fileName
// p171: Format 2: relative or indexed file description entry
// - FD fileName
// - LABEL option | dataName not supported
// - LINAGE not supported
// - RECORDING MODE not supported
// - CODE-SET not supported
// p172: Format 3: line-sequential file description entry
// - FD fileName
// - BLOCK CONTAINS not supported
// - CONTAINS? integer TO integer CHARACTERS? not supported
// - LABEL not supported
// - VALUE OF not supported
// - DATA not supported
// - LINAGE not supported
// - RECORDING MODE not supported
// - CODE-SET not supported
// p173: Format 4: sort/merge file description entry
// - SD fileName
// - (IS? EXTERNAL)? (IS? GLOBAL)? not supported
// - RECORDING MODE not supported

// p174: The FILE SECTION must contain a level-indicator for each input and output file.
// For all files except sort or merge files, the FILE SECTION must contain an FD
// entry. For each sort or merge file, the FILE SECTION must contain an SD entry.
//
// file-name
// Must follow the level indicator (FD or SD), and must be the same as that
// specified in the associated SELECT clause. file-name must adhere to the
// rules of formation for a user-defined word; at least one character must be
// alphabetic. file-name must be unique within this program.
//
// One or more record description entries must follow file-name. When more
// than one record description entry is specified, each entry implies a
// redefinition of the same storage area.
//
// The clauses that follow file-name are optional, and they can appear in any
// order.
//
// FD (formats 1, 2, and 3)
// The last clause in the FD entry must be immediately followed by a
// separator period.
// SD (format 4)
// An SD entry must be written for each sort or merge file in the program.
// The last clause in the SD entry must be immediately followed by a
// separator period.
//
// The following example illustrates the FILE SECTION entries needed for a
// sort or merge file:
// SD SORT-FILE.
// 01 SORT-RECORD PICTURE X(80).
// A record in the FILE SECTION must be described as an alphanumeric group item,
// a national group item, or an elementary item of class alphabetic, alphanumeric,
// DBCS, national, or numeric.

// p159: A level indicator, with its descriptive entry, identifies each file in a program. 
// Level indicators represent the highest level of any data hierarchy with which they are associated. 
// FD is the file description level indicator and SD is the sort-merge file description level indicator.
// levelIndicator : (FD | SD);

fileDescriptionEntry: 
    (FD | SD) fileNameReference 
    (externalClause |
     globalClause |
     blockContainsClause |
     recordClause |
     labelRecordsClause |
     valueOfClause |
     dataRecordsClause |
     linageClause |
     recordingModeClause |
     codeSetClause)*
    PeriodSeparator;

// p174 : The EXTERNAL clause specifies that a file connector is external, and permits
// communication between two programs by the sharing of files.
// A file connector is external if the storage associated with that file is associated with
// the run unit rather than with any particular program within the run unit. An
// external file can be referenced by any program in the run unit that describes the
// file. References to an external file from different programs that use separate
// descriptions of the file are always to the same file. In a run unit, there is only one
// representative of an external file.
// In the FILE SECTION, the EXTERNAL clause can be specified only in file
// description entries.
// The records appearing in the file description entry need not have the same name in
// corresponding external file description entries. In addition, the number of such
// records need not be the same in corresponding file description entries.
// Use of the EXTERNAL clause does not imply that the associated file-name is a
// global name. See Sharing data by using the EXTERNAL clause in the Enterprise
// COBOL Programming Guide for specific information about the use of the
// EXTERNAL clause.

externalClause:
    IS? EXTERNAL;

// p175: The GLOBAL clause specifies that the file connector named by a file-name is a
// global name. A global file-name is available to the program that declares it and to
// every program that is contained directly or indirectly in that program.
// A file-name is global if the GLOBAL clause is specified in the file description entry
// for that file-name. A record-name is global if the GLOBAL clause is specified in the
// record description entry by which the record-name is declared or, in the case of
// record description entries in the FILE SECTION, if the GLOBAL clause is specified
// in the file description entry for the file-name associated with the record description
// entry. For details on using the GLOBAL clause, see Using data in input and output
// operations and Scope of names in the Enterprise COBOL Programming Guide.
// Two programs in a run unit can reference global file connectors in the following
// circumstances:
// - An external file connector can be referenced from any program that describes
//   that file connector.
// - If a program is contained within another program, both programs can refer to a
//   global file connector by referring to an associated global file-name either in the
//   containing program or in any program that directly or indirectly contains the
//   containing program.

globalClause:
    IS? GLOBAL;

// p175: The BLOCK CONTAINS clause specifies the size of the physical records.
// The CHARACTERS phrase indicates that the integer specified in the BLOCK
// CONTAINS clause reflects the number of bytes in the record. For example, if you
// have a block with 10 DBCS characters or 10 national characters, the BLOCK
// CONTAINS clause should say BLOCK CONTAINS 20 CHARACTERS.
// If the records in the file are not blocked, the BLOCK CONTAINS clause can be
// omitted. When it is omitted, the compiler assumes that records are not blocked.
// Even if each physical record contains only one complete logical record, coding
// BLOCK CONTAINS 1 RECORD would result in fixed blocked records.
// The BLOCK CONTAINS clause can be omitted when the associated file-control
// entry specifies a VSAM file. The concept of blocking has no meaning for VSAM
// files. The BLOCK CONTAINS clause is syntax checked but has no effect on the
// execution of the program.
// For external files, the value of all BLOCK CONTAINS clauses of corresponding
// external files must match within the run unit. This conformance is in terms of
// bytes and does not depend upon whether the value was specified as
// CHARACTERS or as RECORDS.
// integer-1 , integer-2
// Must be unsigned integers. They specify:
// * CHARACTERS
// Specifies the number of bytes required to store the physical record,
// no matter what USAGE the data items have within the data record.
// If only integer-2 is specified, it specifies the exact number of bytes
// in the physical record. When integer-1 and integer-2 are both
// specified, they represent the minimum and maximum number of
// bytes in the physical record, respectively.
// integer-1 and integer-2 must include any control bytes and padding
// contained in the physical record. (Logical records do not include
// padding.)
// The CHARACTERS phrase is the default. CHARACTERS must be
// specified when:
// - The physical record contains padding.
// - Logical records are grouped so that an inaccurate physical record
//   size could be implied. For example, suppose you describe a
//   variable-length record of 100 bytes, yet each time you write a
//   block of 4, one 50-byte record is written followed by three
//   100-byte records. If the RECORDS phrase were specified, the
//   compiler would calculate the block size as 420 bytes instead of
//   the actual size, 370 bytes. (This calculation includes block and
//   record descriptors.)
// * RECORDS
// Specifies the number of logical records contained in each physical
// record.
// The compiler assumes that the block size must provide for integer-2
// records of maximum size, and provides any additional space
// needed for control bytes.
// BLOCK CONTAINS 0 can be specified for QSAM files. If BLOCK CONTAINS 0 is
// specified for a QSAM file, then:
// - The block size is determined at run time from the DD parameters or the data set
//   label of the file. For output data sets, the DCB used by Language Environment
//   will have a zero block size value. When the DCB has a zero block size value, the
//   operating system might select a system-determined block size (SDB). See the
//   operating system specifications for further information about SDB.
// BLOCK CONTAINS can be omitted for SYSIN files and for SYSOUT files. The
// blocking is determined by the operating system.
// For a way to apply BLOCK CONTAINS 0 to QSAM files that do not already have
// a BLOCK CONTAINS clause, see the description of the compiler option, BLOCK0
// in the Enterprise COBOL Programming Guide.
// The BLOCK CONTAINS clause is syntax checked but has no effect on the
// execution of the program when specified under an SD.
// The BLOCK CONTAINS clause cannot be used with the RECORDING MODE U
// clause.

blockContainsClause:
    BLOCK CONTAINS? (minNumberOfBytes=integerValue TO)? maxNumberOfBytes=integerValue (CHARACTERS | RECORDS)?;

// p177: When the RECORD clause is used, the record size must be specified as the number
// of bytes needed to store the record internally, regardless of the USAGE of the data
// items contained within the record.
// For example, if you have a record with 10 DBCS characters, the RECORD clause
// should say RECORD CONTAINS 20 CHARACTERS. For a record with 10 national
// characters, the RECORD clause should say the same, RECORD CONTAINS 20
// CHARACTERS.
// The size of a record is determined according to the rules for obtaining the size of a
// group item. (See “USAGE clause” on page 228 and “SYNCHRONIZED clause”
// on page 223.)
// When the RECORD clause is omitted, the compiler determines the record lengths
// from the record descriptions. When one of the entries within a record description
// contains an OCCURS DEPENDING ON clause, the compiler uses the maximum
// value of the variable-length item to calculate the number of bytes needed to store
// the record internally.
// If the associated file connector is an external file connector, all file description
// entries in the run unit that are associated with that file connector must specify the
// same maximum number of bytes.

// p177: Format 1 specifies the number of bytes for fixed-length records.
// integer-3
// Must be an unsigned integer that specifies the number of bytes contained
// in each record in the file.
// The RECORD CONTAINS 0 CHARACTERS clause can be specified for
// input QSAM files containing fixed-length records; the record size is
// determined at run time from the DD statement parameters or the data set
// label. If, at run time, the actual record is larger than the 01 record
// description, then only the 01 record length is available. If the actual record
// is shorter, then only the actual record length can be referred to. Otherwise,
// uninitialized data or an addressing exception can be produced.
// Usage note: If the RECORD CONTAINS 0 clause is specified, then the
// SAME AREA, SAME RECORD AREA, or APPLY WRITE-ONLY clauses
// cannot be specified.
// Do not specify the RECORD CONTAINS 0 clause for an SD entry.

// p178: Format 2 specifies the number of bytes for either fixed-length or variable-length
// records.
// Fixed-length records are obtained when all 01 record description entry lengths are
// the same. The format-2 RECORD CONTAINS clause is never required, because the
// minimum and maximum record lengths are determined from the record
// description entries.
// integer-4, integer-5
// Must be unsigned integers. integer-4 specifies the size of the smallest data
// record, and integer-5 specifies the size of the largest data record.

// p178: Format 3 is used to specify variable-length records.
// integer-6
// Specifies the minimum number of bytes to be contained in any record of
// the file. If integer-6 is not specified, the minimum number of bytes to be
// contained in any record of the file is equal to the least number of bytes
// described for a record in that file.
// integer-7
// Specifies the maximum number of bytes in any record of the file. If
// integer-7 is not specified, the maximum number of bytes to be contained in
// any record of the file is equal to the greatest number of bytes described for
// a record in that file.
// The number of bytes associated with a record description is determined by the
// sum of the number of bytes in all elementary data items (excluding redefinitions
// and renamings), plus any implicit FILLER due to synchronization. If a table is
// specified:
// - The minimum number of table elements described in the record is used in the
//   summation above to determine the minimum number of bytes associated with
//   the record description.
// - The maximum number of table elements described in the record is used in the
//   summation above to determine the maximum number of bytes associated with
//   the record description.
// If data-name-1 is specified:
// - data-name-1 must be an elementary unsigned integer.
// - The number of bytes in the record must be placed into the data item referenced
//   by data-name-1 before any RELEASE, REWRITE, or WRITE statement is executed
//   for the file.
// - The execution of a DELETE, RELEASE, REWRITE, START, or WRITE statement
//   or the unsuccessful execution of a READ or RETURN statement does not alter
//   the content of the data item referenced by data-name-1.
// - After the successful execution of a READ or RETURN statement for the file, the
//   contents of the data item referenced by data-name-1 indicate the number of bytes
//   in the record just read.
// During the execution of a RELEASE, REWRITE, or WRITE statement, the number
// of bytes in the record is determined by the following conditions:
// - If data-name-1 is specified, by the content of the data item referenced by
//   data-name-1
// - If data-name-1 is not specified and the record does not contain a variable
//   occurrence data item, by the number of bytes positions in the record
// - If data-name-1 is not specified and the record contains a variable occurrence data
//   item, by the sum of the fixed position and that portion of the table described by
//   the number of occurrences at the time of execution of the output statement
// During the execution of a READ ... INTO or RETURN ... INTO statement, the
// number of bytes in the current record that participate as the sending data items in
// the implicit MOVE statement is determined by the following conditions:
// - If data-name-1 is specified, by the content of the data item referenced by
//   data-name-1
// - If data-name-1 is not specified, by the value that would have been moved into
//   the data item referenced by data-name-1 had data-name-1 been specified

recordClause:
    RECORD ((CONTAINS? numberOfBytes=integerValue CHARACTERS?) |
            (CONTAINS? minNumberOfBytes=integerValue TO maxNumberOfBytes=integerValue CHARACTERS?) |
            (IS? VARYING IN? SIZE? (FROM? fromNumberOfBytes=integerValue)? (TO toNumberOfBytes=integerValue)? CHARACTERS? (DEPENDING ON? dataNameReference)?));

// p179: For sequential, relative, or indexed files, and for sort/merge SDs, the LABEL
// RECORDS clause is syntax checked, but has no effect on the execution of the
// program.
// The LABEL RECORDS clause documents the presence or absence of labels.
// STANDARD
// Labels conforming to system specifications exist for this file.
// STANDARD is permitted for mass storage devices and tape devices.
// OMITTED
// No labels exist for this file.
// OMITTED is permitted for tape devices.
// data-name-2
// User labels are present in addition to standard labels. data-name-2 specifies
// the name of a user label record. data-name-2 must appear as the subject of a
// record description entry associated with the file.

labelRecordsClause:
    LABEL ((RECORD IS?) | (RECORDS ARE?)) ((STANDARD | OMITTED) | dataNameReference*);

// p180: The VALUE OF clause describes an item in the label records associated with the
// file.
// data-name-3
// Should be qualified when necessary, but cannot be subscripted. It must be
// described in the WORKING-STORAGE SECTION. It cannot be described
// with the USAGE IS INDEX clause.
// literal-1
// Can be numeric or alphanumeric, or a figurative constant of category
// numeric or alphanumeric. Cannot be a floating-point literal.
// The VALUE OF clause is syntax checked, but has no effect on the execution of the
// program.

valueOfClause:
    VALUE OF (qualifiedDataName IS? variable5)+;

// p180: The DATA RECORDS clause is syntax checked but serves only as documentation
// for the names of data records associated with the file.
// data-name-4
// The names of record description entries associated with the file.
// The data-name need not have an associated 01 level number record description
// with the same name.

dataRecordsClause:
    DATA ((RECORD IS?) | (RECORDS ARE?)) dataNameReference+;

// p180: The LINAGE clause specifies the depth of a logical page in number of lines.
// Optionally, it also specifies the line number at which the footing area begins and
// the top and bottom margins of the logical page. (The logical page and the physical
// page cannot be the same size.)
// The LINAGE clause is effective for sequential files opened as OUTPUT or
// EXTEND.
// All integers must be unsigned. All data-names must be described as unsigned
// integer data items.
// data-name-5 , integer-8
// The number of lines that can be written or spaced on this logical page. The
// area of the page that these lines represent is called the page body. The value
// must be greater than zero.
// WITH FOOTING AT
// integer-9 or the value of the data item in data-name-6 specifies the first line
// number of the footing area within the page body. The footing line number
// must be greater than zero, and not greater than the last line of the page
// body. The footing area extends between those two lines.
// LINES AT TOP
// integer-10 or the value of the data item in data-name-7 specifies the number
// of lines in the top margin of the logical page. The value can be zero.
// LINES AT BOTTOM
// integer-11 or the value of the data item in data-name-8 specifies the number
// of lines in the bottom margin of the logical page. The value can be zero.
// The following figure illustrates the use of each phrase of the LINAGE clause.
// The logical page size specified in the LINAGE clause is the sum of all values
// specified in each phrase except the FOOTING phrase. If the LINES AT TOP phrase
// is omitted, the assumed value for the top margin is zero. Similarly, if the LINES AT
// BOTTOM phrase is omitted, the assumed value for the bottom margin is zero.
// Each logical page immediately follows the preceding logical page, with no
// additional spacing provided.
// If the FOOTING phrase is omitted, its assumed value is equal to that of the page
// body (integer-8 or data-name-5).
// At the time an OPEN OUTPUT statement is executed, the values of integer-8,
// integer-9, integer-10, and integer-11, if specified, are used to determine the page
// body, first footing line, top margin, and bottom margin of the logical page for this
// file. (See the figure above.) These values are then used for all logical pages printed
// for this file during a given execution of the program.
// At the time an OPEN statement with the OUTPUT phrase is executed for the file,
// data-name-5, data-name-6, data-name-7, and data-name-8 determine the page body,
// first footing line, top margin, and bottom margin for the first logical page only.
// At the time a WRITE statement with the ADVANCING PAGE phrase is executed
// or a page overflow condition occurs, the values of data-name-5, data-name-6,
// data-name-7, and data-name-8 if specified, are used to determine the page body, first
// footing line, top margin, and bottom margin for the next logical page.
// If an external file connector is associated with this file description entry, all file
// description entries in the run unit that are associated with this file connector must
// have:
// - A LINAGE clause, if any file description entry has a LINAGE clause
// - The same corresponding values for integer-8, integer-9, integer-10, and integer-11,
//   if specified
// - The same corresponding external data items referenced by data-name-5,
//   data-name-6, data-name-7, and data-name-8
// See “ADVANCING phrase” on page 451 for the behavior of carriage control
// characters in external files.
// A LINAGE clause under an SD is syntax checked, but has no effect on the
// execution of the program.
// LINAGE-COUNTER special register
// For information about the LINAGE-COUNTER special register, see
// “LINAGE-COUNTER” on page 20.

linageClause:
    LINAGE IS? numberOfLinesInPage=integerVariable2 LINES? 
    (WITH? FOOTING AT? firstLineNumberOfFootingArea=integerVariable2)? 
    (LINES? AT? TOP numberOfLinesInTopMargin=integerVariable2)? 
    (LINES? AT? BOTTOM numberOfLinesInBottomMargin=integerVariable2)?;

// p182: The RECORDING MODE clause specifies the format of the physical records in a
// QSAM file. The clause is ignored for a VSAM file.

// p183: If the RECORDING MODE clause is not specified for a QSAM file, the Enterprise
// COBOL compiler determines the recording mode as follows:
// F 
// The compiler determines the recording mode to be F if the largest level-01
// record associated with the file is not greater than the block size specified in
// the BLOCK CONTAINS clause, and you do one of the following things:
// - Use the RECORD CONTAINS integer clause. (For more information, see
// the Enterprise COBOL Migration Guide.)
// - Omit the RECORD clause and make sure that all level-01 records
// associated with the file are the same size and none contains an OCCURS
// DEPENDING ON clause.
// V
// The compiler determines the recording mode to be V if the largest level-01
// record associated with the file is not greater than the block size specified in
// the BLOCK CONTAINS clause, and you do one of the following things:
// - Use the RECORD IS VARYING clause.
// - Omit the RECORD clause and make sure that all level-01 records
//   associated with the file are not the same size or some contain an
//   OCCURS DEPENDING ON clause.
// - Use the RECORD CONTAINS integer-1 TO integer-2 clause, with integer-1
//   the minimum length and integer-2 the maximum length of the level-01
//   records associated with the file. The two integers must be different, with
//   values matching minimum and maximum length of either different
//   length records or records with an OCCURS DEPENDING ON clause.
// S 
// The compiler determines the recording mode to be S if the maximum block
// size is smaller than the largest record size.
// U
// Recording mode U is never obtained by default. The RECORDING MODE
// U clause must be explicitly specified to get recording mode U.

recordingModeClause:
    RECORDING MODE? IS? recordingMode;

// p183: The CODE-SET clause specifies the character code used to represent data on a
// magnetic tape file. When the CODE-SET clause is specified, an alphabet-name
// identifies the character code convention used to represent data on the input-output
// device.
// alphabet-name must be defined in the SPECIAL-NAMES paragraph as
// STANDARD-1 (for ASCII-encoded files), STANDARD-2 (for ISO 7-bit encoded
// files), EBCDIC (for EBCDIC-encoded files), or NATIVE. When NATIVE is specified,
// the CODE-SET clause is syntax checked but has no effect on the execution of the
// program.
// The CODE-SET clause also specifies the algorithm for converting the character
// codes on the input-output medium from and to the internal EBCDIC character set.
// When the CODE-SET clause is specified for a file, all data in the file must have
// USAGE DISPLAY; and if signed numeric data is present, it must be described with
// the SIGN IS SEPARATE clause.
// When the CODE-SET clause is omitted, the EBCDIC character set is assumed for
// the file.
// If the associated file connector is an external file connector, all CODE-SET clauses
// in the run unit that are associated with the file connector must have the same
// character set.
// The CODE-SET clause is valid only for magnetic tape files.
// The CODE-SET clause is syntax checked but has no effect on the execution of the
// program when specified under an SD.

codeSetClause:
    CODE_SET IS? alphabetNameReference;

// p158: Record description entries describe the logical records in the file (including the category and format of data within each field of the logical record), different values the data might be assigned, and so forth.

// recordDescriptionEntry : dataDescriptionEntry+;

// p158: Items that need not be so grouped can be defined in independent data description entries (called data item description entries). 
 
// dataItemDescriptionEntry : dataDescriptionEntry;

// p185: A data description entry specifies the characteristics of a data item. In the sections
// that follow, sets of data description entries are called record description entries. The
// term data description entry refers to data and record description entries.
// Data description entries that define independent data items do not make up a
// record. These entries are known as data item description entries.

// p185: Data description entries have three general formats, and all data description entries
// must end with a separator period.

// p185: Format 1: data description entry
// Format 1 is used for data description entries in all DATA DIVISION sections.
// The clauses can be written in any order, with two exceptions:
// - data-name-1 or FILLER, if specified, must immediately follow the level-number.
// - When the REDEFINES clause is specified, it must immediately follow
//   data-name-1 or FILLER, if either is specified. If data-name-1 or FILLER is not
//   specified, the REDEFINES clause must immediately follow the level-number.
// The level-number in format 1 can be any number in the range 01–49, or 77.
// A space, a comma, or a semicolon must separate clauses.

// !! p205: The clauses can be written in any order, with two exceptions: 
// - data-name-1 or FILLER, if specified, must immediately follow the level-number. 
// - When the REDEFINES clause is specified, it must immediately follow data-name-1 or FILLER, if either is specified. If data-name-1 or FILLER is not specified, the REDEFINES clause must immediately follow the level-number.

dataDescriptionEntry:
	  // Semantic predicate necessary to distinguish 
	  // dataDescriptionEntry, dataRenamesEntry and dataConditionEntry
	( { CurrentToken.Text != "66" && CurrentToken.Text != "88" }? 	

	levelNumber=integerValue2 (dataNameDefinition | FILLER)? redefinesClause?
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
	)* PeriodSeparator
	
	)
	
	| dataRenamesEntry
	| dataConditionEntry;

// p186: Format 2: renames
// Format 2 regroups previously defined items.
// A level-66 entry cannot rename another level-66 entry, nor can it rename a level-01,
// level-77, or level-88 entry.
// All level-66 entries associated with one record must immediately follow the last
// data description entry in that record.

dataRenamesEntry: { CurrentToken.Text == "66" }? 
	levelNumber=integerValue2 dataNameDefinition renamesClause PeriodSeparator;

// p186: Format 3: condition-name
// Format 3 describes condition-names.
// condition-name-1
// A user-specified name that associates a value, a set of values, or a range of
// values with a conditional variable.
// Level-88 entries must immediately follow the data description entry for the
// conditional variable with which the condition-names are associated.
// Format 3 can be used to describe elementary items, national group items, or
// alphanumeric group items.

dataConditionEntry: { CurrentToken.Text == "88" }? 
	levelNumber=integerValue2 conditionNameDefinition valueClauseForCondition PeriodSeparator;

// p186: The level-number specifies the hierarchy of data within a record, and identifies
// special-purpose data entries. A level-number begins a data description entry, a
// renamed or redefined item, or a condition-name entry.
// A level-number has an integer value between 1 and 49, inclusive, or one of the
// special level-number values 66, 77, or 88.

// p187: level-number
// 01 and 77 must begin in Area A and be followed either by a separator
// period or by a space followed by its associated data-name, FILLER, or
// appropriate data description clause.
// Level numbers 02 through 49 can begin in Areas A or B and must be
// followed by a space or a separator period.
// Level numbers 66 and 88 can begin in Areas A or B and must be followed
// by a space.
// Single-digit level-numbers 1 through 9 can be substituted for
// level-numbers 01 through 09.
// Successive data description entries can start in the same column as the first
// entry or can be indented according to the level-number. Indentation does
// not affect the magnitude of a level-number.
// When level-numbers are indented, each new level-number can begin any
// number of spaces to the right of Area A. The extent of indentation to the
// right is limited only by the width of Area B.

// p158: The relationships among all data to be used in a program are defined in the DATA DIVISION through a system of level indicators and level-numbers.

// ... more details on Levels of data p 159 -> 161 ...
 
// p159: elementary items / group items
// The basic subdivisions of a record (that is, those fields not further subdivided) are called elementary items. Thus a record can be made up of a series of elementary items or can itself be an elementary item.
// It might be necessary to refer to a set of elementary items; thus, elementary items can be combined into group items. Groups can also be combined into a more inclusive group that contains one or more subgroups. 

// p159: A level-number, with its descriptive entry, indicates the properties of specific data. 
// Level-numbers can be used to describe a data hierarchy; they can indicate that this data has a special purpose. 
// Although they can be associated with (and subordinate to) level indicators, they can also be used independently to describe internal data or data common to two or more programs. 
// (See “Level-numbers” on page 186 for level-number rules.) 

// p159: A level-number is a one-digit or two-digit integer between 01 and 49, or one of three special level-numbers: 66, 77, or 88. 
// 01 
// This level-number specifies the record itself, and is the most inclusive level-number possible. 
// A level-01 entry can be an alphanumeric group item, a national group item, or an elementary item. 
// The level number must begin in Area A. 
// 02 through 49 
// These level-numbers specify group and elementary items within a record. 
// They can begin in Area A or Area B. 
// Less inclusive data items are assigned higher (not necessarily consecutive) level-numbers in this series.
// The relationship between level-numbers within a group item defines the hierarchy of data within that group.

// p161: 
// 66 
// Identifies items that must contain a RENAMES clause; such items regroup previously defined data items. (For details, see “RENAMES clause” on page 219.) 
// 77 
// Identifies data item description entries that are independent WORKING-STORAGE, LOCAL-STORAGE, or LINKAGE SECTION items; they are not subdivisions of other items and are not subdivided themselves. 
// Level-77 items must begin in Area A. 
// 88 
// Identifies any condition-name entry that is associated with a particular value of a conditional variable. (For details, see “VALUE clause” on page 237.)

// p161: Successive data description entries can begin in the same column as preceding entries, or can be indented.
// Indentation is useful for documentation but does not affect the action of the compiler. 

// p161: Level-77 and level-01 entries in the WORKING-STORAGE, LOCAL-STORAGE, or LINKAGE SECTION that are referenced in a program or method must be given unique data-names because level-77 and level-01 entries cannot be qualified. 
// Subordinate data-names that are referenced in the program or method must be either uniquely defined, or made unique through qualification. 
// Unreferenced data-names need not be uniquely defined. 

// levelNumber: integerValue2 => LevelNumber token

// p187: data-name-1
// Explicitly identifies the data being described.
// data-name-1, if specified, identifies a data item used in the program.
// data-name-1 must be the first word following the level-number.
// The data item can be changed during program execution.
// data-name-1 must be specified for level-66 and level-88 items. It must also
// be specified for any entry containing the GLOBAL or EXTERNAL clause,
// and for record description entries associated with file description entries
// that have the GLOBAL or EXTERNAL clauses.

// ... more details on Classes and categories of data p161 -> 166  ...

// p162: Most data and all literals used in a COBOL program are divided into classes and categories. 
// Data classes are groupings of data categories. 
// Data categories are determined by the attributes of data description entries or function definitions.

// p162: The following elementary data items do not have a class and category:
// - Index data items 
// - Items described with USAGE POINTER, USAGE FUNCTION-POINTER, USAGE PROCEDURE-POINTER, or USAGE OBJECT REFERENCE
// All other types of elementary data items have a class and category as shown in Table 8 on page 163.
// A function references an elementary data item and belongs to the data class and category associated with the type of the function, as shown in Table 9 on page 163.
// Literals have a class and category as shown in Table 10 on page 163. 
// Figurative constants (except NULL) have a class and category that depends on the literal or value represented by the figurative constant in the context of its use. For details, see “Figurative constants” on page 13.
// All group items have a class and category, even if the subordinate elementary items belong to another class and category. For the classification of group items, see “Classes and categories of group items” on page 161. 

// p164: The category of a data item is established by the attributes of its data description entry (such as its PICTURE character-string or USAGE clause) or by its function definition.

// p166: The standard alignment rules for positioning data in an elementary item depend on the category of a receiving item.
// ... more details on Alignment rules p166 ...

// p167: For items described with a PICTURE clause, the size of an elementary item is expressed in source code by the number of character positions described in the PICTURE character-string and a SIGN clause (if applicable). 
// Storage size, however, is determined by the actual number of bytes the item occupies as determined by the combination of its PICTURE character-string, SIGN IS SEPARATE clause (if specified), and USAGE clause.
// ... more details on Character-string and item size p167 ...

// p168: There are two categories of algebraic signs used in COBOL: operational signs and editing signs. 
// ... more details on Operational signs / Editing signs p168 ...

// p187: FILLER
// A data item that is not explicitly referred to in a program. The keyword
// FILLER is optional. If specified, FILLER must be the first word following
// the level-number.
// The keyword FILLER can be used with a conditional variable if explicit
// reference is never made to the conditional variable but only to values that
// it can assume. FILLER cannot be used with a condition-name.
// In a MOVE CORRESPONDING statement or in an ADD
// CORRESPONDING or SUBTRACT CORRESPONDING statement, FILLER
// items are ignored. In an INITIALIZE statement, elementary FILLER items
// are ignored.
// If data-name-1 or the FILLER clause is omitted, the data item being described is
// treated as though FILLER had been specified.

// p188: The BLANK WHEN ZERO clause specifies that an item contains only spaces when
// its value is zero.
// The BLANK WHEN ZERO clause may be specified only for an elementary item
// described by its picture character string as category numeric-edited or numeric,
// without the picture symbol S or *. These items must be described, either implicitly
// or explicitly, as USAGE DISPLAY or USAGE NATIONAL.
// A BLANK WHEN ZERO clause that is specified for an item defined as numeric by
// its picture character string defines the item as category numeric-edited.

blankWhenZeroClause:
    BLANK WHEN? (ZERO | ZEROS | ZEROES);

// p188: The EXTERNAL clause specifies that the storage associated with a data item is
// associated with the run unit rather than with any particular program or method
// within the run unit.
// An external data item can be referenced by any program or method in the run unit
// that describes the data item. References to an external data item from different
// programs or methods using separate descriptions of the data item are always to
// the same data item. In a run unit, there is only one representative of an external
// data item.
// The EXTERNAL clause can be specified only on data description entries whose
// level-number is 01. It can be specified only on data description entries that are in
// the WORKING-STORAGE SECTION of a program or method. It cannot be
// specified in LINKAGE SECTION or FILE SECTION data description entries. Any
// data item described by a data description entry subordinate to an entry that
// describes an external record also attains the external attribute. Indexes in an
// external data record do not possess the external attribute.
// The data contained in the record named by the data-name clause is external and
// can be accessed and processed by any program or method in the run unit that
// describes and, optionally, redefines it. This data is subject to the following rules:
// - If two or more programs or methods within a run unit describe the same
//   external data record, each record-name of the associated record description
//   entries must be the same, and the records must define the same number of
//   bytes. However, a program or method that describes an external record can
//   contain a data description entry including the REDEFINES clause that redefines
//   the complete external record, and this complete redefinition need not occur
//   identically in other programs or methods in the run unit.
// - Use of the EXTERNAL clause does not imply that the associated data-name is a
//   global name.

// p189: The GLOBAL clause specifies that a data-name is available to every program
// contained within the program that declares it, as long as the contained program
// does not itself have a declaration for that name. All data-names subordinate to or
// condition-names or indexes associated with a global name are global names.
// A data-name is global if the GLOBAL clause is specified either in the data
// description entry by which the data-name is declared or in another entry to which
// that data description entry is subordinate. The GLOBAL clause can be specified in
// the WORKING-STORAGE SECTION, the FILE SECTION, the LINKAGE SECTION,
// and the LOCAL-STORAGE SECTION, but only in data description entries whose
// level-number is 01.
// In the same DATA DIVISION, the data description entries for any two data items
// for which the same data-name is specified must not include the GLOBAL clause.
// A statement in a program contained directly or indirectly within a program that
// describes a global name can reference that name without describing it again.
// Two programs in a run unit can reference common data in the following
// circumstances:
// - The data content of an external data record can be referenced from any program
//   that describes the data record as external.
// - If a program is contained within another program, both programs can refer to
//   data that possesses the global attribute either in the containing program or in
//   any program that directly or indirectly contains the containing program.

// p189: The JUSTIFIED clause overrides standard positioning rules for receiving items of
// category alphabetic, alphanumeric, DBCS, or national.
// You can specify the JUSTIFIED clause only at the elementary level. JUST is an
// abbreviation for JUSTIFIED, and has the same meaning.
// You cannot specify the JUSTIFIED clause:
// - For data items of category numeric, numeric-edited, alphanumeric-edited, or
//   national-edited
// - For edited DBCS items
// - For index data items
// - For items described as USAGE FUNCTION-POINTER, USAGE POINTER,
//   USAGE PROCEDURE-POINTER, or USAGE OBJECT REFERENCE
// - For external floating-point or internal floating-point items
// - With level-66 (RENAMES) and level-88 (condition-name) entries
// When the JUSTIFIED clause is specified for a receiving item, the data is aligned at
// the rightmost character position in the receiving item. Also:
// - If the sending item is larger than the receiving item, the leftmost character
//   positions are truncated.
// - If the sending item is smaller than the receiving item, the unused character
//   positions at the left are filled with spaces. For a DBCS item, each unused
//   position is filled with a DBCS space (X'4040'); for an item described with usage
//   NATIONAL, each unused position is filled with the default Unicode space
//   (NX'0020'); otherwise, each unused position is filled with an alphanumeric space.
// If you omit the JUSTIFIED clause, the rules for standard alignment are followed
// (see “Alignment rules” on page 166).
// The JUSTIFIED clause does not affect initial settings as determined by the VALUE
// clause.

justifiedClause:
    (JUSTIFIED | JUST) RIGHT?;

// p190: A GROUP-USAGE clause with the NATIONAL phrase specifies that the group
// item defined by the entry is a national group item. A national group item contains
// national characters in all subordinate data items and subordinate group items.
// When GROUP-USAGE NATIONAL is specified:
// - The subject of the entry is a national group item. The class and category of a
//  national group are national.
// - A USAGE clause must not be specified for the subject of the entry. A USAGE
//   NATIONAL clause is implied.
// - A USAGE NATIONAL clause is implied for any subordinate elementary data
//   items that are not described with a USAGE NATIONAL clause.
// - All subordinate elementary data items must be explicitly or implicitly described
//   with USAGE NATIONAL.
// - Any signed numeric data items must be described with the SIGN IS SEPARATE
//   clause.
// - A GROUP-USAGE NATIONAL clause is implied for any subordinate group
//   items that are not described with a GROUP-USAGE NATIONAL clause.
// - All subordinate group items must be explicitly or implicitly described with a
//   GROUP-USAGE NATIONAL clause.
// - The JUSTIFIED clause must not be specified.
// Unless stated otherwise, a national group item is processed as though it were an
// elementary data item of usage national, class and category national, described with
// PICTURE N(m), where m is the length of the group in national character positions.
// Usage note: When you use national groups, the compiler can ensure proper
// truncation and padding of group items for statements such as MOVE and
// INSPECT. Groups defined without a GROUP-USAGE NATIONAL clause are
// alphanumeric groups. The content of alphanumeric groups, including any national
// characters, is treated as alphanumeric data, possibly leading to invalid truncation
// or mishandling of national character data.
// The table below summarizes the cases where a national group item is processed as
// a group item.
// Language feature => Processing of national group items
// Name qualification => The name of a national group item can be used to qualify the names of
// elementary data items and subordinate group items in the national group. The
// rules of qualification for a national group are the same as the rules of
// qualification for an alphanumeric group.
// RENAMES clause => The rules for a national group item specified in the THROUGH phrase are the
// same as the rules for an alphanumeric group item specified in the THROUGH
// phrase. The result is an alphanumeric group item.
// CORRESPONDING phrase => A national group item is processed as a group in accordance with the rules of
// the CORRESPONDING phrase. Elementary data items within a national group
// are processed the same as they would be if defined within an alphanumeric
// group.
// INITIALIZE statement => A national group item is processed as a group in accordance with the rules of
// the INITIALIZE statement. Elementary items within the national group are
// initialized the same as they would be if defined within an alphanumeric
// group.
// XML GENERATE statement => A national group item specified in the FROM phrase is processed as a group in
// accordance with the rules of the XML GENERATE statement. Elementary items
// within the national group are processed the same as they would be if defined
// within an alphanumeric group.

groupUsageClause:
    GROUP_USAGE IS? NATIONAL;

// p191: The DATA DIVISION language elements used for table handling are the OCCURS
// clause and the INDEXED BY phrase.
// For the INDEXED BY phrase description, see “INDEXED BY phrase” on page 194.
// The OCCURS clause specifies tables whose elements can be referred to by indexing
// or subscripting. It also eliminates the need for separate entries for repeated data
// items.
// Formats for the OCCURS clause include fixed-length tables and variable-length
// tables.
// The subject of an OCCURS clause is the data-name of the data item that contains
// the OCCURS clause. Except for the OCCURS clause itself, data description clauses
// used with the subject apply to each occurrence of the item described.
// Whenever the subject of an OCCURS clause or any data-item subordinate to it is
// referenced, it must be subscripted or indexed, with the following exceptions:
// - When the subject of the OCCURS clause is used as the subject of a SEARCH
//   statement
// - When the subject or a subordinate data item is the object of the
//   ASCENDING/DESCENDING KEY phrase
// - When the subordinate data item is the object of the REDEFINES clause
// When subscripted or indexed, the subject refers to one occurrence within the table,
// unless the ALL subscript is used in an intrinsic function.
// The OCCURS clause cannot be specified in a data description entry that:
// - Has a level number of 01, 66, 77, or 88.
// - Describes a redefined data item. (However, a redefined item can be subordinate
//   to an item that contains an OCCURS clause.) See “REDEFINES clause” on page
//   216.
                
// p192: Fixed-length tables are specified using the OCCURS clause.
// Because seven subscripts or indexes are allowed, six nested levels and one
// outermost level of the format-1 OCCURS clause are allowed. The format-1
// OCCURS clause can be specified as subordinate to the OCCURS DEPENDING ON
// clause. In this way, a table of up to seven dimensions can be specified.
// integer-2
// The exact number of occurrences. integer-2 must be greater than zero.

// p195: Variable-length tables
// You can specify variable-length tables by using the OCCURS DEPENDING ON
// clause.
// integer-1
// The minimum number of occurrences.
// The value of integer-1 must be greater than or equal to zero, and it must
// also be less than the value of integer-2.
// If integer-1 is omitted, a value of 1 is assumed and the keyword TO must
// also be omitted.
// integer-2
// The maximum number of occurrences.
// integer-2 must be greater than integer-1.
// The length of the subject item is fixed. Only the number of repetitions of the subject
// item is variable.
// UNBOUNDED
// Unbounded maximum number of occurrences.
// Unbounded table
// A table with an OCCURS clause that specifies UNBOUNDED.
// You can reference unbounded tables in COBOL syntax anywhere a
// table can be referenced.
// Unbounded group
// A group that contains at least one unbounded table.
// You can define unbounded groups only in the LINKAGE
// SECTION. Either alphanumeric groups or national groups can be
// unbounded.
// You can reference unbounded groups in COBOL syntax anywhere
// an alphanumeric or national group can be referenced, with the
// following exceptions:
// - You cannot specify unbounded groups as a BY CONTENT argument
//   in a CALL statement.
// - You cannot specify unbounded groups as data-name-2 on the
//   PROCEDURE DIVISION RETURNING phrase.
// - You cannot specify unbounded groups as arguments to intrinsic
//   functions, except as an argument to the LENGTH intrinsic function.
// The total size of an unbounded group at run time must be less
// than 999,999,999 bytes.
// For unbounded tables and groups, the effect of the SSRANGE
// compiler option is limited. For more information, see SSRANGE in
// the Enterprise COBOL Programming Guide.
// For references about working with unbounded tables and groups,
// see Working with unbounded tables and groups in the Enterprise
// COBOL Programming Guide.

// p196: OCCURS DEPENDING ON clause
// The OCCURS DEPENDING ON clause specifies variable-length tables.
// data-name-1
// Identifies the object of the OCCURS DEPENDING ON clause; that is, the
// data item whose current value represents the current number of
// occurrences of the subject item. The contents of items whose occurrence
// numbers exceed the value of the object are undefined.
// The object of the OCCURS DEPENDING ON clause (data-name-1) must
// describe an integer data item.
// The object of the OCCURS DEPENDING ON clause must not occupy any
// storage position within the range of the table (that is, any storage position
// from the first character position in the table through the last character
// position in the table).
// The object of the OCCURS DEPENDING ON clause cannot be variably
// located; the object cannot follow an item that contains an OCCURS
// DEPENDING ON clause.
// If the OCCURS clause is specified in a data description entry included in a
// record description entry that contains the EXTERNAL clause, data-name-1,
// if specified, must reference a data item that possesses the external attribute.
// data-name-1 must be described in the same DATA DIVISION as the subject
// of the entry.
// If the OCCURS clause is specified in a data description entry subordinate
// to one that contains the GLOBAL clause, data-name-1, if specified, must be
// a global name. data-name-1 must be described in the same DATA DIVISION
// as the subject of the entry.
// All data-names used in the OCCURS clause can be qualified; they cannot be
// subscripted or indexed.
// At the time that the group item, or any data item that contains a subordinate
// OCCURS DEPENDING ON item or that follows but is not subordinate to the
// OCCURS DEPENDING ON item, is referenced, the value of the object of the
// OCCURS DEPENDING ON clause must fall within the range integer-1 through
// integer-2, if identifier-2 is specified.
// When a group item that contains a subordinate OCCURS DEPENDING ON item is
// referred to, the part of the table area used in the operation is determined as
// follows:
// - If the object is outside the group, only that part of the table area that is specified
//   by the object at the start of the operation is used.
// - If the object is included in the same group and the group data item is referenced
//   as a sending item, only that part of the table area that is specified by the value
//   of the object at the start of the operation is used in the operation.
// - If the object is included in the same group and the group data item is referenced
//   as a receiving item, the maximum length of the group item is used in the
//   operation.
// The following statements are affected by the maximum length rule:
// - ACCEPT identifier (format 1 and 2)
// - CALL ... USING BY REFERENCE identifier
// - INVOKE ... USING BY REFERENCE identifier
// - MOVE ... TO identifier
// - READ ... INTO identifier
// - RELEASE identifier FROM ...
// - RETURN ... INTO identifier
// - REWRITE identifier FROM ...
// - STRING ... INTO identifier
// - UNSTRING ... INTO identifier DELIMITER IN identifier
// - WRITE identifier FROM ...
// If a variable-length group item is not followed by a nonsubordinate item, the
// maximum length of the group is used when it appears as the identifier in CALL ...
// USING BY REFERENCE identifier. Therefore, the object of the OCCURS
// DEPENDING ON clause does not need to be set unless the group is variably
// located.
// If the group item is followed by a nonsubordinate item, the actual length, rather
// than the maximum length, is used. At the time the subject of entry is referenced, or
// any data item subordinate or superordinate to the subject of entry is referenced,
// the object of the OCCURS DEPENDING ON clause must fall within the range
// integer-1 through integer-2, if integer-2 is specified.
// Note: The maximum length rule does not apply to unbounded groups. For
// unbounded groups, based on the current run time value of the OCCURS
// DEPENDING ON objects, the actual length of the group is used for all references
// to the group. Consequently, before any COBOL statement that references an
// unbounded group runs, you must set the OCCURS DEPENDING ON objects for
// that group.
// Certain uses of the OCCURS DEPENDING ON clause result in complex OCCURS
// DEPENDING ON (ODO) items. The following constitute complex ODO items:
// - A data item described with an OCCURS DEPENDING ON clause that is
//   followed by a nonsubordinate elementary data item, described with or without
//   an OCCURS clause
// - A data item described with an OCCURS DEPENDING ON clause that is
//   followed by a nonsubordinate group item
// - A group item that contains one or more subordinate items described with an
//   OCCURS DEPENDING ON clause
// - A data item described with an OCCURS clause or an OCCURS DEPENDING
//   ON clause that contains a subordinate data item described with an OCCURS
//   DEPENDING ON clause (a table that contains variable-length elements)
// - An index-name associated with a table that contains variable-length elements
// The object of an OCCURS DEPENDING ON clause cannot be a nonsubordinate
// item that follows a complex ODO item.
// Any nonsubordinate item that follows an item described with an OCCURS
// DEPENDING ON clause is a variably located item. That is, its location is affected by
// the value of the OCCURS DEPENDING ON object.
// When implicit redefinition is used in a File Description (FD) entry, subordinate
// level items can contain OCCURS DEPENDING ON clauses.
// The INDEXED BY phrase can be specified for a table that has a subordinate item
// that contains an OCCURS DEPENDING ON clause.
// For more information about complex OCCURS DEPENDING ON, see Complex
// OCCURS DEPENDING ON in the Enterprise COBOL Programming Guide.

// p192: ASCENDING KEY and DESCENDING KEY phrases
// Data is arranged in ascending or descending order, depending on the keyword
// specified, according to the values contained in data-name-2. The data-names are
// listed in their descending order of significance.
// The order is determined by the rules for comparison of operands (see “Relation
// conditions” on page 259). The ASCENDING KEY and DESCENDING KEY data
// items are used in OCCURS clauses and the SEARCH ALL statement for a binary
// search of the table element.
// data-name-2
// Must be the name of the subject entry or the name of an entry subordinate
// to the subject entry. data-name-2 can be qualified.
// If data-name-2 names the subject entry, that entire entry becomes the
// ASCENDING KEY or DESCENDING KEY and is the only key that can be
// specified for this table element.
// If data-name-2 does not name the subject entry, then data-name-2:
// - Must be subordinate to the subject of the table entry itself
// - Must not be subordinate to, or follow, any other entry that contains an
//   OCCURS clause
// - Must not contain an OCCURS clause
// data-name-2 must not have subordinate items that contain OCCURS
// DEPENDING ON clauses.
// When the ASCENDING KEY or DESCENDING KEY phrase is specified, the
// following rules apply:
// - Keys must be listed in decreasing order of significance.
// - The total number of keys for a given table element must not exceed 12.
// - The data in the table must be arranged in ascending or descending sequence
//   according to the collating sequence in use.
// - The key must be described with one of the following usages:
//   BINARY / DISPLAY / DISPLAY-1 / NATIONAL / PACKED-DECIMAL / COMPUTATIONAL /
//   COMPUTATIONAL-1 / COMPUTATIONAL-2 / COMPUTATIONAL-3 / COMPUTATIONAL-4 /
//   COMPUTATIONAL-5
// - A key described with usage NATIONAL can have one of the following
//   categories: national, national-edited, numeric-edited, numeric, or external
//   floating-point.
// - The sum of the lengths of all the keys associated with one table element must
//   not exceed 256.
// - If a key is specified without qualifiers and it is not a unique name, the key will
//   be implicitly qualified with the subject of the OCCURS clause and all qualifiers
//   of the OCCURS clause subject.

// p194: In the preceding example, records in EMPLOYEE-TABLE must be arranged in
// ascending order of WAGE-RATE, and in ascending order of EMPLOYEE-NO
// within WAGE-RATE. Records in WEEK-RECORD must be arranged in ascending
// order of WEEK-NO. If they are not, results of any SEARCH ALL statement are
// unpredictable.

// p194: INDEXED BY phrase
// The INDEXED BY phrase specifies the indexes that can be used with a table. A
// table without an INDEXED BY phrase can be referred to through indexing by
// using an index-name associated with another table.
// For more information about using indexing, see “Subscripting using index-names
// (indexing)” on page 73.
// Indexes normally are allocated in static memory associated with the program that
// contains the table. Thus indexes are in the last-used state when a program is
// reentered. However, in the following cases, indexes are allocated on a
// per-invocation basis. Thus you must set the value of the index on every entry for
// indexes on tables in the following sections:
// - The LOCAL-STORAGE SECTION
// - The WORKING-STORAGE SECTION of a class definition (object instance
//   variables)
// - The LINKAGE SECTION of:
//   – Methods
//   – Programs compiled with the RECURSIVE clause
//   – Programs compiled with the THREAD option

occursClause:
	OCCURS (minNumberOfOccurences=integerValue TO)? (maxNumberOfOccurences=integerValue | UNBOUNDED) TIMES?
	(DEPENDING ON? varNumberOfOccurences=numericVariable1)?
	tableSortingKeys*
	(INDEXED BY? indexNameDefinition+)?;

tableSortingKeys: 
	(ASCENDING | DESCENDING) KEY? IS? dataNameReference+;
            
// p198: The PICTURE clause specifies the general characteristics and editing requirements
// of an elementary item.
// PICTURE or PIC
// The PICTURE clause must be specified for every elementary item except
// the following ones:
// - Index data items
// - The subject of the RENAMES clause
// - Items described with USAGE POINTER, USAGE FUNCTION-POINTER,
//   USAGE PROCEDURE-POINTER, or USAGE OBJECT REFERENCE
// - Internal floating-point data items
// In these cases, use of the PICTURE clause is prohibited.
// The PICTURE clause can be specified only at the elementary level.
// PIC is an abbreviation for PICTURE and has the same meaning.
// character-string
// character-string is made up of certain COBOL characters used as picture
// symbols. The allowable combinations determine the category of the
// elementary data item.

pictureClause:
    (PICTURE |PIC) IS? pictureCharacterString=alphanumericValue7;

// p199: character-string can contain a maximum of 50 characters.
// Symbols used in the PICTURE clause
// Any punctuation character that appears within the PICTURE character-string is not
// considered a punctuation character, but rather is a PICTURE character-string
// symbol.
// When specified in the SPECIAL-NAMES paragraph, DECIMAL-POINT IS
// COMMA exchanges the functions of the period and the comma in PICTURE
// character-strings and in numeric literals.
// The lowercase letters that correspond to the uppercase letters that represent the
// following PICTURE symbols are equivalent to their uppercase representations in a
// PICTURE character-string:
// A, B, E, G, N, P, S, V, X, Z, CR, DB
// All other lowercase letters are not equivalent to their corresponding uppercase
// representations.
// Table 12 (p199) defines the meaning of each PICTURE clause symbol. The heading Size
// indicates how the item is counted in determining the number of character positions
// in the item. The type of the character positions depends on the USAGE clause
// specified for the item, as follows:
// Usage | Type of character positions | Number of bytes per character
// DISPLAY | Alphanumeric | 1
// DISPLAY-1 | DBCS | 2
// NATIONAL | National | 2
// All others | Conceptual | Not applicable

// p199: ... Table 12. PICTURE clause symbol meanings ...

// p202: ... The following figure shows the sequences in which picture symbols can be
// specified to form picture character-strings. More detailed explanations of PICTURE
// clause symbols follow the figure ...

// p203: Character-string representation
// The topic lists symbols that can appear once or more than once in the PICTURE
// character-string.
// Symbols that can appear more than once
// The following symbols can appear more than once in one PICTURE
// character-string:
//    A B G N P X Z 9 0 / , + – * cs
// At least one of the symbols A, G, N, X, Z, 9, or *, or at least two of the
// symbols +, –, or cs must be present in a PICTURE string.
// An unsigned nonzero integer enclosed in parentheses immediately
// following any of these symbols specifies the number of consecutive
// occurrences of that symbol.
// Example: The following two PICTURE clause specifications are equivalent:
// PICTURE IS $99999.99CR
// PICTURE IS $9(5).9(2)CR
// Symbols that can appear only once
// The following symbols can appear only once in one PICTURE
// character-string:
//    E S V . CR DB
// Except for the PICTURE symbol V, each occurrence of any of the above
// symbols in a given PICTURE character-string represents an occurrence of
// that character or set of allowable characters in the data item.

// p204: ... Data categories and PICTURE rules ...
// ... many constraints on other clauses described until page 216 ...

// p204: The data categories are:
// - Alphabetic
//   Symbol : A
// - Numeric
//   Symbols : 9, P, S, and V
// - Numeric-edited
//   Symbols : B P V Z 9 0 / , . + - CR DB * cs
// - Alphanumeric
//   Symbols : A, X, and 9 
//   (a character-string containing all As or all 9s does not define an alphanumeric item.)
// - Alphanumeric-edited
//   Symbols : A X 9 B 0 /
//   (the string must contain at least one A or X, and at least one B or 0 (zero) or /)
// - DBCS
//   Symbols: G, G and B, or N
// - National
//   Symbol : N
// - National-edited
//   At least one symbol N, 
//   and at least one instance of one of these symbols: B 0 (zero) or / (slash)
// - External floating-point
//   Format : (+ | -) mantissa E (+ | -) exponent
//   The mantissa can contain the symbols: 9 . V
//   The exponent must consist of the symbol 99
// - Internal floating point
//   Note: Category internal floating point is defined by a USAGE clause that specifies
//   the COMP-1 or COMP-2 phrase.

// p205: There are several types of numeric items.
// The types are:
// - Binary
// - Packed decimal (internal decimal)
// - Zoned decimal (external decimal)
// - National decimal (external decimal)
// The type of a numeric item is defined by the usage clause as shown in the table
// below. Table 13. Numeric types p205.

// p210: ... PICTURE clause editing ...
// Types of editing are described in the following sections:
// - “Simple insertion editing” o, page 211
// - “Special insertion editing” on page 212
// - “Fixed insertion editing” on page 212
// - “Floating insertion editing” on page 213
// - “Zero suppression and replacement editing” on page 214

// => Lexer token type : PictureCharacterString
                     
// p216: The REDEFINES clause allows you to use different data description entries to
// describe the same computer storage area.
// When specified, the REDEFINES clause must be the first entry following
// data-name-1 or FILLER. If data-name-1 or FILLER is not specified, the REDEFINES
// clause must be the first entry following the level-number.
// data-name-1, FILLER
// Identifies an alternate description for the data area identified by
// data-name-2; data-name-1 is the redefining item or the REDEFINES subject.
// Neither data-name-1 nor any of its subordinate entries can contain a VALUE
// clause.
// data-name-2
// Identifies the redefined item or the REDEFINES object.
// The data description entry for data-name-2 can contain a REDEFINES
// clause.
// The data description entry for data-name-2 cannot contain an OCCURS
// clause. However, data-name-2 can be subordinate to an item whose data
// description entry contains an OCCURS clause; in this case, the reference to
// data-name-2 in the REDEFINES clause must not be subscripted.
// Neither data-name-1 nor data-name-2 can contain an OCCURS DEPENDING ON
// clause.
// data-name-1 and data-name-2 must have the same level in the hierarchy; however,
// the level numbers need not be the same. Neither data-name-1 nor data-name-2 can
// be defined with level number 66 or 88.
// data-name-1 and data-name-2 can each be described with any usage.
// Redefinition begins at data-name-1 and ends when a level-number less than or
// equal to that of data-name-1 is encountered. No entry that has a level-number
// numerically lower than those of data-name-1 and data-name-2 can occur between
// these entries.
// If the GLOBAL clause is used in the data description entry that contains the
// REDEFINES clause, only data-name-1 (the redefining item) possesses the global
// attribute. 
// The EXTERNAL clause must not be specified in the same data description entry as
// a REDEFINES clause.
// If the redefined data item (data-name-2) is declared to be an external data record,
// the size of the redefining data item (data-name-1) must not be greater than the size
// of the redefined data item. If the redefined data item is not declared to be an
// external data record, there is no such constraint.
// One or more redefinitions of the same storage area are permitted. The entries that
// give the new descriptions of the storage area must immediately follow the
// description of the redefined area without intervening entries that define new
// character positions. Multiple redefinitions can, but need not, all use the data-name
// of the original entry that defined this storage area. 
// Also, multiple redefinitions can use the name of the preceding definition.
// When more than one level-01 entry is written subordinate to an FD entry, a
// condition known as implicit redefinition occurs. That is, the second level-01 entry
// implicitly redefines the storage allotted for the first entry. In such level-01 entries,
// the REDEFINES clause must not be specified.
// When the data item implicitly redefines multiple 01-level records in a file
// description (FD) entry, items subordinate to the redefining or redefined item can
// contain an OCCURS DEPENDING ON clause.

// p216: ... more considerations on REDEFINES usage until page  ...

redefinesClause:
	REDEFINES dataNameReference;

// p219: The RENAMES clause specifies alternative and possibly overlapping groupings of
// elementary data items.
// The special level-number 66 must be specified for data description entries that
// contain the RENAMES clause.
// One or more RENAMES entries can be written for a logical record. All RENAMES
// entries associated with one logical record must immediately follow the last data
// description entry of that record.
// data-name-1
// Identifies an alternative grouping of data items.
// A level-66 entry cannot rename a level-01, level-77, level-88, or another
// level-66 entry.
// data-name-1 cannot be used as a qualifier; it can be qualified only by the
// names of level indicator entries or level-01 entries.
// data-name-2, data-name-3
// Identify the original grouping of elementary data items; that is, they must
// name elementary or group items within the associated level-01 entry and
// must not be the same data-name. Both data-names can be qualified.
// data-name-2 and data-name-3 can each reference any of the following items:
// - An elementary data item
// - An alphanumeric group item
// - A national group item
// When data-name-2 or data-name-3 references a national group item, the
// referenced item is processed as a group (not as an elementary data item of
// category national).
// The OCCURS clause must not be specified in the data entries for
// data-name-2 and data-name-3, or for any group entry to which they are
// subordinate. In addition, the OCCURS DEPENDING clause must not be
// specified for any item defined between data-name-2 and data-name-3.
// The keywords THROUGH and THRU are equivalent.
// When the THROUGH phrase is specified:
// - data-name-1 defines an alphanumeric group item that includes all the elementary
//   items that:
//   – Start with data-name-2 if it is an elementary item, or the first elementary item
//     within data-name-2 if it is a group item
//   – End with data-name-3 if it is an elementary item, or the last elementary item
//     within data-name-3 if it is an alphanumeric group item or national group item
// - The storage area occupied by the starting item through the ending item becomes
//   the storage area occupied by data-name-1.
// Usage note: The group defined with the THROUGH phrase can include data items
// of usage NATIONAL.
// The leftmost character position in data-name-3 must not precede the leftmost
// character position in data-name-2, and the rightmost character position in
// data-name-3 must not precede the rightmost character position in data-name-2. This
// means that data-name-3 cannot be totally subordinate to data-name-2.
// When the THROUGH phrase is not specified:
// - The storage area occupied by data-name-2 becomes the storage area occupied by
//   data-name-1.
// - All of the data attributes of data-name-2 become the data attributes for
//   data-name-1. That is:
//   – When data-name-2 is an alphanumeric group item, data-name-1 is an
//     alphanumeric group item.
//   – When data-name-2 is a national group item, data-name-1 is a national group
//     item.
//   – When data-name-2 is an elementary item, data-name-1 is an elementary item.

renamesClause:
	RENAMES (qualifiedDataName | dataNamesRange);

dataNamesRange: 
	startDataName=qualifiedDataName (THROUGH | THRU) endDataName=qualifiedDataName;

// p221: The SIGN clause specifies the position and mode of representation of the
// operational sign for the signed numeric item to which it applies.
// The SIGN clause is required only when an explicit description of the properties or
// position of the operational sign is necessary.
// The SIGN clause can be specified only for the following items:
// - An elementary numeric data item of usage DISPLAY or NATIONAL that is
//   described with an S in its picture character string, or
// - A group item that contains at least one such elementary entry as a subordinate
//   item
// When the SIGN clause is specified at the group level, that SIGN clause applies
// only to subordinate signed numeric elementary data items of usage DISPLAY or
// NATIONAL. Such a group can also contain items that are not affected by the SIGN
// clause. If the SIGN clause is specified for a group or elementary entry that is
// subordinate to a group item that has a SIGN clause, the SIGN clause for the
// subordinate entry takes precedence for that subordinate entry.
// The SIGN clause is treated as documentation for external floating-point items.
// When the SIGN clause is specified without the SEPARATE phrase, USAGE
// DISPLAY must be specified explicitly or implicitly. When SIGN IS SEPARATE is
// specified, either USAGE DISPLAY or USAGE NATIONAL can be specified.
// If you specify the CODE-SET clause in an FD entry, any signed numeric data
// description entries associated with that file description entry must be described
// with the SIGN IS SEPARATE clause.
// If the SEPARATE CHARACTER phrase is not specified, then:
// - The operational sign is presumed to be associated with the LEADING or
//   TRAILING digit position, whichever is specified, of the elementary numeric data
//   item. (In this instance, specification of SIGN IS TRAILING is the equivalent of
//   the standard action of the compiler.)
// - The character S in the PICTURE character string is not counted in determining
//   the size of the item (in terms of standard data format characters).
// If the SEPARATE CHARACTER phrase is specified, then:
// - The operational sign is presumed to be the LEADING or TRAILING character
//   position, whichever is specified, of the elementary numeric data item. This
//   character position is not a digit position.
// - The character S in the PICTURE character string is counted in determining the
//   size of the data item (in terms of standard data format characters).
// - + is the character used for the positive operational sign.
// - - is the character used for the negative operational sign.

signClause:
	(SIGN IS?)? (LEADING | TRAILING) (SEPARATE CHARACTER?)?;

// p223: The SYNCHRONIZED clause specifies the alignment of an elementary item on a
// natural boundary in storage.
// SYNC is an abbreviation for SYNCHRONIZED and has the same meaning.
// The SYNCHRONIZED clause is never required, but can improve performance on
// some systems for binary items used in arithmetic.
// The SYNCHRONIZED clause can be specified for elementary items and for
// level-01 group items, in which case every elementary item within the group item is
// synchronized.
// LEFT Specifies that the elementary item is to be positioned so that it will begin
// at the left character position of the natural boundary in which the
// elementary item is placed.
// RIGHT Specifies that the elementary item is to be positioned such that it will
// terminate on the right character position of the natural boundary in which
// it has been placed.
// When specified, the LEFT and the RIGHT phrases are syntax checked but have no
// effect on the execution of the program.
// The length of an elementary item is not affected by the SYNCHRONIZED clause.

// p223-228 : ... more details on the effect of the SYNCHRONIZE clause on other language elements ...

synchronizedClause:
	(SYNCHRONIZED | SYNC) (LEFT |RIGHT)?;

// p228: The USAGE clause specifies the format in which data is represented in storage.

// p229: Format 1
// NATIVE is treated as a comment in all phrases for which NATIVE is
// shown in the USAGE clause.
// The USAGE clause can be specified for a data description entry with any
// level-number other than 66 or 88.
// When specified at the group level, the USAGE clause applies to each elementary
// item in the group. The usage of elementary items must not contradict the usage of
// a group to which the elementary items belongs.
// A USAGE clause must not be specified in a group level entry for which a
// GROUP-USAGE NATIONAL clause is specified.
// When a GROUP-USAGE NATIONAL clause is specified or implied for a group
// level entry, USAGE NATIONAL must be specified or implied for every elementary
// item within the group. For details, see “GROUP-USAGE clause” on page 190.
// When the USAGE clause is not specified at either the group or elementary level, a
// usage clause is implied with:
// - Usage DISPLAY when the PICTURE clause contains only symbols other than G
//   or N
// - Usage NATIONAL when the PICTURE clause contains only one or more of the
//   symbol N and the NSYMBOL(NATIONAL) compiler option is in effect
// - Usage DISPLAY-1 when the PICTURE clause contains one or more of the symbol
//   N and the NSYMBOL(DBCS) compiler option is in effect

// p230-236 : ... more details on the effects of the USAGE clause ...
// p230: BINARY
// p231: PACKED-DECIMAL
// p231: COMPUTATIONAL or COMP (binary)
// p231: COMPUTATIONAL-1 or COMP-1 (floating-point)
// p231: COMPUTATIONAL-2 or COMP-2 (long floating-point)
// p231: COMPUTATIONAL-3 or COMP-3 (internal decimal)
// p231: COMPUTATIONAL-4 or COMP-4 (binary)
// p231: COMPUTATIONAL-5 or COMP-5 (native binary)
// p232: DISPLAY phrase
// p233: DISPLAY-1 phrase
// p233: FUNCTION-POINTER phrase
// p233: INDEX phrase
// p234: NATIONAL phrase
// p234: OBJECT REFERENCE phrase
// p235: POINTER phrase
// p236: PROCEDURE-POINTER phrase
// p236: NATIVE phrase

// p71: Index data item
// An index data item is a data item that can hold the value of an index.
// You define an index data item by specifying the USAGE IS INDEX clause in a data
// description entry. The name of an index data item is a data-name. An index data
// item can be used anywhere a data-name or identifier can be used, unless stated
// otherwise in the rules of a particular statement. You can use the SET statement to
// save the value of an index (referenced by index-name) in an index data item.

usageClause:
	(USAGE IS?)? ( (BINARY NATIVE?) |
					(COMP NATIVE?)   | (COMPUTATIONAL NATIVE?)   |
					(COMP_1 NATIVE?) | (COMPUTATIONAL_1 NATIVE?) |
					(COMP_2 NATIVE?) | (COMPUTATIONAL_2 NATIVE?) |
					(COMP_3 NATIVE?) | (COMPUTATIONAL_3 NATIVE?) |
					(COMP_4 NATIVE?) | (COMPUTATIONAL_4 NATIVE?) |
					(COMP_5 NATIVE?) | (COMPUTATIONAL_5 NATIVE?) |
					(DISPLAY NATIVE?)   |
					(DISPLAY_1 NATIVE?) |
					INDEX |
					(NATIONAL NATIVE?) |
					(OBJECT REFERENCE classNameReference?) |
					(PACKED_DECIMAL NATIVE?) |
					(POINTER |
					PROCEDURE_POINTER |
					FUNCTION_POINTER)                            
				);

// p237: The VALUE clause specifies the initial contents of a data item or the values
// associated with a condition-name. The use of the VALUE clause differs depending
// on the DATA DIVISION section in which it is specified.
// A VALUE clause that is used in the FILE SECTION or the LINKAGE SECTION in
// an entry other than a condition-name entry is syntax checked, but has no effect on
// the execution of the program.
// In the WORKING-STORAGE SECTION and the LOCAL-STORAGE SECTION, the
// VALUE clause can be used in condition-name entries or in specifying the initial
// value of any data item. The data item assumes the specified value at the beginning
// of program execution. If the initial value is not explicitly specified, the value is
// unpredictable.

// p237: Format 1: literal value
// Format 1 specifies the initial value of a data item. Initialization is independent of
// any BLANK WHEN ZERO or JUSTIFIED clause that is specified.
// A format-1 VALUE clause specified in a data description entry that contains or is
// subordinate to an OCCURS clause causes every occurrence of the associated data
// item to be assigned the specified value. Each structure that contains the
// DEPENDING ON phrase of the OCCURS clause is assumed to contain the
// maximum number of occurrences for the purposes of VALUE initialization.
// The VALUE clause must not be specified for a data description entry that contains
// or is subordinate to an entry that contains either an EXTERNAL or a REDEFINES
// clause. This rule does not apply to condition-name entries.
// A format-1 VALUE clause can be specified for an elementary data item or for a
// group item. When the VALUE clause is specified at the group level, the group area
// is initialized without consideration for the subordinate entries within the group. In
// addition, a VALUE clause must not be specified for subordinate entries within the
// group.
// For group items, the VALUE clause must not be specified if any subordinate
// entries contain a JUSTIFIED or SYNCHRONIZED clause.
// If the VALUE clause is specified for an alphanumeric group, all subordinate items
// must be explicitly or implicitly described with USAGE DISPLAY.
// The VALUE clause must not conflict with other clauses in the data description
// entry or in the data description of that entry's hierarchy.
// The functions of the editing characters in a PICTURE clause are ignored in
// determining the initial value of the item described. However, editing characters are
// included in determining the size of the item. Therefore, any editing characters
// must be included in the literal. For example, if the item is defined as PICTURE
// +999.99 and the value is to be +12.34, then the VALUE clause should be specified
// as VALUE "+012.34".
// A VALUE clause cannot be specified for external floating-point items.
// A data item cannot contain a VALUE clause if the prior data item contains an
// OCCURS clause with the DEPENDING ON phrase.

// p238 : ... more details - Rules for literal values ...

// p242: Format 3: NULL value
// This format assigns an invalid address as the initial value of an item defined as
// USAGE POINTER, USAGE PROCEDURE POINTER, or USAGE
// FUNCTION-POINTER. It also assigns an invalid object reference as the initial
// value of an item defined as USAGE OBJECT REFERENCE.
// VALUE IS NULL can be specified only for elementary items described implicitly or
// explicitly as USAGE POINTER, USAGE PROCEDURE-POINTER, USAGE
// FUNCTION-POINTER, or USAGE OBJECT REFERENCE.

valueClause:
	VALUE IS? value2;
			   
// p239: Format 2: condition-name value
// This format associates a value, values, or ranges of values with a condition-name.
// Each such condition-name requires a separate level-88 entry. Level-number 88 and
// the condition-name are not part of the format-2 VALUE clause itself. They are
// included in the format only for clarity.
// condition-name-1
// A user-specified name that associates a value with a conditional variable. If
// the associated conditional variable requires subscripts or indexes, each
// procedural reference to the condition-name must be subscripted or indexed
// as required for the conditional variable.
// Condition-names are tested procedurally in condition-name conditions (see
// “Conditional expressions” on page 256).
// literal-1
// Associates the condition-name with a single value.
// The class of literal-1 must be a valid class for assignment to the associated
// conditional variable.
// literal-1 THROUGH literal-2
// Associates the condition-name with at least one range of values. When the
// THROUGH phrase is used, literal-1 must be less than literal-2. For details,
// see “Rules for condition-name entries.”
// literal-1 and literal-2 must be of the same class. The class of literal-1 and
// literal-2 must be a valid class for assignment to the associated conditional
// variable.
// When literal-1 and literal-2 are DBCS literals, the range of DBCS values
// specified by the THROUGH phrase is based on the binary collating
// sequence of the hexadecimal values of the DBCS characters.
// When literal-1 and literal-2 are national literals, the range of national
// character values specified by the THROUGH phrase is based on the binary
// collating sequence of the hexadecimal values of the national characters
// represented by the literals.
// If the associated conditional variable is of class DBCS, literal-1 and literal-2
// must be DBCS literals. The figurative constant SPACE or the figurative
// constant ALL DBCS-literal can be specified.
// If the associated conditional variable is of class national, literal-1 and
// literal-2 must be either both national literals or both alphanumeric literals
// for a given condition-name. The figurative constants ZERO, SPACE,
// QUOTE, HIGH-VALUE, LOW-VALUE, symbolic-character, ALL
// national-literal, or ALL literal can be specified.

// p240: ... more details - Rules for condition-name entries ...

valueClauseForCondition:
	((VALUE IS?) | (VALUES ARE?)) (value1 | valuesRange)+; 

valuesRange: 
	startValue=value1 (THROUGH | THRU) endValue=value1;

// p245: The PROCEDURE DIVISION is an optional division.
// Program procedure division
// The program procedure division consists of optional declaratives, and
// procedures that contain sections, paragraphs, sentences, and statements.
// Factory procedure division
// The factory procedure division contains only factory method definitions.
// Object procedure division
// The object procedure division contains only object method definitions.
// Method procedure division
// A method procedure division consists of optional declaratives, and
// procedures that contain sections, paragraphs, sentences, and statements. A
// method can INVOKE other methods, be recursively invoked, and issue a
// CALL to a program. A method procedure division cannot contain nested
// programs or methods.

// p247: The PROCEDURE DIVISION header
// The PROCEDURE DIVISION, if specified, is identified by one of the following
// headers, depending on whether you are specifying a program, a factory definition,
// an object definition, or a method definition.
// The following syntax diagram shows the format for a PROCEDURE DIVISION
// header in a program.

// p247: Format: program procedure division header
// p248: Format: method procedure division header
// -> BY REFERENCE not supported in using phrase

procedureDivisionHeader: PROCEDURE DIVISION usingPhrase? returningPhrase? PeriodSeparator;
usingPhrase: USING programInputParameters+;
returningPhrase: RETURNING programOutputParameter;
	
// p248: The USING phrase specifies the parameters that a program or method receives
// when the program is called or the method is invoked.
// The USING phrase is valid in the PROCEDURE DIVISION header of a called
// subprogram or invoked method entered at the beginning of the nondeclaratives
// portion. Each USING identifier must be defined as a level-01 or level-77 item in the
// LINKAGE SECTION of the called subprogram or invoked method.
// In a called subprogram entered at the first executable statement following an
// ENTRY statement, the USING phrase is valid in the ENTRY statement. Each
// USING identifier must be defined as a level-01 or level-77 item in the LINKAGE
// SECTION of the called subprogram.
// However, a data item specified in the USING phrase of the CALL statement can be
// a data item of any level in the DATA DIVISION of the calling COBOL program or
// method. A data item specified in the USING phrase of an INVOKE statement can
// be a data item of any level in the DATA DIVISION of the invoking COBOL
// program or method.
// A data item in the USING phrase of the header can have a REDEFINES clause in
// its data description entry.
// It is possible to call COBOL programs from non-COBOL programs or to pass user
// parameters from a system command to a COBOL main program. COBOL methods
// can be invoked only from Java or COBOL.
// The order of appearance of USING identifiers in both calling and called
// subprograms, or invoking methods or programs and invoked methods, determines
// the correspondence of single sets of data available to both. The correspondence is
// positional and not by name. For calling and called subprograms, corresponding
// identifiers must contain the same number of bytes although their data descriptions
// need not be the same.
// For index-names, no correspondence is established. Index-names in calling and
// called programs, or invoking method or program and invoked methods, always
// refer to separate indexes.
// The identifiers specified in a CALL USING or INVOKE USING statement name the
// data items available to the calling program or invoking method or program that
// can be referred to in the called program or invoked method. These items can be
// defined in any DATA DIVISION section.
// A given identifier can appear more than once in a USING phrase. The last value
// passed to it by a CALL or INVOKE statement is used.
// The BY REFERENCE or BY VALUE phrase applies to all parameters that follow
// until overridden by another BY REFERENCE or BY VALUE phrase.
// BY REFERENCE (for programs only)
// When an argument is passed BY CONTENT or BY REFERENCE, BY
// REFERENCE must be specified or implied for the corresponding formal
// parameter on the PROCEDURE or ENTRY USING phrase.
// BY REFERENCE is the default if neither BY REFERENCE nor BY VALUE is
// specified.
// If the reference to the corresponding data item in the CALL statement
// declares the parameter to be passed BY REFERENCE (explicit or implicit),
// the program executes as if each reference to a USING identifier in the
// called subprogram is replaced by a reference to the corresponding USING
// identifier in the calling program.
// If the reference to the corresponding data item in the CALL statement
// declares the parameter to be passed BY CONTENT, the value of the item is
// moved when the CALL statement is executed and placed into a
// system-defined storage item that possesses the attributes declared in the
// LINKAGE SECTION for data-name-1. The data description of each
// parameter in the BY CONTENT phrase of the CALL statement must be the
// same, meaning no conversion or extension or truncation, as the data
// description of the corresponding parameter in the USING phrase of the
// header.
// BY VALUE
// When an argument is passed BY VALUE, the value of the argument is
// passed, not a reference to the sending data item. The receiving subprogram
// or method has access only to a temporary copy of the sending data item.
// Any modifications made to the formal parameters that correspond to an
// argument passed BY VALUE do not affect the argument.
// Parameters specified in the USING phrase of a method procedure division
// header must be passed to the method BY VALUE. See Passing data in the
// Enterprise COBOL Programming Guide for examples that illustrate these
// concepts.
// data-name-1
// data-name-1 must be a level-01 or level-77 item in the LINKAGE SECTION.
// When data-name-1 is an object reference in a method procedure division
// header, an explicit class-name must be specified in the data description
// entry for that object reference; that is, data-name-1 must not be a universal
// object reference.
// For methods, the parameter data types are restricted to the data types that
// are interoperable between COBOL and Java, as listed in “Interoperable
// data types for COBOL and Java” on page 361.

programInputParameters:
    (BY? (REFERENCE | VALUE))? sharedStorageArea2+;

// p250: The RETURNING phrase specifies a data item that is to receive the program or
// method result.
// data-name-2
// data-name-2 is the RETURNING data item. data-name-2 must be a level-01
// or level-77 item in the LINKAGE SECTION.
// Note: An unbounded group cannot be specified as data-name-2.
// In a method procedure division header, the data type of data-name-2 must
// be one of the types supported for Java interoperation, as listed in
// “Interoperable data types for COBOL and Java” on page 361.
// The RETURNING data item is an output-only parameter. On entry to the
// method, the initial state of the RETURNING data item has an undefined
// and unpredictable value. You must initialize the PROCEDURE DIVISION
// RETURNING data item before you reference its value. The value that is
// returned to the invoking routine is the value that the data item has at the
// point of exit from the method. See “RETURNING phrase” on page 359 for
// further details on conformance requirements for the INVOKE RETURNING
// identifier and the method RETURNING data item.
// Do not use the PROCEDURE DIVISION RETURNING phrase in:
// - Programs that contain the ENTRY statement.
// - Nested programs.
// - Main programs: Results of specifying PROCEDURE DIVISION
// RETURNING on a main program are undefined. You should specify the
// PROCEDURE DIVISION RETURNING phrase only on called
// subprograms. For main programs, use the RETURN-CODE special
// register to return a value to the operating environment.

programOutputParameter:
	sharedStorageArea2;

// p246: Format: procedure division
// 1 The USE statement is described under “USE statement” on page 546.
// 2 Section-name can be omitted. If you omit section-name, paragraph-name can be omitted.
// 3 Priority-numbers are not valid for methods, recursive programs, or programs compiled with the
//   THREAD option.

// p246: Requirements for a method procedure division
// There are specific requirements when you code a method procedure division.
// The requirements are:
// - You can use the EXIT METHOD statement or the GOBACK statement to return
//   control to the invoking method or program. An implicit EXIT METHOD
//   statement is generated as the last statement of every method procedure division.
// For details on the EXIT METHOD statement, see “EXIT METHOD statement” on
// page 336.
// - You can use the STOP RUN statement (which terminates the run unit) in a
//   method.
// - You can use the RETURN-CODE special register within a method procedure
//   division to access return codes from subprograms that are called with the CALL
//   statement, but the RETURN-CODE value is not returned to the invoker of the
//   current method. Use the procedure division RETURNING data name to return a
//   value to the invoker of the current method. For details, see the discussion of
//   RETURNING data-name-2 under “The PROCEDURE DIVISION header.”
// You cannot specify the following statements or clauses in a method procedure
// division:
// - ALTER
// - ENTRY
// - EXIT PROGRAM
// - GO TO without a specified procedure name
// - SEGMENT-LIMIT
// - USE FOR DEBUGGING

// p251: Declaratives provide one or more special-purpose sections that are executed when
// an exceptional condition occurs.
// When declarative sections are specified, they must be grouped at the beginning of
// the procedure division and the entire PROCEDURE DIVISION must be divided
// into sections.
// Each declarative section starts with a USE statement that identifies the section's
// function. The series of procedures that follow specify the actions that are to be
// taken when the exceptional condition occurs. Each declarative section ends with
// another section-name followed by a USE statement, or with the keywords END
// DECLARATIVES.
// The entire group of declarative sections is preceded by the keyword
// DECLARATIVES written on the line after the PROCEDURE DIVISION header. The
// group is followed by the keywords END DECLARATIVES. The keywords
// DECLARATIVES and END DECLARATIVES must each begin in Area A and be
// followed by a separator period. No other text can appear on the same line.
// In the declaratives part of the PROCEDURE DIVISION, each section header must
// be followed by a separator period, and must be followed by a USE statement
// followed by a separator period. No other text can appear on the same line.
// The USE statement has three formats, discussed in these sections:
// - “EXCEPTION/ERROR declarative” on page 547
// - “DEBUGGING declarative” on page 549
// The USE statement itself is never executed; instead, the USE statement defines the
// conditions that execute the succeeding procedural paragraphs, which specify the
// actions to be taken. After the procedure is executed, control is returned to the
// routine that activated it.
// A declarative procedure can be performed from a nondeclarative procedure.
// A nondeclarative procedure can be performed from a declarative procedure.
// A declarative procedure can be referenced in a GO TO statement in a declarative
// procedure.
// A nondeclarative procedure can be referenced in a GO TO statement in a
// declarative procedure.
// You can include a statement that executes a previously called USE procedure that
// is still in control. However, to avoid an infinite loop, you must be sure there is an
// eventual exit at the bottom.
// The declarative procedure is exited when the last statement in the procedure is
// executed.

declarativesHeader:
    DECLARATIVES PeriodSeparator;

declarativesEnd:
    END DECLARATIVES PeriodSeparator;

// p546 : The USE statement defines the conditions under which the procedures that follow
// the statement will be executed.
// The formats for the USE statement are:
// - EXCEPTION/ERROR declarative
// - DEBUGGING declarative

useStatement:
    useStatementForExceptionDeclarative |
    useStatementForDebuggingDeclarative;

// p547: EXCEPTION/ERROR declarative
// The EXCEPTION/ERROR declarative specifies procedures for input/output
// exception or error handling that are to be executed in addition to the standard
// system procedures.
// The words EXCEPTION and ERROR are synonymous and can be used
// interchangeably.

// p547: Format 1: USE statement for EXCEPTION/ERROR declarative
// file-name-1
// Valid for all files. When this option is specified, the procedure is executed
// only for the files named. No file-name can refer to a sort or merge file. For
// any given file, only one EXCEPTION/ERROR procedure can be specified;
// thus, file-name specification must not cause simultaneous requests for
// execution of more than one EXCEPTION/ERROR procedure.
// A USE AFTER EXCEPTION/ERROR declarative statement specifying the
// name of a file takes precedence over a declarative statement specifying the
// open mode of the file.
// INPUT
// Valid for all files. When this option is specified, the procedure is executed
// for all files opened in INPUT mode or in the process of being opened in
// INPUT mode that get an error.
// OUTPUT
// Valid for all files. When this option is specified, the procedure is executed
// for all files opened in OUTPUT mode or in the process of being opened in
// OUTPUT mode that get an error.
// I-O 
// Valid for all direct-access files. When this option is specified, the procedure
// is executed for all files opened in I-O mode or in the process of being
// opened in I-O mode that get an error.
// EXTEND
// Valid for all files. When this option is specified, the procedure is executed
// for all files opened in EXTEND mode or in the process of being opened in
// EXTEND mode that get an error.
//
// The EXCEPTION/ERROR procedure is executed:
// - Either after completing the system-defined input/output error routine, or
// - Upon recognition of an INVALID KEY or AT END condition when an INVALID
//   KEY or AT END phrase has not been specified in the input/output statement, or
// - Upon recognition of an IBM-defined condition that causes file status key 1 to be
//   set to 9. (See “File status key” on page 287.)
// After execution of the EXCEPTION/ERROR procedure, control is returned to the
// invoking routine in the input/output control system. If the input/output status
// value does not indicate a critical input/output error, the input/output control
// system returns control to the next executable statement following the input/output
// statement whose execution caused the exception.
// An applicable EXCEPTION/ERROR procedure is activated when an input/output
// error occurs during execution of a READ, WRITE, REWRITE, START, OPEN,
// CLOSE, or DELETE statement. To determine what conditions are errors, see
// “Common processing facilities” on page 286.

useStatementForExceptionDeclarative:
	USE GLOBAL? AFTER STANDARD? (EXCEPTION | ERROR) PROCEDURE ON? (fileNameReference+ | (INPUT | OUTPUT | I_O | EXTEND)) PeriodSeparator;

// p548: ... more rules that appky to declarative procedures until page 549 ...

// p549: DEBUGGING declarative
// Debugging sections are permitted only in the outermost program; they are not
// valid in nested programs. Debugging sections are never triggered by procedures
// contained in nested programs.
// Debugging sections are not permitted in:
// - A method
// - A program defined with the recursive attribute
// - A program compiled with the THREAD compiler option
// The WITH DEBUGGING MODE clause of the SOURCE-COMPUTER paragraph
// activates all debugging sections and lines that have been compiled into the object
// code. See Appendix D, “Source language debugging,” on page 577 for additional
// details.
// When the debugging mode is suppressed by not specifying the WITH
// DEBUGGING MODE clause, all USE FOR DEBUGGING declarative procedures
// and all debugging lines are inhibited.
// Automatic execution of a debugging section is not caused by a statement that
// appears in a debugging section.

// p549: Format 2: USE statement for DEBUGGING declarative
// USE FOR DEBUGGING
// All debugging statements must be written together in a section
// immediately after the DECLARATIVES header.
// Except for the USE FOR DEBUGGING sentence itself, within the
// debugging procedure there must be no reference to any nondeclarative
// procedures.
// procedure-name-1
// Must not be defined in a debugging session.
// Table 55 shows, for each valid option, the points during execution when
// the USE FOR DEBUGGING procedures are executed.
// Any given procedure-name can appear in only one USE FOR
// DEBUGGING sentence, and only once in that sentence. All procedures
// must appear in the outermost program.
// ALL PROCEDURES
// procedure-name-1 must not be specified in any USE FOR DEBUGGING
// sentences. The ALL PROCEDURES phrase can be specified only once in a
// program. Only the procedures contained in the outermost program will
// trigger execution of the debugging section.
// Table 55. Execution of debugging declaratives
// USE FOR DEBUGGING operand 
// => Upon execution of the following, the USE FOR DEBUGGING
// procedures are executed immediately
// procedure-name-1 
// => Before each execution of the named procedure
// After the execution of an ALTER statement referring to the named
// procedure
// ALL PROCEDURES 
// => Before each execution of each nondebugging procedure in the
// outermost program
// After the execution of each ALTER statement in the outermost
// program (except ALTER statements in declarative procedures) 

useStatementForDebuggingDeclarative:
	USE FOR? DEBUGGING ON? (procedureName+ | (ALL PROCEDURES)) PeriodSeparator;

// p252: Procedures
// Within the PROCEDURE DIVISION, a procedure consists of a section or a group of
// sections, and a paragraph or group of paragraphs.
// A procedure-name is a user-defined name that identifies a section or a paragraph.

// p252: Section
// A section-header optionally followed by one or more paragraphs.
// Section-header
// A section-name followed by the keyword SECTION, optionally
// followed by a priority-number, followed by a separator period.
// Section-headers are optional after the keywords END
// DECLARATIVES or if there are no declaratives.
// A section ends immediately before the next section header, or at the end of
// the PROCEDURE DIVISION, or, in the declaratives portion, at the
// keywords END DECLARATIVES.

sectionHeader:
    sectionNameDefinition SECTION priorityNumber? PeriodSeparator;

// p252: Segments
// A segment consists of all sections in a program that have the same
// priority-number. Priority-number determines whether a section is stored in
// a fixed segment or an independent segment at run time.
// Segments with a priority-number of 0 through 49 are fixed segments.
// Segments with a priority-number of 50 through 99 are independent
// segments.
// The type of segment (fixed or independent) controls the segmentation
// feature.
// In fixed segments, procedures are always in last-used state. In independent
// segments, procedures are in initial state each time the segment receives
// control from a segment with a different priority-number, except when the
// transfer of control results from the execution of a GOBACK or EXIT
// PROGRAM statement. Restrictions on the use of ALTER, SORT, and
// MERGE statements in independent segments are described under those
// statements.
// Enterprise COBOL does not support the overlay feature of the Standard
// COBOL 85 segmentation module.

// p253: Paragraph
// A paragraph-name followed by a separator period, optionally followed by
// one or more sentences.
// Paragraphs must be preceded by a period because paragraphs always
// follow either the IDENTIFICATION DIVISION header, a section, or
// another paragraph, all of which must end with a period.
// A paragraph ends immediately before the next paragraph-name or section
// header, or at the end of the PROCEDURE DIVISION, or, in the declaratives
// portion, at the keywords END DECLARATIVES.
// Paragraphs need not all be contained within sections, even if one or more
// paragraphs are so contained.

paragraphHeader:
    paragraphNameDefinition PeriodSeparator;

// Sentence
// One or more statements terminated by a separator period.

sentenceEnd:
    PeriodSeparator;

// p253: Statement
// A syntactically valid combination of identifiers and symbols (literals,
// relational-operators, and so forth) beginning with a COBOL verb.
// Execution begins with the first statement in the PROCEDURE DIVISION,
// excluding declaratives. Statements are executed in the order in which they are
// presented for compilation, unless the statement rules dictate some other order of
// execution.
// The end of the PROCEDURE DIVISION is indicated by one of the following items:
// - An IDENTIFICATION DIVISION header that indicates the start of a nested
//   source program
// - An END PROGRAM, END METHOD, END FACTORY, or END OBJECT marker
// - The physical end of a program; that is, the physical position in a source program
//   after which no further source program lines occur 

// p276: Statement categories
// There are four categories of COBOL statements: imperative statements, conditional
// statements, delimited scope statements and compiler-directing statements. See the
// links below for more details.

// p276: Imperative statements
// An imperative statement either specifies an unconditional action to be taken by the
// program, or is a conditional statement terminated by its explicit scope terminator.
// A series of imperative statements can be specified wherever an imperative
// statement is allowed. A conditional statement that is terminated by its explicit
// scope terminator is also classified as an imperative statement.
// For more information about explicit scope terminator, see “Delimited scope
// statements” on page 280).
                   
// p278: Conditional statements
// A conditional statement specifies that the truth value of a condition is to be
// determined and that the subsequent action of the object program is dependent on
// this truth value.
// For more information about conditional expressions, see “Conditional expressions”
// on page 256.)

// p280: Delimited scope statements
// In general, a DELIMITED SCOPE statement uses an explicit scope terminator to
// turn a conditional statement into an imperative statement.
// The resulting imperative statement can then be nested. Explicit scope terminators
// can also be used to terminate the scope of an imperative statement. Explicit scope
// terminators are provided for all COBOL statements that can have conditional
// phrases.
// Unless explicitly specified otherwise, a delimited scope statement can be specified
// wherever an imperative statement is allowed by the rules of the language.
// Explicit scope terminators
// An explicit scope terminator marks the end of certain PROCEDURE DIVISION
// statements.
// A conditional statement that is delimited by its explicit scope terminator is
// considered an imperative statement and must follow the rules for imperative
// statements.

// p281: There are several phrases common to arithmetic and data manipulation statements,
// such as:
// - CORRESPONDING phrase
// - GIVING phrase
// - ROUNDED phrase
// - SIZE ERROR phrases

// p281: CORRESPONDING phrase
// The CORRESPONDING (CORR) phrase causes ADD, SUBTRACT, and MOVE
// operations to be performed on elementary data items of the same name if the
// alphanumeric group item or national group item to which they belong is specified.
// A national group is processed as a group item when the CORRESPONDING
// phrase is used.
// Both identifiers that follow the keyword CORRESPONDING must name group
// items. In this discussion, these identifiers are referred to as identifier-1 and
// identifier-2. identifier-1 references the sending group item. identifier-2 references the
// receiving group item.
// Two subordinate data items, one from identifier-1 and one from identifier-2,
// correspond if the following conditions are true:
// - In an ADD or SUBTRACT statement, both of the data items are elementary
//   numeric data items. Other data items are ignored.
// - In a MOVE statement, at least one of the data items is an elementary item, and
//   the move is permitted by the move rules.
// - The two subordinate items have the same name and the same qualifiers up to
//   but not including identifier-1 and identifier-2.
// -  The subordinate items are not identified by the keyword FILLER.
// - Neither identifier-1 nor identifier-2 is described as a level 66, 77, or 88 item, and
//   neither is described as an index data item. Neither identifier-1 nor identifier-2 can
//   be reference-modified.
// - Neither identifier-1 nor identifier-2 is described with USAGE POINTER, USAGE
//   FUNCTION-POINTER, USAGE PROCEDURE-POINTER, or USAGE OBJECT
//   REFERENCE.
// - The subordinate items do not include a REDEFINES, RENAMES, OCCURS,
//   USAGE INDEX, USAGE POINTER, USAGE PROCEDURE-POINTER, USAGE
//   FUNCTION-POINTER, or USAGE OBJECT REFERENCE clause in their
//   descriptions.
//   However, identifier-1 and identifier-2 themselves can contain or be subordinate to
//   items that contain a REDEFINES or OCCURS clause in their descriptions.
// - The name of each subordinate data item that satisfies these conditions is unique
//   after application of implicit qualifiers.
// identifier-1, identifier-2, or both can be subordinate to a FILLER item.
// For example, consider two data hierarchies defined as follows:
// 05 ITEM-1 OCCURS 6.
//  10 ITEM-A PIC S9(3).
//  10 ITEM-B PIC +99.9.
//  10 ITEM-C PIC X(4).
//  10 ITEM-D REDEFINES ITEM-C PIC 9(4).
//  10 ITEM-E USAGE COMP-1.
//  10 ITEM-F USAGE INDEX.
// 05 ITEM-2.
//  10 ITEM-A PIC 99.
//  10 ITEM-B PIC +9V9.
//  10 ITEM-C PIC A(4).
//  10 ITEM-D PIC 9(4).
//  10 ITEM-E PIC 9(9) USAGE COMP.
//  10 ITEM-F USAGE INDEX.
// If ADD CORR ITEM-2 TO ITEM-1(x) is specified, ITEM-A and ITEM-A(x), ITEM-B and
// ITEM-B(x), and ITEM-E and ITEM-E(x) are considered to be corresponding and are
// added together. ITEM-C and ITEM-C(x) are not included because they are not
// numeric. ITEM-D and ITEM-D(x) are not included because ITEM-D(x) includes a
// REDEFINES clause in its data description. ITEM-F and ITEM-F(x) are not included
// because they are index data items. Note that ITEM-1 is valid as either identifier-1 or
// identifier-2.
// If any of the individual operations in the ADD CORRESPONDING statement
// produces a size error condition, imperative-statement-1 in the ON SIZE ERROR
// phrase is not executed until all of the individual additions are completed. 

// p282: GIVING phrase
// The value of the identifier that follows the word GIVING is set equal to the
// calculated result of the arithmetic operation. Because this identifier is not involved
// in the computation, it can be a numeric-edited item.

// p282: ROUNDED phrase
// After decimal point alignment, the number of places in the fraction of the result of
// an arithmetic operation is compared with the number of places provided for the
// fraction of the resultant identifier.
// When the size of the fractional result exceeds the number of places provided for its
// storage, truncation occurs unless ROUNDED is specified. When ROUNDED is
// specified, the least significant digit of the resultant identifier is increased by 1
// whenever the most significant digit of the excess is greater than or equal to 5.
// When the resultant identifier is described by a PICTURE clause that contains
// rightmost Ps and when the number of places in the calculated result exceeds the
// number of integer positions specified, rounding or truncation occurs relative to the
// rightmost integer position for which storage is allocated.
// In a floating-point arithmetic operation, the ROUNDED phrase has no effect; the
// result of a floating-point operation is always rounded. For more information on
// floating-point arithmetic expressions, see Fixed-point contrasted with floating-point
// arithmetic in the Enterprise COBOL Programming Guide.
// When the ARITH(EXTEND) compiler option is in effect, the ROUNDED phrase is
// not supported for arithmetic receivers with 31 digit positions to the right of the
// decimal point. For example, neither X nor Y below is valid as a receiver with the
// ROUNDED phrase:
// 01 X PIC V31.
// 01 Y PIC P(30)9(1).
// . . .
// COMPUTE X ROUNDED = A + B
// COMPUTE Y ROUNDED = A - B
// Otherwise, the ROUNDED phrase is fully supported for extended-precision
// arithmetic statements.

// p283: SIZE ERROR phrases
// A size error condition can occur in different ways.
// These are the four ways a size error condition can occur:
// - When the absolute value of the result of an arithmetic evaluation, after decimal
//   point alignment, exceeds the largest value that can be contained in the result
//   field.
// - When division by zero occurs.
// - In an exponential expression, as indicated in the following table:
// Table 30. Exponentiation size error conditions
// ... see p283 ...
// The size error condition applies only to final results, not to any intermediate
// results.
// If the resultant identifier is defined with usage BINARY, COMPUTATIONAL,
// COMPUTATIONAL-4, or COMPUTATIONAL-5, the largest value that the resultant
// data item can contain is the value implied by the item's decimal PICTURE
// character-string, regardless of the TRUNC compiler option in effect.
// If the ROUNDED phrase is specified, rounding takes place before size error
// checking.
// When a size error occurs, the subsequent action of the program depends on
// whether the ON SIZE ERROR phrase is specified.
// If the ON SIZE ERROR phrase is not specified and a size error condition occurs,
// truncation rules apply and the value of the affected resultant identifier is
// computed.
// If the ON SIZE ERROR phrase is specified and a size error condition occurs, the
// value of the resultant identifier affected by the size error is not altered; that is, the
// error results are not placed in the receiving identifier. After completion of the
// execution of the arithmetic operation, the imperative statement in the ON SIZE
// ERROR phrase is executed, control is transferred to the end of the arithmetic
// statement, and the NOT ON SIZE ERROR phrase, if specified, is ignored.
// For ADD CORRESPONDING and SUBTRACT CORRESPONDING statements, if
// an individual arithmetic operation causes a size error condition, the ON SIZE
// ERROR imperative statement is not executed until all the individual additions or
// subtractions have been completed.
// If the NOT ON SIZE ERROR phrase has been specified and, after execution of an
// arithmetic operation, a size error condition does not exist, the NOT ON SIZE
// ERROR phrase is executed.
// When both the ON SIZE ERROR and NOT ON SIZE ERROR phrases are specified,
// and the statement in the phrase that is executed does not contain any explicit
// transfer of control, then if necessary an implicit transfer of control is made after
// execution of the phrase to the end of the arithmetic statement. 

// -- Individual statements --

// p294: ACCEPT statement
// The ACCEPT statement transfers data or system date-related information into the data area referenced by the specified identifier. 
// There is no editing or error checking of the incoming data. 
// * Data transfer 
// Format 1 transfers data from an input source into the data item referenced by identifier-1 (the receiving area). 
// When the FROM phrase is omitted, the system input device is assumed.
// Format 1 is useful for exceptional situations in a program when operator intervention (to supply a given message, code, or exception indicator) is required. 
// The operator must of course be supplied with the appropriate messages with which to reply. 
// identifier-1 
// The receiving area. Can be: 
// - An alphanumeric group item 
// - A national group item 
// - An elementary data item of usage DISPLAY, DISPLAY-1, or NATIONAL 
// A national group item is processed an elementary data item of category national. 
// mnemonic-name-1 
// Specifies the input device. mnemonic-name-1 must be associated in the SPECIAL-NAMES paragraph with an environment-name. See “SPECIAL-NAMES paragraph” on page 112. 
// - System input device 
// The length of a data transfer is the same as the length of the record on the input device, with a maximum of 32,760 bytes. 
// The system input device is read until the receiving area is filled or EOF is encountered. 
// If the length of the receiving area is not an even multiple of the system input device record length, the final record will be truncated as required. 
// If EOF is encountered after data has been moved and before the receiving area has been filled, the receiving area is padded with spaces of the appropriate representation for the receiving area. 
// If EOF is encountered before any data has been moved to the receiving area, padding will not take place and the contents of the receiving area are unchanged.
// Each input record is concatenated with the previous input record. 
// If the input record is of a fixed-length format, the entire input record is used. 
// No editing is performed to remove trailing or leading blanks.
// If the input record is of the variable-length format, the actual record length is used to determine the amount of data received. 
// With variable-format records, the Record Definition Word (RDW) is removed from the beginning of the input record. Only the actual input data is transferred to identifier-1. 
// If the data item referenced by identifier-1 is of usage national, data is transferred without conversion and without checking for validity. The input data is assumed to be in UTF-16 format. 
// - Console 
// 1. A system-generated message code is automatically displayed, followed by the literal AWAITING REPLY. The maximum length of an input message is 114 characters. 
// 2. Execution is suspended. 
// 3. After the message code (the same code as in item 1) is entered from the console and recognized by the system, ACCEPT statement execution is resumed. The message is moved to the receiving area and left-justified regardless of its PICTURE clause. 
// If identifier-1 references a data item of usage NATIONAL, the message is converted from the native code page representation to national character representation. The native code page is the one that was specified by the CODEPAGE compiler option when the source code was compiled. 
// The ACCEPT statement is terminated if any of the following occurs: 
// – No data is received from the console; for example, if the operator hits the Enter key. 
// – The receiving data item is filled with data. 
// – Fewer than 114 characters of data are entered. If 114 bytes of data are entered and the receiving area is still not filled with data, more requests for data are issued to the console. If more than 114 characters of data are entered, only the first 114 characters will be recognized by the system. 
// If the receiving area is longer than the incoming message, the rightmost characters are padded with spaces of the appropriate representation for the receiving area. If the incoming message is longer than the receiving area, the character positions beyond the length of the receiving area are truncated.
// For information about obtaining ACCEPT input from a z/OS UNIX file or stdin, see Assigning input from a screen or file (ACCEPT) in the Enterprise COBOL Programming Guide. 
// environment-name 
// Identifies the source of input data. An environment-name from the names given in Table 5 on page 114 can be specified.
// If the device is the same as that used for READ statements for a LINE SEQUENTIAL file, results are unpredictable. 
// * System date-related information transfer 
// System information contained in the specified conceptual data items DATE, DATE YYYYMMDD, DAY, DAY YYYYDDD, DAY-OF-WEEK, or TIME, can be transferred
// into the data item referenced by identifier-2. The transfer must follow the rules for the MOVE statement without the CORRESPONDING phrase.
// For more information, see “MOVE statement” on page 369.
// identifier-2 The receiving area. Can be: v An alphanumeric group item v A national group item v An elementary data item of one of the following categories: – alphanumeric – alphanumeric-edited – numeric-edited (with usage DISPLAY or NATIONAL) – national – national-edited – numeric – internal floating-point – external floating-point (with usage DISPLAY or NATIONAL) A national group item is processed an an elementary data item of category national.
// Format 2 accesses the current date in two formats: the day of the week or the time of day as carried by the system (which can be useful in identifying when a particular run of an object program was executed). 
// You can also use format 2 to supply the date in headings and footings.
// The current date and time can also be accessed with the intrinsic function CURRENT-DATE, which also supports four-digit year values and provides additional information (see “CURRENT-DATE” on page 490). 
// DATE, DATE YYYYMMDD, DAY, DAY YYYYDDD, DAY-OF-WEEK, and TIME 
// The conceptual data items DATE, DATE YYYYMMDD, DAY, DAY YYYYDDD, DAY-OF-WEEK, and TIME implicitly have USAGE DISPLAY. 
// Because these are conceptual data items, they cannot be described in the COBOL program.
// The content of the conceptual data items is moved to the receiving area using the rules of the MOVE statement. If the receiving area is of usage NATIONAL, the data is converted to national character representation.
// DATE
// Has the implicit PICTURE 9(6). The sequence of data elements (from left to right) is: Two digits for the year Two digits for the month Two digits for the day
// Thus 27 April 2003 is expressed as 030427. 
// DATE YYYYMMDD 
// Has the implicit PICTURE 9(8). The sequence of data elements (from left to right) is: Four digits for the year Two digits for the month Two digits for the day
// Thus 27 April 2003 is expressed as 20030427. 
// DAY 
// Has the implicit PICTURE 9(5). The sequence of data elements (from left to right) is: Two digits for the year Three digits for the day
// Thus 27 April 2003 is expressed as 03117. 
// DAY YYYYDDD 
// Has the implicit PICTURE 9(7). The sequence of data elements (from left to right) is: Four digits for the year Three digits for the day
// Thus 27 April 2003 is expressed as 2003117. 
// DAY-OF-WEEK Has the implicit PICTURE 9(1). The single data element represents the day of the week according to the following values: 1 represents Monday 5 represents Friday 2 represents Tuesday 6 represents Saturday 3 represents Wednesday 7 represents Sunday 4 represents Thursday
// Thus Wednesday is expressed as 3. 
// TIME Has the implicit PICTURE 9(8). The sequence of data elements (from left to right) is: Two digits for hour of day Two digits for minute of hour Two digits for second of minute Two digits for hundredths of second
// Thus 2:41 PM is expressed as 14410000.

acceptStatement: 
	acceptDataTransfer | acceptSystemDateTime;

acceptDataTransfer:
	ACCEPT alphanumericStorageArea (FROM mnemonicForEnvironmentNameReferenceOrEnvironmentName)?;

acceptSystemDateTime:
	ACCEPT alphanumericStorageArea FROM ((DATE YYYYMMDD?) | (DAY YYYYDDD?) | DAY_OF_WEEK | TIME);

// p298: ADD statement
// The ADD statement sums two or more numeric operands and stores the result.

addStatement:
	addSimple | addGiving | addCorresponding;

addStatementEnd: END_ADD;

// For all formats:
// identifier-1, identifier-2 
// In format 1, must name an elementary numeric item. 
// In format 2, must name an elementary numeric item except when following the word GIVING. Each identifier that follows the word GIVING must name an elementary numeric or numeric-edited item.
// In format 3, must name an alphanumeric group item or national group item. 
// literal 
// Must be a numeric literal. Floating-point data items and literals can be used anywhere that a numeric data item or literal can be specified.
// When the ARITH(COMPAT) compiler option is in effect, the composite of operands can contain a maximum of 30 digits.
// When the ARITH(EXTEND) compiler option is in effect, the composite of operands can contain a maximum of 31 digits. 
// For more information, see “Arithmetic statement operands” on page 284 and the details on arithmetic intermediate results in Appendix A. Intermediate results and arithmetic precision in the Enterprise COBOL Programming Guide. 
		
// p298: Format 1: ADD statement
// All identifiers or literals that precede the keyword TO are added together, and this sum is added to and stored in identifier-2. 
// This process is repeated for each successive occurrence of identifier-2 in the left-to-right order in which identifier-2 is specified.

addSimple:
	ADD numericVariable3+ TO numericStorageAreaRounded+;

// p299: Format 2: ADD statement with GIVING phrase
// The values of the operands that precede the word GIVING are added together, and the sum is stored as the new value of each data item referenced by identifier-3.

addGiving:
	ADD numericVariable3+ TO? toOperand=numericVariable3 GIVING numericStorageAreaRounded+;

// p299: Format 3: ADD statement with CORRESPONDING phrase
// Elementary data items within identifier-1 are added to and stored in the corresponding elementary items within identifier-2.

addCorresponding:
	ADD (CORRESPONDING | CORR) groupItem=dataItemReference TO toGroupItem=dataItemReference ROUNDED?;

// ROUNDED phrase
// For formats 1, 2, and 3, see “ROUNDED phrase” on page 282. 
numericStorageAreaRounded:
	numericStorageArea ROUNDED?;

// p301: ALTER statement
// The ALTER statement changes the transfer point specified in a GO TO statement.
// The ALTER statement encourages the use of unstructured programming practices; the EVALUATE statement provides the same function as the ALTER statement but helps to ensure that a program is well-structured.
// The ALTER statement modifies the GO TO statement in the paragraph named by procedure-name-1. Subsequent executions of the modified GO TO statement transfer control to procedure-name-2. 
// procedure-name-1 Must name a PROCEDURE DIVISION paragraph that contains only one sentence: a GO TO statement without the DEPENDING ON phrase. 
// procedure-name-2 Must name a PROCEDURE DIVISION section or paragraph.
// Before the ALTER statement is executed, when control reaches the paragraph specified in procedure-name-1, the GO TO statement transfers control to the paragraph specified in the GO TO statement. 
// After execution of the ALTER statement however, the next time control reaches the paragraph specified in procedure-name-1, the GO TO statement transfers control to the paragraph specified in procedure-name-2.
// The ALTER statement acts as a program switch, allowing, for example, one sequence of execution during initialization and another sequence during the bulk of file processing.
// Altered GO TO statements in programs with the INITIAL attribute are returned to their initial states each time the program is entered.
// Do not use the ALTER statement in programs that have the RECURSIVE attribute, in methods, or in programs compiled with the THREAD option. 
// Segmentation considerations 
// A GO TO statement that is coded in an independent segment must not be referenced by an ALTER statement in a segment with a different priority-number. 
// All other uses of the ALTER statement are valid and are performed even if the GO TO referenced by the ALTER statement is in a fixed segment.
// Altered GO TO statements in independent segments are returned to their initial state when control is transferred to the independent segment that contains the ALTERED GO TO from another independent segment with a different priority-number.
// This transfer of control can take place because of: 
// - The effect of previous statements 
// - An explicit transfer of control with a PERFORM or GO TO statement 
// - A sort or merge statement with the INPUT or OUTPUT phrase specified

alterStatement:
	ALTER (alteredProcedure=procedureName TO (PROCEED TO)? targetProcedure=procedureName)+;

// p303: CALL statement
// The CALL statement transfers control from one object program to another within the run unit.
// The program containing the CALL statement is the calling program; the program identified in the CALL statement is the called subprogram. 
// Called programs can contain CALL statements; however, only programs defined with the RECURSIVE clause can execute a CALL statement that directly or indirectly calls itself.
// identifier-1, literal-1 
// literal-1 must be an alphanumeric literal. 
// identifier-1 must be an alphanumeric, alphabetic, or numeric data item described with USAGE DISPLAY such that its value can be a program-name. 
// The rules of formation for program-names are dependent on the PGMNAME compiler option. For details, see the discussion of program-names in “PROGRAM-ID paragraph” on page 100 and also the description of PGMNAME in the Enterprise COBOL Programming Guide. 
// Usage note: Do not specify the name of a class or method in the CALL statement. 
// When the called subprogram is to be entered at the beginning of the PROCEDURE DIVISION, literal-1 or the contents of identifier-1 must specify the program-name of the called subprogram.
// When the called subprogram is entered through an ENTRY statement, literal-1 or the contents of identifier-1 must be the same as the name specified in the called subprogram's ENTRY statement.
// For information about how the compiler resolves calls to program-names found in multiple programs, see “Conventions for program-names” on page 86. 
// USING phrase
// The USING phrase specifies arguments that are passed to the target program.
// Include the USING phrase in the CALL statement only if there is a USING phrase in the PROCEDURE DIVISION header or the ENTRY statement through which the called program is run. 
// The number of operands in each USING phrase must be identical.
// For more information about the USING phrase, see “The PROCEDURE DIVISION header” on page 247.
// The sequence of the operands in the USING phrase of the CALL statement and in the corresponding USING phrase in the called subprogram's PROCEDURE DIVISION header or ENTRY statement determines the correspondence between the operands used by the calling and called programs. This correspondence is positional.
// The values of the parameters referenced in the USING phrase of the CALL statement are made available to the called subprogram at the time the CALL statement is executed. 
// The description of the data items in the called program must describe the same number of character positions as the description of the corresponding data items in the calling program.
// The BY CONTENT, BY REFERENCE, and BY VALUE phrases apply to parameters that follow them until another BY CONTENT, BY REFERENCE, or BY VALUE phrase is encountered. 
// BY REFERENCE is assumed if you do not specify a BY CONTENT, BY REFERENCE, or BY VALUE phrase prior to the first parameter. 
// BY REFERENCE phrase
// If the BY REFERENCE phrase is either specified or implied for a parameter, the corresponding data item in the calling program occupies the same storage area as the data item in the called program. 
// identifier-2 
// Can be any data item of any level in the DATA DIVISION. identifier-2 cannot be a function-identifier. 
// If it is defined in the LINKAGE SECTION or FILE SECTION, you must have already provided addressability for identifier-2 prior to invocation of the CALL statement. You can do this by coding one of the following items: 
// - SET ADDRESS OF identifier-2 TO pointer 
// - PROCEDURE DIVISION USING 
// - ENTRY USING 
// file-name-1
// A file-name for a QSAM file. See Passing data in the Enterprise COBOL Programming Guide for details on using file-name with the CALL statement. 
// ADDRESS OF identifier-2
// identifier-2 must be a level-01 or level-77 item defined in the LINKAGE SECTION. 
// OMITTED Indicates that no argument is passed.
// BY CONTENT phrase
// If the BY CONTENT phrase is specified or implied for a parameter, the called program cannot change the value of this parameter as referenced in the CALL statement's USING phrase, though the called program can change the value of the data item referenced by the corresponding data-name in the called program's PROCEDURE DIVISION header. Changes to the parameter in the called program do not affect the corresponding argument in the calling program. 
// identifier-3 
// Can be any data item of any level in the DATA DIVISION. identifier-3 cannot be a function identifier or an unbounded group.
// If defined in the LINKAGE SECTION or FILE SECTION, you must have already provided addressability for identifier-3 prior to invocation of the CALL statement. You can do this by coding one of the following phrases: 
// - SET ADDRESS OF identifier-3 TO pointer 
// - PROCEDURE DIVISION USING 
// - ENTRY USING 
// literal-2 Can be: 
// - An alphanumeric literal 
// - A figurative constant (except ALL literal or NULL/NULLS)
// - A DBCS literal
// - A national literal 
// LENGTH OF special register 
// For information about the LENGTH OF special register, see “LENGTH OF” on page 19.
// ADDRESS OF identifier-3 
// identifier-3 must be a data item of any level except 66 or 88 defined in the LINKAGE SECTION, the WORKING-STORAGE SECTION, or the LOCAL-STORAGE SECTION. 
// OMITTED Indicates that no argument is passed.
// For alphanumeric literals, the called subprogram should describe the parameter as PIC X(n) USAGE DISPLAY, where n is the number of characters in the literal.
// For DBCS literals, the called subprogram should describe the parameter as PIC G(n) USAGE DISPLAY-1, orPIC N(n) with implicit or explicit USAGE DISPLAY-1, where n is the length of the literal.
// For national literals, the called subprogram should describe the parameter as PIC N(n) with implicit or explicit USAGE NATIONAL, where n is the length of the literal.
// BY VALUE phrase
// The BY VALUE phrase applies to all arguments that follow until overridden by another BY REFERENCE or BY CONTENT phrase.
// If the BY VALUE phrase is specified or implied for an argument, the value of the argument is passed, not a reference to the sending data item. The called program can modify the formal parameter that corresponds to the BY VALUE argument, but any such changes do not affect the argument because the called program has access to a temporary copy of the sending data item.
// Although BY VALUE arguments are primarily intended for communication with non-COBOL programs (such as C), they can also be used for COBOL-to-COBOL invocations. 
// In this case, BY VALUE must be specified or implied for both the argument in the CALL USING phrase and the corresponding formal parameter in the PROCEDURE DIVISION USING phrase. 
// identifier-4 
// Must be an elementary data item in the DATA DIVISION. It must be one of the following items: 
// - Binary (USAGE BINARY, COMP, COMP-4, or COMP-5) 
// - Floating point (USAGE COMP-1 or COMP-2)
// - Function-pointer (USAGE FUNCTION-POINTER) 
// - Pointer (USAGE POINTER) 
// - Procedure-pointer (USAGE PROCEDURE-POINTER) 
// - Object reference (USAGE OBJECT REFERENCE) 
// - One single-byte alphanumeric character (such as PIC X or PIC A) 
// - One national character (PIC N), described as an elementary data item of category national. 
// The following can also be passed BY VALUE:
// - Reference-modified item of usage display and length 1 
// - Reference-modified item of usage national and length 1
// - SHIFT-IN and SHIFT-OUT special registers 
// - LINAGE-COUNTER special register when it is usage binary 
// ADDRESS OF identifier-4 
// identifier-4 must be a data item of any level except 66 or 88 defined in the LINKAGE SECTION, the WORKING-STORAGE SECTION, or the LOCAL-STORAGE SECTION. 
// LENGTH OF special register A LENGTH OF special register passed BY VALUE is treated as a PIC 9(9) binary. For information about the LENGTH OF special register, see “LENGTH OF” on page 19. 
// literal-3 
// Must be of one of the following types: 
// - A numeric literal 
// - A figurative constant ZERO 
// - A one-character alphanumeric literal 
// - A one-character national literal 
// - A symbolic character
// - A single-byte figurative constant – SPACE – QUOTE – HIGH-VALUE – LOW-VALUE 
//   ZERO is treated as a numeric value; a fullword binary zero is passed. 
// If literal-3 is a fixed-point numeric literal, it must have a precision of nine or fewer digits. In this case, a fullword binary representation of the literal value is passed. 
// If literal-3 is a floating-point numeric literal, an 8-byte internal floating-point (COMP-2) representation of the value is passed. 
// literal-3 must not be a DBCS literal. 
// RETURNING phrase 
// identifier-5
// The RETURNING data item, which can be any data item defined in the DATA DIVISION. 
// The return value of the called program is implicitly stored into identifier-5.
// You can specify the RETURNING phrase for calls to functions written in COBOL, C, or in other programming languages that use C linkage conventions. 
// If you specify the RETURNING phrase on a CALL to a COBOL subprogram: 
// - The called subprogram must specify the RETURNING phrase on its PROCEDURE DIVISION header.
// - identifier-5 and the corresponding PROCEDURE DIVISION RETURNING identifier in the target program must have the same PICTURE, USAGE, SIGN, SYNCHRONIZE, JUSTIFIED, and BLANK WHEN ZERO clauses (except that PICTURE clause currency symbols can differ, and periods and commas can be interchanged due to the DECIMAL POINT IS COMMA clause).
// When the target returns, its return value is assigned to identifier-5 using the rules for the SET statement if identifier-6 is of usage INDEX, POINTER, FUNCTION-POINTER, PROCEDURE-POINTER, or OBJECT REFERENCE. 
// When identifier-5 is of any other usage, the rules for the MOVE statement are used.
// The CALL ... RETURNING data item is an output-only parameter. 
// On entry to the called program, the initial state of the PROCEDURE DIVISION RETURNING data item has an undefined and unpredictable value. You must initialize the PROCEDURE DIVISION RETURNING data item in the called program before you reference its value. 
// The value that is passed back to the calling program is the final value of the PROCEDURE DIVISION RETURNING data item when the called program returns.
// Note: If a COBOL program returns a doubleword binary item via a PROCEDURE DIVISION RETURNING header to a calling COBOL program with a CALL ... RETURNING statement, an issue occurs if only one of the programs is recompiled with Enterprise COBOL V5. Both the called and calling programs must be recompiled with Enterprise COBOL V5 together, so that the linkage convention for the RETURNING item is consistent.
// If an EXCEPTION or OVERFLOW occurs, identifier-5 is not changed.
// identifier-5 must not be reference-modified.
// The RETURN-CODE special register is not set by execution of CALL statements that include the RETURNING phrase. 
// ON EXCEPTION phrase
// An exception condition occurs when the called subprogram cannot be made available. At that time, one of the following two actions will occur: 
// 1. If the ON EXCEPTION phrase is specified, control is transferred to imperative-statement-1. Execution then continues according to the rules for each statement specified in imperative-statement-1. If a procedure branching or conditional statement that causes explicit transfer of control is executed, control is transferred in accordance with the rules for that statement. Otherwise, upon completion of the execution of imperative-statement-1, control is transferred to the end of the CALL statement and the NOT ON EXCEPTION phrase, if specified, is ignored. 
// 2. If the ON EXCEPTION phrase is not specified in the CALL statement, the NOT ON EXCEPTION phrase, if specified, is ignored. 
// NOT ON EXCEPTION phrase
// If an exception condition does not occur (that is, the called subprogram can be made available), control is transferred to the called program. After control is returned from the called program, control is transferred to:
// -  imperative-statement-2, if the NOT ON EXCEPTION phrase is specified. 
// - The end of the CALL statement in any other case. (If the ON EXCEPTION phrase is specified, it is ignored.)
// If control is transferred to imperative-statement-2, execution continues according to the rules for each statement specified in imperative-statement-2. If a procedure branching or conditional statement that causes explicit transfer of control is executed, control is transferred in accordance with the rules for that statement.
// Otherwise, upon completion of the execution of imperative-statement-2, control is transferred to the end of the CALL statement. 
// ON OVERFLOW phrase
// The ON OVERFLOW phrase has the same effect as the ON EXCEPTION phrase. 
// END-CALL phrase
// This explicit scope terminator serves to delimit the scope of the CALL statement. END-CALL permits a conditional CALL statement to be nested in another conditional statement. END-CALL can also be used with an imperative CALL statement.
// For more information, see “Delimited scope statements” on page 280.
// SUMMARY :
// * by reference : the address of the data is passed in the arguments list
// * by value : the compiler builds a copy of the data in the arguments list, but only floating point, binary, or single byte data item may be passed by value
// * by content : the address of a copy of the data item is passed, it looks the same as passing by reference for the called subroutine, but but any change is not reflected back to the caller

callStatement:
	CALL programNameOrProgramEntryOrProcedurePointerOrFunctionPointerVariable 
		(USING callUsingParameters+)?
		(RETURNING callReturningParameter)?;

callUsingParameters:
	(BY? (REFERENCE | CONTENT | VALUE))? variableOrFileNameOrOmitted+;

variableOrFileNameOrOmitted: 
	sharedVariableOrFileName | OMITTED;

callReturningParameter:
	sharedStorageArea1;

callStatementEnd: END_CALL;

// p305: procedure-pointer-1 
// Must be defined with USAGE IS PROCEDURE-POINTER and must be set to a valid program entry point; otherwise, the results of the CALL statement are undefined. 
// After a program has been canceled by COBOL, released by PL/I or C, or deleted by assembler, any procedure-pointers that had been set to that program's entry point are no longer valid. 
// => replaced by programNameFromDataOrProgramEntryFromDataOrProcedurePointerOrFunctionPointer

// p305: function-pointer-1 
// Must be defined with USAGE IS FUNCTION-POINTER and must be set to a valid function or program entry point; otherwise, the results of the CALL statement are undefined. 
// After a program has been canceled by COBOL, released by PL/I or C, or deleted by the assembler, any function-pointers that had been set to that function or program's entry point are no longer valid.
// => replaced by programNameFromDataOrProgramEntryFromDataOrProcedurePointerOrFunctionPointer

// p311: CANCEL statement
// The CANCEL statement ensures that the referenced subprogram is entered in initial state the next time that it is called.
// identifier-1, literal-1 
// literal-1 must be an alphanumeric literal. 
// identifier-1 must be an alphanumeric, alphabetic, or zoned decimal data item such that its value can be a program-name.
// The rules of formation for program-names are dependent on the PGMNAME compiler option. For details, see the discussion of program-names in “PROGRAM-ID paragraph” on page 100 and the description of PGMNAME in the Enterprise COBOL Programming Guide. 
// literal-1 or the contents of identifier-1 must be the same as a literal or the contents of an identifier specified in an associated CALL statement.
// Do not specify the name of a class or a method in the CANCEL statement.
// After a CANCEL statement for a called subprogram has been executed, that subprogram no longer has a logical connection to the program. 
// The contents of data items in external data records described by the subprogram are not changed when that subprogram is canceled.
// If a CALL statement is executed later by any program in the run unit naming the same subprogram, that subprogram is entered in its initial state.
// When a CANCEL statement is executed, all programs contained within the program referenced in the CANCEL statement are also canceled. The result is the same as if a valid CANCEL were executed for each contained program in the reverse order in which the programs appear in the separately compiled program.
// A CANCEL statement closes all open files that are associated with an internal file connector in the program named in an explicit CANCEL statement.
// USE procedures associated with those files are not executed.
// You can cancel a called subprogram in any of the following ways: 
// - By referencing it as the operand of a CANCEL statement 
// - By terminating the run unit of which the subprogram is a member
// - By executing an EXIT PROGRAM statement or a GOBACK statement in the called subprogram if that subprogram possesses the initial attribute
// No action is taken when a CANCEL statement is executed if the specified program: 
// - Has not been dynamically called in this run unit by another COBOL program 
// - Has been called and subsequently canceled
// In a multithreaded environment, a program cannot execute a CANCEL statement naming a program that is active on any thread. The named program must be completely inactive.
// Called subprograms can contain CANCEL statements. However, a called subprogram must not execute a CANCEL statement that directly or indirectly cancels the calling program itself or that cancels any program higher than itself in the calling hierarchy. In such a case, the run unit is terminated.
// A program named in a CANCEL statement must be a program that has been called and has executed an EXIT PROGRAM statement or a GOBACK statement.
// A program can cancel a program that it did not call, provided that, in the calling hierarchy, the program that executes the CANCEL statement is higher than or equal to the program it is canceling. 
// For example: A calls B and B calls C (When A receives control, it can cancel C.) A calls B and A calls C (When C receives control, it can cancel B.)

cancelStatement:
	CANCEL programNameVariable+;

// p313: CLOSE statement
// The CLOSE statement terminates the processing of volumes and files.
// Format 1: CLOSE statement for sequential files
// Format 3: CLOSE statement for line-sequential files
// Notes: 1 The REEL, UNIT, and NO REWIND phrases are not valid for VSAM files.
// Format 2: CLOSE statement for indexed and relative file
// file-name-1 Designates the file upon which the CLOSE statement is to operate. 
// If more than one file-name is specified, the files need not have the same organization or access. 
// file-name-1 must not be a sort or merge file. 
// REEL and UNIT 
// You can specify these phrases only for QSAM multivolume or single volume files. The terms REEL and UNIT are interchangeable. 
// WITH NO REWIND and FOR REMOVAL
// These phrases apply only to QSAM tape files. If they are specified for storage devices to which they do not apply, the close operation is successful and a status key value is set to indicate the file was on a non-reel medium.
// A CLOSE statement can be executed only for a file in an open mode. 
// After successful execution of a CLOSE statement (without the REEL/UNIT phrase if using format 1): 
// - The record area associated with the file-name is no longer available. Unsuccessful execution of a CLOSE statement leaves availability of the record data undefined.
// - An OPEN statement for the file must be executed before any other input/output statement can be executed for the file and before data is moved to a record description entry associated with the file.
// If the FILE STATUS clause is specified in the file-control entry, the associated file status key is updated when the CLOSE statement is executed.
// If the file is in an open status and the execution of a CLOSE statement is unsuccessful, the EXCEPTION/ERROR procedure (if specified) for this file is executed. 
// ... p314->p315 : more details on the Effect of CLOSE statement on file types  / The permissible combinations of CLOSE statement phrases ...

closeStatement:
	CLOSE closeFileDirective+;

closeFileDirective: 
	fileNameReference ( ( (REEL | UNIT) ((FOR? REMOVAL) | (WITH NO REWIND))? ) | ( WITH? (LOCK | (NO REWIND)) ) )?;

// p317: COMPUTE statement
// The COMPUTE statement assigns the value of an arithmetic expression to one or more data items.
// With the COMPUTE statement, arithmetic operations can be combined without the restrictions on receiving data items imposed by the rules for the ADD, SUBTRACT, MULTIPLY, and DIVIDE statements.
// When arithmetic operations are combined, the COMPUTE statement can be more efficient than the separate arithmetic statements written in a series.
// identifier-1 
// Must name an elementary numeric item or an elementary numeric-edited item. Can name an elementary floating-point data item. 
// arithmetic-expression 
// Can be any arithmetic expression, as defined in “Arithmetic expressions” on page 253. 
// When the COMPUTE statement is executed, the value of arithmetic expression is calculated and stored as the new value of each data item referenced by identifier-1. 
// An arithmetic expression consisting of a single identifier, numeric function, or literal allows the user to set the value of the data items that are referenced by identifier-1 equal to the value of that identifier, function, or literal. 
// ROUNDED phrase
// For a discussion of the ROUNDED phrase, see “ROUNDED phrase” on page 282.
// SIZE ERROR phrases
// For a discussion of the SIZE ERROR phrases, see “SIZE ERROR phrases” on page 283. 
// END-COMPUTE phrase
// This explicit scope terminator serves to delimit the scope of the COMPUTE statement. END-COMPUTE permits a conditional COMPUTE statement to be nested in another conditional statement. END-COMPUTE can also be used with an imperative COMPUTE statement.
// For more information, see “Delimited scope statements” on page 280.

computeStatement:
	COMPUTE numericStorageAreaRounded+ (EqualOperator | EQUAL) arithmeticExpression;

computeStatementEnd: END_COMPUTE;

// p319: CONTINUE statement
// The CONTINUE statement is a no operation statement. CONTINUE indicates that no executable instruction is present.

continueStatement:
	CONTINUE;

// p320: DELETE statement
// The DELETE statement removes a record from an indexed or relative file. 
// For indexed files, the key can then be reused for record addition.
// For relative files, the space is then available for a new record with the same RELATIVE KEY value.
// When the DELETE statement is executed, the associated file must be open in I-O mode.
// file-name-1
// Must be defined in an FD entry in the DATA DIVISION and must be the name of an indexed or relative file.
//After successful execution of a DELETE statement, the record is removed from the file and can no longer be accessed.
//Execution of the DELETE statement does not affect the contents of the record area associated with file-name-1 or the content of the data item referenced by the data-name specified in the DEPENDING ON phrase of the RECORD clause associated with file-name-1.
//If the FILE STATUS clause is specified in the file-control entry, the associated file status key is updated when the DELETE statement is executed.
//The file position indicator is not affected by execution of the DELETE statement. Sequential access mode
//For a file in sequential access mode, the previous input/output statement must be a successfully executed READ statement. When the DELETE statement is executed, the system removes the record that was retrieved by that READ statement.
//For a file in sequential access mode, the INVALID KEY and NOT INVALID KEY phrases must not be specified. An EXCEPTION/ERROR procedure can be specified.
//Random or dynamic access mode
//In random or dynamic access mode, DELETE statement execution results depend on the file organization: indexed or relative.
//When the DELETE statement is executed, the system removes the record identified by the contents of the prime RECORD KEY data item for indexed files, or the RELATIVE KEY data item for relative files. If the file does not contain such a record, an INVALID KEY condition exists. (See “Invalid key condition” on page 290.)
//Both the INVALID KEY phrase and an applicable EXCEPTION/ERROR procedure can be omitted.
//Transfer of control after the successful execution of a DELETE statement, with the NOT INVALID KEY phrase specified, is to the imperative statement associated with the phrase. END-DELETE phrase
//This explicit scope terminator serves to delimit the scope of the DELETE statement. END-DELETE permits a conditional DELETE statement to be nested in another conditional statement. END-DELETE can also be used with an imperative DELETE statement.
//For more information, see “Delimited scope statements” on page 280.

deleteStatement:
	DELETE fileNameReference RECORD?;

deleteStatementEnd: END_DELETE;

// p322: DISPLAY statement
// DISPLAY statement transfers the contents of each operand to the output device. 
// The contents are displayed on the output device in the order, left to right, in which the operands are listed.
// identifier-1 
// Identifier-1 references the data that is to be displayed. 
// Identifier-1 can reference any data item except an item of usage PROCEDURE-POINTER, FUNCTION-POINTER, OBJECT REFERENCE, or INDEX. Identifier-1 cannot be an index-name.
// If identifier-1 is a binary, internal decimal, or internal floating-point data item, identifier-1 is converted automatically to external format as follows: 
// - Binary and internal decimal items are converted to zoned decimal. Negative signed values cause a low-order sign overpunch. 
// - Internal floating-point numbers are converted to external floating-point numbers for display such that: 
//    – A COMP-1 item will display as if it had an external floating-point PICTURE clause of -.9(8)E-99. 
//    – A COMP-2 item will display as if it had an external floating-point PICTURE clause of -.9(17)E-99. 
// Data items defined with USAGE POINTER are converted to a zoned decimal number that has an implicit PICTURE clause of PIC 9(10). 
// If the output is directed to CONSOLE, data items described with usage NATIONAL are converted from national character representation to EBCDIC. The conversion uses the EBCDIC code page that was specified in the CODEPAGE compiler option when the source code was compiled. National characters without EBCDIC counterparts are converted to default substitution characters; no exception condition is indicated or raised. 
// If the output is not directed to CONSOLE, data items described with usage NATIONAL are written without conversion and without data validation. 
// No other categories of data require conversion.
// DBCS data items, explicitly or implicitly defined as USAGE DISPLAY-1, are transferred to the sending field of the output device. For proper results, the output device must have the capability to recognize DBCS shift-out and shift-in control characters. Both DBCS and non-DBCS operands can be specified in a single DISPLAY statement. 
// literal-1 
// Can be any literal or any figurative constant as specified in “Figurative constants” on page 13. 
// When a figurative constant is specified, only a single occurrence of that figurative constant is displayed. 
// UPON 
// environment-name-1 or the environment name associated with mnemonic-name-1 must be associated with an output device. See “SPECIAL-NAMES paragraph” on page 112. 
// A default logical record size is assumed for each device, as follows: 
// - The system logical output device 120 characters 
// - The system punch device 80 characters
// - The console 100 characters
// A maximum logical record size is allowed for each device, as follows: 
// - The system logical output device 255 characters 
// - The system punch device 255 characters 
// - The console 100 characters
// On the system punch device, the last eight characters are used for PROGRAM-ID name. 
// When the UPON phrase is omitted, the system's logical output device is assumed. The list of valid environment-names in a DISPLAY statement is shown in Table 5 on page 114. 
// For details on routing DISPLAY output to stdout, see Displaying values on a screen or in a file (DISPLAY) in the Enterprise COBOL Programming Guide. 
// WITH NO ADVANCING 
// When specified, the positioning of the output device will not be changed in any way following the display of the last operand.
// If the WITH NO ADVANCING phrase is not specified, after the last operand has been transferred to the output device, the positioning of the output device will be reset to the leftmost position of the next line of the device.
// Enterprise COBOL does not support output devices that are capable of positioning to a specific character position. See Displaying values on a screen or in a file (DISPLAY) in the Enterprise COBOL Programming Guide for more information about the DISPLAY statement.
// The DISPLAY statement transfers the data in the sending field to the output device. The size of the sending field is the total byte count of all operands listed. 
// If the output device is capable of receiving data of the same size as the data item being transferred, then the data item is transferred. 
// If the output device is not capable of receiving data of the same size as the data item being transferred, then one of the following applies: 
// - If the total count is less than the device maximum, the remaining rightmost positions are padded with spaces. 
// - If the total count exceeds the maximum, as many records are written as are needed to display all operands. Any operand being printed or displayed when the end of a record is reached is continued in the next record.
// ... more details on DBCS operands p324 ...

displayStatement:
    DISPLAY variable4+ uponOutputDevice? withNoAdvancing?;

uponOutputDevice:
	UPON outputDevice=mnemonicForEnvironmentNameReferenceOrEnvironmentName;

withNoAdvancing:
	WITH? NO ADVANCING;
	
// p325: DIVIDE statement
// The DIVIDE statement divides one numeric data item into or by others and sets the values of data items equal to the quotient and remainder.
// Format 1: DIVIDE statement
// In format 1, the value of identifier-1 or literal-1 is divided into the value of identifier-2, and the quotient is then stored in identifier-2. 
// For each successive occurrence of identifier-2, the division takes place in the left-to-right order in which identifier-2 is specified.
//
// Format 2: DIVIDE statement with INTO and GIVING phrases
// In format 2, the value of identifier-1 or literal-1 is divided into the value of identifier-2 or literal-2.
// The value of the quotient is stored in each data item referenced by identifier-3.
// 
// Format 3: DIVIDE statement with BY and GIVING phrase
// In format 3, the value of identifier-1 or literal-1 is divided by the value of identifier-2 or literal-2. 
// The value of the quotient is stored in each data item referenced by identifier-3.
//
// Format 4: DIVIDE statement with INTO and REMAINDER phrases
// In format 4, the value of identifier-1 or literal-1 is divided into identifier-2 or literal-2. 
// The value of the quotient is stored in identifier-3, and the value of the remainder is stored in identifier-4.
// 
// Format 5: DIVIDE statement with BY and REMAINDER phrase
// In format 5, the value of identifier-1 or literal-1 is divided by identifier-2 or literal-2.
// The value of the quotient is stored in identifier-3, and the value of the remainder is stored in identifier-4.
// 
// For all formats: 
// identifier-1, identifier-2 
// Must name an elementary numeric data item.
// identifier-3, identifier-4 
// Must name an elementary numeric or numeric-edited item. 
// literal-1, literal-2
// Must be a numeric literal.
// In formats 1, 2, and 3, floating-point data items and literals can be used anywhere that a numeric data item or literal can be specified.
// In formats 4 and 5, floating-point data items or literals cannot be used. 
// ROUNDED phrase
// For formats 1, 2, and 3, see “ROUNDED phrase” on page 282.
// For formats 4 and 5, the quotient used to calculate the remainder is in an intermediate field. The value of the intermediate field is truncated rather than rounded. 
// REMAINDER phrase
// The result of subtracting the product of the quotient and the divisor from the dividend is stored in identifier-4. Ifidentifier-3, the quotient, is a numeric-edited item, the quotient used to calculate the remainder is an intermediate field that contains the unedited quotient.
// The REMAINDER phrase is invalid if the receiver or any of the operands is a floating-point item.
// Any subscripts for identifier-4 in the REMAINDER phrase are evaluated after the result of the divide operation is stored in identifier-3 of the GIVING phrase. 
// SIZE ERROR phrases
// For formats 1, 2, and 3, see “SIZE ERROR phrases” on page 283.
// For formats 4 and 5, if a size error occurs in the quotient, no remainder calculation is meaningful. Therefore, the contents of the quotient field (identifier-3) and the remainder field (identifier-4) are unchanged.
// If size error occurs in the remainder, the contents of the remainder field (identifier-4) are unchanged.
// In either of these cases, you must analyze the results to determine which situation has actually occurred.
// For information about the NOT ON SIZE ERROR phrase, see “SIZE ERROR phrases” on page 283.
// END-DIVIDE phrase
// This explicit scope terminator serves to delimit the scope of the DIVIDE statement. END-DIVIDE turns a conditional DIVIDE statement into an imperative statement that can be nested in another conditional statement. END-DIVIDE can also be used with an imperative DIVIDE statement.

divideStatement:
	divideSimple | divideGiving | divideRemainder;

divideSimple:
	DIVIDE divisor=numericVariable3 INTO dividendAndQuotient=numericStorageAreaRounded+;

divideGiving:
	DIVIDE ( (divisor1=numericVariable3 INTO dividend1=numericVariable3) | 
	         (dividend2=numericVariable3 BY divisor2=numericVariable3)   ) 
	GIVING quotient2=numericStorageAreaRounded+;

divideRemainder:
	DIVIDE ( (divisor1=numericVariable3 INTO dividend1=numericVariable3) | 
	         (dividend2=numericVariable3 BY divisor2=numericVariable3)   ) 
	GIVING quotient1=numericStorageAreaRounded 
	REMAINDER remainder=numericStorageArea;

divideStatementEnd: END_DIVIDE;

// p330: ENTRY statement
// The ENTRY statement establishes an alternate entry point into a COBOL called subprogram.
// The ENTRY statement cannot be used in: 
// - Programs that specify a return value using the PROCEDURE DIVISION RETURNING phrase. For details, see the discussion of the RETURNING phrase under “The PROCEDURE DIVISION header” on page 247. 
// - Nested program. See “Nested programs” on page 85 for a description of nested programs.
// When a CALL statement that specifies the alternate entry point is executed in a calling program, control is transferred to the next executable statement following the ENTRY statement.
// Execution of the called program begins at the first executable statement following the ENTRY statement whose literal corresponds to the literal or identifier specified in the CALL statement.
// The entry point name on the ENTRY statement can be affected by the PGMNAME compiler option. For details, see PGMNAME in the Enterprise COBOL Programming Guide. 
// USING phrase
// For a discussion of the USING phrase, see “The PROCEDURE DIVISION header” on page 247.

entryStatement:
	ENTRY programEntryDefinition (USING programInputParameters+)?;

// p331: EVALUATE statement
// The EVALUATE statement provides a shorthand notation for a series of nested IF statements. 
// The EVALUATE statement can evaluate multiple conditions.
// The subsequent action depends on the results of these evaluations.
// Operands before the WHEN phrase 
// Are interpreted in one of two ways, depending on how they are specified: 
// - Individually, they are called selection subjects. 
// - Collectively, they are called a set of selection subjects. 
// Operands in the WHEN phrase 
// Are interpreted in one of two ways, depending on how they are specified: 
// - Individually, they are called selection objects
// - Collectively, they are called a set of selection objects. 
// ALSO 
// Separates selection subjects within a set of selection subjects; separates selection objects within a set of selection objects. 
// THROUGH and THRU Are equivalent.
// Two operands connected by a THRU phrase must be of the same class. The two operands thus connected constitute a single selection object.
// The number of selection objects within each set of selection objects must be equal to the number of selection subjects.
// Each selection object within a set of selection objects must correspond to the selection subject having the same ordinal position within the set of selection subjects, according to the following rules: 
// - Identifiers, literals, or arithmetic expressions appearing within a selection object must be valid operands for comparison to the corresponding operand in the set of selection subjects. 
// - condition-1, condition-2, or the word TRUE or FALSE appearing as a selection object must correspond to a conditional arithmeticExpression | conditionalExpression or the word TRUE or FALSE in the set of selection subjects. 
// - The word ANY can correspond to a selection subject of any type.
// END-EVALUATE phrase
// This explicit scope terminator serves to delimit the scope of the EVALUATE statement. END-EVALUATE permits a conditional EVALUATE statement to be nested in another conditional statement.
// For more information, see “Delimited scope statements” on page 280. 
// ... more details on Determining values / Comparing selection subjects and objects / Executing the EVALUATE statement p332 to 334 ...

evaluateStatement:
	EVALUATE comparisonLHSExpression (ALSO comparisonLHSExpression)*;

comparisonLHSExpression:
	variableOrExpression2 | booleanValueOrExpression;

whenCondition:
	WHEN LeftParenthesisSeparator? comparisonRHSExpression RightParenthesisSeparator?
  ( ALSO LeftParenthesisSeparator? comparisonRHSExpression RightParenthesisSeparator? )*;

comparisonRHSExpression: 
	ANY | booleanValueOrExpression | NOT? (variableOrExpression2 | alphanumericExpressionsRange);

alphanumericExpressionsRange: 
	startExpression=variableOrExpression2 (THROUGH | THRU) endExpression=variableOrExpression2;

whenOtherCondition:
	WHEN OTHER;

evaluateStatementEnd: END_EVALUATE;

// p335: EXIT statement
// The EXIT statement provides a common end point for a series of procedures.
// The EXIT statement enables you to assign a procedure-name to a given point in a program.
// The EXIT statement is treated as a CONTINUE statement. Any statements following the EXIT statement are executed.

exitStatement:
	EXIT;

// p336: EXIT METHOD statement
// The EXIT METHOD statement specifies the end of an invoked method.
// You can specify EXIT METHOD only in the PROCEDURE DIVISION of a method. EXIT METHOD causes the executing method to terminate, and control returns to the invoking statement. If the containing method specifies the PROCEDURE DIVISION RETURNING phrase, the value in the data item referred to by the RETURNING phrase becomes the result of the method invocation.
// If you need method-specific data to be in the last-used state on each invocation, declare it in method WORKING-STORAGE. If you need method-specific data to be in the initial state on each invocation, declare it in method LOCAL-STORAGE.
// If control reaches an EXIT METHOD statement in a method definition, control returns to the point that immediately follows the INVOKE statement in the invoking program or method. The state of the invoking program or method is identical to that which existed at the time it executed the INVOKE statement.
// The contents of data items and the contents of data files shared between the invoking program or method and the invoked method could have changed. The state of the invoked method is not altered except that the end of the ranges of all PERFORM statements executed by the method are considered to have been reached.
// The EXIT METHOD statement does not have to be the last statement in a sequence of imperative statements, but the statements following the EXIT METHOD will not be executed.
// When there is no next executable statement in an invoked method, an implicit EXIT METHOD statement is executed.

exitMethodStatement:
	EXIT METHOD;

// p337: EXIT PROGRAM statement
// The EXIT PROGRAM statement specifies the end of a called program and returns control to the calling program.
// You can specify EXIT PROGRAM only in the PROCEDURE DIVISION of a program. 
// EXIT PROGRAM must not be used in a declarative procedure in which the GLOBAL phrase is specified.
// If control reaches an EXIT PROGRAM statement in a program that does not possess the INITIAL attribute while operating under the control of a CALL statement (that is, the CALL statement is active), control returns to the point in the calling routine (program or method) immediately following the CALL statement. The state of the calling routine is identical to that which existed at the time it executed the CALL statement. The contents of data items and the contents of data files shared between the calling and called routine could have been changed. The state of the called program or method is not altered except that the ends of the ranges of all executed PERFORM statements are considered to have been reached.
// The execution of an EXIT PROGRAM statement in a called program that possesses the INITIAL attribute is equivalent also to executing a CANCEL statement referencing that program.
// If control reaches an EXIT PROGRAM statement, and no CALL statement is active, control passes through the exit point to the next executable statement.
// If a subprogram specifies the PROCEDURE DIVISION RETURNING phrase, the value in the data item referred to by the RETURNING phrase becomes the result of the subprogram invocation.
// The EXIT PROGRAM statement should be the last statement in a sequence of imperative statements. When it is not, statements following the EXIT PROGRAM will not be executed if a CALL statement is active.
// When there is no next executable statement in a called program, an implicit EXIT PROGRAM statement is executed.

exitProgramStatement:
	EXIT PROGRAM;

// p338: GOBACK statement
// The GOBACK statement functions like the EXIT PROGRAM statement when it is coded as part of a called program (or the EXIT METHOD statement when GOBACK is coded as part of an invoked method) and like the STOP RUN statement when coded in a main program.
// The GOBACK statement specifies the logical end of a called program or invoked method.
// A GOBACK statement should appear as the only statement or as the last of a series of imperative statements in a sentence because any statements following the GOBACK are not executed. GOBACK must not be used in a declarative procedure in which the GLOBAL phrase is specified.
// If control reaches a GOBACK statement while a CALL statement is active, control returns to the point in the calling program or method immediately following the CALL statement, as in the EXIT PROGRAM statement.
// If control reaches a GOBACK statement while an INVOKE statement is active, control returns to the point in the invoking program or method immediately following the INVOKE statement, as in the EXIT METHOD statement.
// In addition, the execution of a GOBACK statement in a called program that possesses the INITIAL attribute is equivalent to executing a CANCEL statement referencing that program.
// The table below shows the action taken for the GOBACK statement in both a main program and a subprogram.
// Main program -> Returns to the calling program. (Can be the system, which causes the application to end.) 
// Subprogram -> Returns to the calling program.

gobackStatement:
	GOBACK;

// p339: GO TO statement
// The GO TO statement transfers control from one part of the PROCEDURE DIVISION to another.
// The types of GO TO statements are: 
// - Unconditional 
// - Conditional 
// - Altered 
//
// Unconditional GO TO 
// The unconditional GO TO statement transfers control to the first statement in the paragraph or section identified by procedure-name, unless the GO TO statement has been modified by an ALTER statement.
// For more information, see “ALTER statement” on page 301.
// Format 1: unconditional GO TO statement
// procedure-name-1 
// Must name a procedure or a section in the same PROCEDURE DIVISION as the GO TO statement.
// When the unconditional GO TO statement is not the last statement in a sequence of imperative statements, the statements following the GO TO are not executed.
// When a paragraph is referred to by an ALTER statement, the paragraph must consist of a paragraph-name followed by an unconditional or altered GO TO statement. 
//
// Conditional GO TO 
// The conditional GO TO statement transfers control to one of a series of procedures, depending on the value of the data item referenced by identifier-1.
// Format 2: conditional GO TO statement
// procedure-name-1 
// Must be a procedure or a section in the same PROCEDURE DIVISION as the GO TO statement. 
// The number of procedure-names must not exceed 255. 
// identifier-1 
// Must be a numeric elementary data item that is an integer. 
// If 1, control is transferred to the first statement in the procedure named by the first occurrence of procedure-name-1. 
// If 2, control is transferred to the first statement in the procedure named by the second occurrence of procedure-name-1, and so forth. 
// If the value of identifier is anything other than a value within the range of 1 through n (where n is the number of procedure-names specified in this GO TO statement), no control transfer occurs. Instead, control passes to the next statement in the normal sequence of execution.
// 
// Altered GO TO 
// The altered GO TO statement transfers control to the first statement of the paragraph named in the ALTER statement.
// You cannot specify the altered GO TO statement in the following cases: 
// - A program or method that has the RECURSIVE attribute
// - A program compiled with the THREAD compiler option
// An ALTER statement referring to the paragraph that contains the altered GO TO statement should be executed before the GO TO statement is executed. 
// Otherwise, the GO TO statement acts like a CONTINUE statement.
// Format 3: altered GO TO statement
// When an ALTER statement refers to a paragraph, the paragraph can consist only of the paragraph-name followed by an unconditional or altered GO TO statement.

gotoStatement: 
	gotoSimple | gotoConditional;

gotoSimple:
	GO TO? procedureName;

gotoConditional:
	GO TO? procedureName+ DEPENDING ON? variable1;

// p341: IF statement
// The IF statement evaluates a condition and provides for alternative actions in the
// object program, depending on the evaluation.
// Notes: END-IF can be specified with statement-2 or NEXT SENTENCE.
// condition-1
// Can be any simple or complex condition, as described in “Conditional
// expressions” on page 256.
// statement-1, statement-2
// Can be any one of the following options:
// - An imperative statement
// - A conditional statement
// - An imperative statement followed by a conditional statement
// NEXT SENTENCE
// The NEXT SENTENCE phrase transfers control to an implicit CONTINUE
// statement immediately following the next separator period.
// When NEXT SENTENCE is specified with END-IF, control does not pass to
// the statement following the END-IF. Instead, control passes to the
// statement after the closest following period.
// END-IF phrase
// This explicit scope terminator serves to delimit the scope of the IF statement.
// END-IF permits a conditional IF statement to be nested in another conditional
// statement. For more information about explicit scope terminators, see “Delimited
// scope statements” on page 280.
// The scope of an IF statement can be terminated by any of the following options:
// - An END-IF phrase at the same level of nesting
// - A separator period
// - If nested, by an ELSE phrase associated with an IF statement at a higher level of
//   nesting
// Transferring control
// The topic describes the actions to take when conditions tested is true or false.
// If the condition tested is true, one of the following actions takes place:
// - If statement-1 is specified, statement-1 is executed. If statement-1 contains a
//   procedure branching or conditional statement, control is transferred according to
//   the rules for that statement. If statement-1 does not contain a
//   procedure-branching statement, the ELSE phrase, if specified, is ignored, and
//   control passes to the next executable statement after the corresponding END-IF
//   or separator period.
// - If NEXT SENTENCE is specified, control passes to an implicit CONTINUE
//   statement immediately preceding the next separator period.
// If the condition tested is false, one of the following actions takes place:
// - If ELSE statement-2 is specified, statement-2 is executed. If statement-2 contains a
//   procedure-branching or conditional statement, control is transferred, according
//   to the rules for that statement. If statement-2 does not contain a
//   procedure-branching or conditional statement, control is passed to the next
//   executable statement after the corresponding END-IF or separator period.
// - If ELSE NEXT SENTENCE is specified, control passes to an implicit CONTINUE
//   STATEMENT immediately preceding the next separator period.
// - If neither ELSE statement-2 nor ELSE NEXT SENTENCE is specified, control
//   passes to the next executable statement after the corresponding END-IF or
//   separator period.
// When the ELSE phrase is omitted, all statements following the condition and
// preceding the corresponding END-IF or the separator period for the sentence are
// considered to be part of statement-1.
// Nested IF statements
// When an IF statement appears as statement-1 or statement-2, or as part of statement-1
// or statement-2, that IF statement is nested.
// Nested IF statements are considered to be matched IF, ELSE, and END-IF
// combinations proceeding from left to right. Thus, any ELSE encountered is
// matched with the nearest preceding IF that either has not been already matched
// with an ELSE or has not been implicitly or explicitly terminated. Any END-IF
// encountered is matched with the nearest preceding IF that has not been implicitly
// or explicitly terminated.

ifStatement:
	IF conditionalExpression THEN?;

elseCondition:
	ELSE;

nextSentenceStatement:
	NEXT SENTENCE;

ifStatementEnd:
	END_IF;

// p343: INITIALIZE statement
// The INITIALIZE statement sets selected categories of data fields to predetermined
// values. The INITIALIZE statement is functionally equivalent to one or more MOVE
// statements.
// identifier-1
// Receiving areas.
// identifier-1 must reference one of the following items:
// - An alphanumeric group item
// - A national group item
// - An elementary data item of one of the following categories:
//   – Alphabetic
//   – Alphanumeric
//   – Alphanumeric-edited
//   – DBCS
//   – External floating-point
//   – Internal floating-point
//   – National
//   – National-edited
//   – Numeric
//   – Numeric-edited
// - A special register that is valid as a receiving operand in a MOVE
//   statement with identifer-2 or literal-1 as the sending operand.
// When identifier-1 references a national group item, identifier-1 is processed
// as a group item.
// identifier-2, literal-1
// Sending areas.
// When identifier-2 references a national group item, identifier-2 is processed
// as an elementary data item of category national.
// identifier-2 must reference an elementary data item (or a national group
// item treated as elementary) that is valid as a sending operand in a MOVE
// statement with identifier-1 as the receiving operand.
// literal-1 must be a literal that is valid as a sending operand in a MOVE
// statement with identifier-1 as the receiving operand.
// A subscripted item can be specified for identifier-1. A complete table can be
// initialized only by specifying identifier-1 as a group that contains the complete
// table.
// Usage note: The data description entry for identifier-1 can contain the DEPENDING
// phrase of the OCCURS clause. However, you cannot use the INITIALIZE statement
// to initialize a variably-located item or a variable-length item.
// The data description entry for identifier-1 must not contain a RENAMES clause.
// Special registers can be specified for identifier-1 and identifier-2 only if they are
// valid receiving fields or sending fields, respectively, for the implied MOVE
// statements.
// REPLACING phrase
// When the REPLACING phrase is specified:
// - identifier-2 must reference an item of a category that is valid as a sending
//   operand in a MOVE statement to an item of the corresponding category
//   specified in the REPLACING phrase.
// - literal-1 must be of a category that is valid as a sending operand in a MOVE
//   statement to an item of the corresponding category specified in the REPLACING
//   phrase.
// - A floating-point literal, a data item of category internal floating-point, or a data
//   item of category external floating point is treated as if it were in the NUMERIC
//   category.
// - The same category cannot be repeated in a REPLACING phrase.
// With the exception of EGCS, the keyword after the word REPLACING corresponds
// to a category of data shown in “Classes and categories of data” on page 162.
// EGCS in the REPLACING phrase is synonymous with DBCS.
// When the REPLACING phrase is not specified:
// - SPACE is the implied sending item for receiving items of category alphabetic,
//   alphanumeric, alphanumeric-edited, DBCS, national, or national-edited.
// - ZERO is the implied sending item for receiving items of category numeric or
//   numeric-edited.
// INITIALIZE statement rules
// The topic provides general rules of the INITIALIZE statement.
// 1. Whether identifier-1 references an elementary item, an alphanumeric group item,
//    or a national group item, all operations are performed as if a series of MOVE
//    statements had been written, each of which had an elementary item as a
//    receiving field.
//    If the REPLACING phrase is specified:
//    - If identifier-1 references an alphanumeric group item or a national group
//      item, any elementary item within the data item referenced by identifier-1 is
//      initialized only if it belongs to a category specified in the REPLACING
//      phrase.
//    Initialization takes place as if the data item referenced by identifier-2 or literal-1
//    were the sending operand in an implicit MOVE statement to the receiving item.
//    All elementary receiving fields, including all occurrences of table items within
//    the group, are initialized, with the following exceptions:
//    - Index data items
//    - Object references
//    - Data items defined with USAGE IS POINTER, USAGE IS
//      FUNCTION-POINTER, or USAGE IS PROCEDURE-POINTER
//    - Elementary FILLER data items
//    - Items that are subordinate to identifier-1 and contain a REDEFINES clause, or
//      any items subordinate to such an item. (However, identifier-1 can contain a
//      REDEFINES clause or be subordinate to a redefining item.)
// 2. The areas referenced by identifier-1 are initialized in the order (left to right) of
//    the appearance of identifier-1 in the statement. Within a group receiving field,
//    affected elementary items are initialized in the order of their definition within
//    the group.
// 3. If identifier-1 occupies the same storage area as identifier-2, the result of the
//    execution of this statement is undefined, even if these operands are defined by
//    the same data description entry.

initializeStatement:
	INITIALIZE storageArea1+ (REPLACING initializeReplacingDirective+)?;

initializeReplacingDirective:
	dataCategory DATA? BY variable6;

dataCategory:
	(ALPHABETIC | ALPHANUMERIC | ALPHANUMERIC_EDITED | NATIONAL | NATIONAL_EDITED | NUMERIC | NUMERIC_EDITED | DBCS | EGCS);

// p346: INSPECT statement
// The INSPECT statement examines characters or groups of characters in a data
// item.
// The INSPECT statement does the following tasks:
// - Counts the occurrences of a specific character (alphanumeric, DBCS, or national)
//   in a data item (formats 1 and 3).
// - Counts the occurrences of specific characters and fills all or portions of a data
//   item with specified characters, such as spaces or zeros (formats 2 and 3).
// - Converts all occurrences of specific characters in a data item to user-supplied
//   replacement characters (format 4).
//
// p346: Format 1: INSPECT statement with TALLYING phrase
// p347: Format 2: INSPECT statement with REPLACING phrase
// p348: Format 3: INSPECT statement with TALLYING and REPLACING phrases
// p349: Format 4: INSPECT statement with CONVERTING phrase
//
// identifier-1
// Is the inspected item and can be any of the following items:
// - An alphanumeric group item or a national group item
// - An elementary data item described explicitly or implicitly with usage
//   DISPLAY, DISPLAY-1, or NATIONAL. The item can have any category
//   that is valid for the selected usage.
// identifier-3 , identifier-4 , identifier-5 , identifier-6 , identifier-7
// Must reference an elementary data item described explicitly or implicitly
// with usage DISPLAY, DISPLAY-1, or NATIONAL.
// literal-1 , literal-2 , literal-3 , literal-4
// Must be of category alphanumeric, DBCS, or national.
// When identifier-1 is of usage NATIONAL, literals must be of category
// national.
// When identifier-1 is of usage DISPLAY-1, literals must be of category DBCS.
// When identifier-1 is of usage DISPLAY, literals must be of category
// alphanumeric.
// When identifier-1 is of usage DISPLAY-1 (DBCS) literals may be the
// figurative constant SPACE.
// When identifier-1 is of usage DISPLAY or NATIONAL, literals can be any
// figurative constant that does not begin with the word ALL, as specified in
// “Figurative constants” on page 13. The figurative constant is treated as a
// one-character alphanumeric literal when identifier-1 is of usage DISPLAY,
// and as a one-character national literal when identifier-1 is of usage
// NATIONAL.
// All identifiers (except identifier-2) must have the same usage as identifier-1. All
// literals must have category alphanumeric, DBCS, or national when identifier-1 has
// usage DISPLAY, DISPLAY-1, or NATIONAL, respectively.
//
// TALLYING phrase (formats 1 and 3)
// This phrase counts the occurrences of a specific character or special character in a
// data item.
// When identifier-1 is a DBCS data item, DBCS characters are counted; when
// identifier-1 is a data item of usage national, national characters (encoding units) are
// counted; otherwise, alphanumeric characters (bytes) are counted.
// identifier-2
// Is the count field, and must be an elementary integer item defined without
// the symbol P in its PICTURE character-string.
// identifier-2 cannot be of category external floating-point.
// You must initialize identifier-2 before execution of the INSPECT statement
// begins.
// Usage note: The count field can be an integer data item defined with usage
// NATIONAL.
// identifier-3 or literal-1
// Is the tallying field (the item whose occurrences will be tallied).
// CHARACTERS
// When CHARACTERS is specified and neither the BEFORE nor AFTER
// phrase is specified, the count field (identifier-2) is increased by 1 for each
// character (including the space character) in the inspected item (identifier-1).
// Thus, execution of an INSPECT statement with the TALLYING phrase
// increases the value in the count field by the number of character positions
// in the inspected item.
// ALL 
// When ALL is specified and neither the BEFORE nor AFTER phrase is
// specified, the count field (identifier-2) is increased by 1 for each
// nonoverlapping occurrence of the tallying comparand (identifier-3 or
// literal-1) in the inspected item (identifier-1), beginning at the leftmost
// character position and continuing to the rightmost.
// LEADING
// When LEADING is specified and neither the BEFORE nor AFTER phrase is
// specified, the count field (identifier-2) is increased by 1 for each contiguous
// nonoverlapping occurrence of the tallying comparand in the inspected item
// (identifier-1), provided that the leftmost such occurrence is at the point
// where comparison began in the first comparison cycle for which the
// tallying comparand is eligible to participate.
// FIRST (format 3 only)
// When FIRST is specified and neither the BEFORE nor AFTER phrase is
// specified, the substitution field replaces the leftmost occurrence of the
// subject field in the inspected item (identifier-1).
//
// REPLACING phrase (formats 2 and 3)
// This phrase fills all or portions of a data item with specified characters, such as
// spaces or zeros.
// identifier-3 or literal-1
// Is the subject field, which identifies the characters to be replaced.
// identifier-5 or literal-3
// Is the substitution field (the item that replaces the subject field).
// The subject field and the substitution field must be the same length.
// CHARACTERS BY
// When the CHARACTERS BY phrase is used, the substitution field must be
// one character position in length.
// When CHARACTERS BY is specified and neither the BEFORE nor AFTER
// phrase is specified, the substitution field replaces each character in the
// inspected item (identifier-1), beginning at the leftmost character position
// and continuing to the rightmost.
// ALL
// When ALL is specified and neither the BEFORE nor AFTER phrase is
// specified, the substitution field replaces each nonoverlapping occurrence of
// the subject field in the inspected item (identifier-1), beginning at the
// leftmost character position and continuing to the rightmost.
// LEADING
// When LEADING is specified and neither the BEFORE nor AFTER phrase is
// specified, the substitution field replaces each contiguous nonoverlapping
// occurrence of the subject field in the inspected item (identifier-1), provided
// that the leftmost such occurrence is at the point where comparison began
// in the first comparison cycle for which this substitution field is eligible to
// participate.
// FIRST
// When FIRST is specified and neither the BEFORE nor AFTER phrase is
// specified, the substitution field replaces the leftmost occurrence of the
// subject field in the inspected item (identifier-1).
//
// When both the TALLYING and REPLACING phrases are specified (format 3), the
// INSPECT statement is executed as if an INSPECT TALLYING statement (format 1)
// were specified, immediately followed by an INSPECT REPLACING statement
// (format 2).
//
// The following replacement rules apply:
// - When the subject field is a figurative constant, the one-character substitution
//   field replaces each character in the inspected item that is equivalent to the
//   figurative constant.
// - When the substitution field is a figurative constant, the substitution field
//   replaces each nonoverlapping occurrence of the subject field in the inspected
//   item.
// - When the subject and substitution fields are character-strings, the
//   character-string specified in the substitution field replaces each nonoverlapping
//   occurrence of the subject field in the inspected item.
// - After replacement has occurred in a given character position in the inspected
//   item, no further replacement for that character position is made in this execution
//   of the INSPECT statement.
// 
// BEFORE and AFTER phrases (all formats)
// This phrase narrows the set of items being tallied or replaced.
// No more than one BEFORE phrase and one AFTER phrase can be specified for any
// one ALL, LEADING, CHARACTERS, FIRST or CONVERTING phrase.
// identifier-4 or literal-2
// Is the delimiter.
// Delimiters are not counted or replaced.
// INITIAL
// The first occurrence of a specified item.
// The BEFORE and AFTER phrases change how counting and replacing are done:
// - When BEFORE is specified, counting or replacing of the inspected item
//  (identifier-1) begins at the leftmost character position and continues until the first
//  occurrence of the delimiter is encountered. If no delimiter is present in the
//  inspected item, counting or replacing continues toward the rightmost character
//  position.
// - When AFTER is specified, counting or replacing of the inspected item 
//  (identifier-1) begins with the first character position to the right of the delimiter
//  and continues toward the rightmost character position in the inspected item. If
//  no delimiter is present in the inspected item, no counting or replacement takes
//  place.
//
// CONVERTING phrase (format 4)
// This phrase converts all occurrences of a specific character or string of characters in
// a data item (identifier-1) to user-supplied replacement characters.
// identifier-6 or literal-4
// Specifies the character string to be replaced.
// The same character must not appear more than once in either literal-4 or
// identifier-6.
// identifier-7 or literal-5
// Specifies the replacing character string.
// The replacing character string (identifier-7 or literal-5) must be the same size
// as the replaced character string (identifier-6 or literal-4).
// A format-4 INSPECT statement is interpreted and executed as if a format-2
// INSPECT statement had been written with a series of ALL phrases (one for each
// character of literal-4), specifying the same identifier-1. The effect is as if each single
// character of literal-4 were referenced as literal-1, and the corresponding single
// character of literal-5 referenced as literal-3. Correspondence between the characters
// of literal-4 and the characters of literal-5 is by ordinal position within the data item.
// If identifier-4, identifier-6, or identifier-7 occupies the same storage area as identifier-1,
// the result of the execution of this statement is undefined, even if they are defined
// by the same data description entry.
// The following table describes the treatment of data items that can be used as an
// operand in the INSPECT statement:
//
// ... more details p352->353 : Table 37. Treatment of the content of data items ...
// ... more details p353 : Data flow ...
// ... more details p354->355 : Example of the INSPECT statement ...

inspectStatement:
	INSPECT alphanumericStorageArea (  convertingPhrase                  | 
	                                  (tallyingPhrase? replacingPhrase?) );

convertingPhrase:
	CONVERTING searchedCharacterString =alphanumericVariable1 
	TO         replacingCharacterString=alphanumericVariable2 
	countingOrReplacingCondition*;

tallyingPhrase:
	TALLYING inspectTallyingOperation+;

inspectTallyingOperation:
	countField=numericStorageArea FOR (countAllCharacters | countCharacterStrings)+;

countAllCharacters:
	CHARACTERS countingOrReplacingCondition*;

countCharacterStrings:
	(ALL | LEADING) countCharacterStringPattern+;

countCharacterStringPattern:
	searchedCharacterString=alphanumericVariable1 countingOrReplacingCondition*;

replacingPhrase:
	REPLACING (replaceAllCharacters | replaceCharacterStrings)+;

replaceAllCharacters:
	CHARACTERS BY replacingCharacterString=alphanumericVariable2 countingOrReplacingCondition*;

replaceCharacterStrings:
	(ALL | LEADING | FIRST) replaceCharacterStringPattern+;

replaceCharacterStringPattern:
	searchedCharacterString=alphanumericVariable1 BY replacingCharacterString=alphanumericVariable2 countingOrReplacingCondition*;

countingOrReplacingCondition:
	(BEFORE | AFTER) INITIAL? delimiter=alphanumericVariable1;

// p356: INVOKE statement 
// The INVOKE statement can create object instances of a COBOL or Java class and
// can invoke a method defined in a COBOL or Java class.
// identifier-1
// Must be defined as USAGE OBJECT REFERENCE. The contents of
// identifier-1 specify the object on which a method is invoked.
// When identifier-1 is specified, either literal-1 or identifier-2 must be specified,
// identifying the name of the method to be invoked.
// The results of the INVOKE statement are undefined if either:
// - identifier-1 does not contain a valid reference to an object.
// - identifier-1 contains NULL.
// class-name-1
// When class-name-1 is specified together with literal-1 or identifier-2, the
// INVOKE statement invokes a static or factory method of the class
// referenced by class-name-1. literal-1 or identifier-2 specifies the name of the
// method that is to be invoked. The method must be a static method if
// class-name-1 is a Java class; the method must be a factory method if
// class-name-1 is a COBOL class.
// When class-name-1 is specified together with NEW, the INVOKE statement
// creates a new object that is an instance of class class-name-1.
// You must specify class-name-1 in the REPOSITORY paragraph of the
// configuration section of the class or program that contains the INVOKE
// statement.
// SELF 
// An implicit reference to the object used to invoke the currently executing
// method. When SELF is specified, the INVOKE statement must appear
// within the PROCEDURE DIVISION of a method.
// SUPER
// An implicit reference to the object that was used to invoke the currently
// executing method. The resolution of the method to be invoked will ignore
// any methods declared in the class definition of the currently executing
// method and methods defined in any class derived from that class; thus the
// method invoked will be one that is inherited from an ancestor class.
// literal-1
// The value of literal-1 is the name of the method to be invoked. The
// referenced object must support the method identified by literal-1.
// literal-1 must be an alphanumeric literal or a national literal.
// literal-1 is interpreted in a case-sensitive manner. The method name, the
// number of arguments, and the data types of the arguments in the USING
// phrase of the INVOKE statement are used to select the method with
// matching signature that is supported by the object. The method can be
// overloaded.
// identifier-2
// A data item of category alphabetic, alphanumeric, or national that at run
// time contains the name of the method to be invoked. The referenced object
// must support the method identified by identifier-2.
// If identifier-2 is specified, identifier-1 must be defined as USAGE OBJECT
// REFERENCE without any optional phrases; that is, identifier-1 must be a
// universal object reference.
// The content of identifier-2 is interpreted in a case-sensitive manner. The
// method name, the number of arguments, and the data types of the
// arguments in the USING phrase of the INVOKE statement are used to
// select the method with matching signature that is supported by the object.
// The method can be overloaded.
// NEW 
// The NEW operand specifies that the INVOKE statement is to create a new
// object instance of the class class-name-1. class-name-1 must be specified.
// When class-name-1 is implemented in Java, the USING phrase of the
// INVOKE statement can be specified. The number of arguments and the
// data types of the arguments in the USING phrase of the INVOKE
// statement are used to select the Java constructor with matching signature
// that is supported by the class. An object instance of class class-name-1 is
// allocated, the selected constructor (or the default constructor) is executed,
// and a reference to the created object is returned.
// When class-name-1 is implemented in COBOL, the USING phrase of the
// INVOKE statement must not be specified. An object instance of class
// class-name-1 is allocated, instance data items are initialized to the values
// specified in associated VALUE clauses, and a reference to the created object
// is returned.
// When NEW is specified, you must also specify a RETURNING phrase as
// described in “RETURNING phrase” on page 359.
// USING phrase
// The USING phrase specifies arguments that are passed to the target method. The
// argument data types and argument linkage conventions are restricted to those
// supported by Java. See “BY VALUE phrase” for details.
// BY VALUE phrase
// Arguments specified in an INVOKE statement must be passed BY VALUE.
// The BY VALUE phrase specifies that the value of the argument is passed, not a
// reference to the sending data item. The invoked method can modify the formal
// parameter that corresponds to an argument passed by value, but changes do not
// affect the argument because the invoked method has access only to a temporary
// copy of the sending data item.
// identifier-3
// Must be an elementary data item in the DATA DIVISION. The data type of
// identifier-3 must be one of the types supported for Java interoperation, as
// listed in “Interoperable data types for COBOL and Java” on page 361.
// Miscellaneous cases that are also supported as identifier-3 are listed in
// “Miscellaneous argument types for COBOL and Java” on page 362, with
// their corresponding Java type.
// See Conformance requirements for arguments for additional requirements
// that apply to identifier-3.
// literal-2
// Must be of a type suitable for Java interoperation and must exactly match
// the type of the corresponding parameter in the target method. Supported
// literal forms are listed in “Miscellaneous argument types for COBOL and
// Java” on page 362, with their corresponding Java type.
// literal-2 must not be a DBCS literal.
// LENGTH OF identifier-3
// Specifies that the length of identifier-3 is passed as an argument in the
// LENGTH OF special register. A LENGTH OF special register passed BY
// VALUE is treated as a PIC 9(9) binary value. For information about the
// LENGTH OF special register, see “LENGTH OF” on page 19.
// Conformance requirements for arguments
// When identifier-3 is an object reference, certain rules apply.
// The rules are:
// - A class-name must be specified in the data description entry for that object
//   reference. That is, identifier-3 must not be a universal object reference.
// - The specified class-name must reference a class that is exactly the class of the
//   corresponding parameter in the invoked method. That is, the class of identifier-3
//   must not be a subclass or a superclass of the corresponding parameter's class.
// When identifier-3 is not an object reference, the following rules apply:
// - If the target method is implemented in COBOL, the description of identifier-3
//   must exactly match the description of the corresponding formal parameter in the
//   target method.
// - If the target method is implemented in Java, the description of identifier-3 must
//   correspond to the Java type of the formal parameter in the target method, as
//   specified in “Interoperable data types for COBOL and Java” on page 361.
// Usage note: Adherence to conformance requirements for arguments is the
// responsibility of the programmer. Conformance requirements are not verified by
// the compiler.
// RETURNING phrase
// The RETURNING phrase specifies a data item that will contain the value returned
// from the invoked method. You can specify the RETURNING phrase on the
// INVOKE statement when invoking methods that are written in COBOL or Java.
// identifier-4
// The RETURNING data item. identifier-4:
// - Must be defined in the DATA DIVISION
// - Must not be reference-modified
// - Is not changed if an EXCEPTION occurs
// The data type of identifier-4 must be one of the types supported for Java
// interoperation, as listed in “Interoperable data types for COBOL and Java”
// on page 361.
// See Conformance requirements for the RETURNING item for additional
// requirements that apply to identifier-4.
// If identifier-4 is specified and the target method is written in COBOL, the
// target method must have a RETURNING phrase in its PROCEDURE
// DIVISION header. When the target method returns, its return value is
// assigned to identifier-4 using the rules for the SET statement if identifier-4 is
// described with USAGE OBJECT REFERENCE; otherwise, the rules for the
// MOVE statement are used.
// The RETURNING data item is an output-only parameter. On entry to the called
// method, the initial state of the PROCEDURE DIVISION RETURNING data item
// has an undefined and unpredictable value. You must initialize the PROCEDURE
// DIVISION RETURNING data item in the invoked method before you reference its
// value. The value that is passed back to the invoker is the final value of the
// PROCEDURE DIVISION RETURNING data item when the invoked method
// returns.
// See Managing local and global references in the Enterprise COBOL Programming Guide
// for discussion of local and global object references as defined in Java. These
// attributes affect the life-time of object references.
// Usage note: The RETURN-CODE special register is not set by execution of
// INVOKE statements.
// Conformance requirements for the RETURNING item
// For INVOKE statements that specify class-name-1 NEW, the RETURNING phrase is
// required.
// The returning item must be one of the following ones:
// - A universal object reference
// - An object reference specifying class-name-1
// - An object reference specifying a superclass of class-name-1
// For INVOKE statements without the NEW phrase, the RETURNING item specified
// in the method invocation and in the corresponding target method must satisfy the
// following requirements:
// - The presence or absence of a return value must be the same on the INVOKE
//   statement and in the target method.
// - If the RETURNING item is not an object reference, the following rules apply:
//   – If the target method is implemented in COBOL, the returning item in the
//     INVOKE statement and the RETURNING item in the target method must
//     have an identical data description entry.
//   – If the target method is implemented in Java, the returning item in the
//     INVOKE statement must correspond to the Java type of the method result, as
//     described in “Interoperable data types for COBOL and Java” on page 361.
// - If the RETURNING item is an object reference, the RETURNING item specified
//   in the INVOKE statement must be an object reference typed exactly to the class
//   of the returning item specified in the target method. That is, the class of
//   identifier-4 must not be a subclass or a superclass of the class of the returning
//   item in the target method.
// Usage note: Adherence to conformance requirements for returning items is the
// responsibility of the programmer. Conformance requirements are not verified by
// the compiler.
// ON EXCEPTION phrase
// An exception condition occurs when the identified object or class does not support
// a method with a signature that matches the signature of the method specified in
// the INVOKE statement. When an exception condition occurs, one of the following
// actions occurs:
// - If the ON EXCEPTION phrase is specified, control is transferred to
//   imperative-statement-1.
// - If the ON EXCEPTION phrase is not specified, a severity-3 Language
//   Environment condition is raised at run time.
// NOT ON EXCEPTION phrase
// If an exception condition does not occur (that is, the identified method is
// supported by the specified object), control is transferred to the invoked method.
// After control is returned from the invoked method, control is then transferred:
// 1. To imperative-statement-2, if the NOT ON EXCEPTION phrase is specified.
// 2. To the end of the INVOKE statement if the NOT ON EXCEPTION phrase is not
//    specified.
// END-INVOKE phrase
// This explicit scope terminator serves to delimit the scope of the INVOKE
// statement. An INVOKE statement that is terminated by END-INVOKE, along with
// its contained statements, becomes a unit that is treated as though it were an
// imperative statement. It can be specified as an imperative statement in a
// conditional statement; for example, in the exception phrase of another statement.
//
// ... more details p361->362 Interoperable data types for COBOL and Java ...
// ... more details p362->363 Miscellaneous argument types for COBOL and Java ...

invokeStatement:
	INVOKE 
	(classNameOrObjectReferenceVariable | selfObjectIdentifier | superObjectIdentifier) 
	(methodNameVariable | NEW) 
	(USING invokeInputParameter+)? 
	(RETURNING invokeOutputParameter)?;

invokeInputParameter:
	BY? VALUE sharedVariable3+;

invokeOutputParameter:
	sharedStorageArea1;

invokeStatementEnd: END_INVOKE;

// p364: MERGE statement
// The MERGE statement combines two or more identically sequenced files (that is,
// files that have already been sorted according to an identical set of ascending or
// descending keys) on one or more keys and makes records available in merged
// order to an output procedure or output file.
// A MERGE statement can appear anywhere in the PROCEDURE DIVISION except
// in a declarative section.
// The MERGE statement is not supported for programs compiled with the THREAD
// compiler option.
// file-name-1
// The name given in the SD entry that describes the records to be merged.
// No file-name can be repeated in the MERGE statement.
// No pair of file-names in a MERGE statement can be specified in the same
// SAME AREA, SAME SORT AREA, or SAME SORT-MERGE AREA clause.
// However, any file-names in the MERGE statement can be specified in the
// same SAME RECORD AREA clause.
// When the MERGE statement is executed, all records contained in file-name-2,
// file-name-3, ... , are accepted by the merge program and then merged according to
// the keys specified.
// ASCENDING/DESCENDING KEY phrase
// This phrase specifies that records are to be processed in an ascending or
// descending sequence (depending on the phrase specified), based on the specified
// merge keys.
// data-name-1
// Specifies a KEY data item on which the merge will be based. Each such
// data-name must identify a data item in a record associated with file-name-1.
// The data-names following the word KEY are listed from left to right in the
// MERGE statement in order of decreasing significance without regard to
// how they are divided into KEY phrases. The leftmost data-name is the
// major key, the next data-name is the next most significant key, and so
// forth.
// The following rules apply:
// - A specific key data item must be physically located in the same position
//   and have the same data format in each input file. However, it need not
//   have the same data-name.
// - If file-name-1 has more than one record description, the KEY data items
//   need be described in only one of the record descriptions.
// - If file-name-1 contains variable-length records, all of the KEY data-items
//   must be contained within the first n character positions of the record,
//   where n equals the minimum records size specified for file-name-1.
// - KEY data items must not contain an OCCURS clause or be subordinate
//   to an item that contains an OCCURS clause.
// - KEY data items cannot be:
//   – Variably located
//   – Group items that contain variable-occurrence data items
//   – Category numeric described with usage NATIONAL (national decimal
//     type)
//   – Category external floating-point described with usage NATIONAL
//     (national floating-point)
//   – Category DBCS
// - KEY data items can be qualified.
// - KEY data items can be any of the following data categories:
//   – Alphabetic, alphanumeric, alphanumeric-edited
//   – Numeric (except numeric with usage NATIONAL)
//   – Numeric-edited (with usage DISPLAY or NATIONAL)
//   – Internal floating-point or display floating-point
//   – National or national-edited
// The direction of the merge operation depends on the specification of the
// ASCENDING or DESCENDING keywords as follows:
// - When ASCENDING is specified, the sequence is from the lowest key value to
//   the highest key value.
// - When DESCENDING is specified, the sequence is from the highest key value to
//   the lowest key value.
// If the KEY data item is described with usage NATIONAL, the sequence of the KEY
// values is based on the binary values of the national characters.
// When the COLLATING SEQUENCE phrase is not specified, the key comparisons
// are performed according to the rules for comparison of operands in a relation
// condition. For details, see “General relation conditions” on page 260.
// When the COLLATING SEQUENCE phrase is specified, the indicated collating
// sequence is used for key data items of alphabetic, alphanumeric,
// alphanumeric-edited, external floating-point, and numeric-edited categories. For all
// other key data items, the comparisons are performed according to the rules for
// comparison of operands in a relation condition.
// COLLATING SEQUENCE phrase
// This phrase specifies the collating sequence to be used in alphanumeric
// comparisons for the KEY data items in this merge operation.
// The COLLATING SEQUENCE phrase has no effect for keys that are not alphabetic
// or alphanumeric.
// alphabet-name-1
// Must be specified in the ALPHABET clause of the SPECIAL-NAMES
// paragraph. Any one of the alphabet-name clause phrases can be specified,
// with the following results:
// STANDARD-1
// The ASCII collating sequence is used for all alphanumeric
// comparisons. (The ASCII collating sequence is shown in “US
// English ASCII code page” on page 572.)
// STANDARD-2
// The 7-bit code defined in the International Reference Version of
// ISO/IEC 646, 7-bit coded character set for information interchange is
// used for all alphanumeric comparisons.
// NATIVE
// The EBCDIC collating sequence is used for all alphanumeric
// comparisons. (The EBCDIC collating sequence is shown in
// “EBCDIC collating sequence” on page 569.)
// EBCDIC
// The EBCDIC collating sequence is used for all alphanumeric
// comparisons. (The EBCDIC collating sequence is shown in
// “EBCDIC collating sequence” on page 569.)
// literal
// The collating sequence established by the specification of literals in
// the ALPHABET-NAME clause is used for all alphanumeric
// comparisons.
// When the COLLATING SEQUENCE phrase is omitted, the PROGRAM
// COLLATING SEQUENCE clause (if specified) in the OBJECT-COMPUTER
// paragraph identifies the collating sequence to be used. When both the
// COLLATING SEQUENCE phrase of the MERGE statement and the PROGRAM
// COLLATING SEQUENCE clause of the OBJECT-COMPUTER paragraph are
// omitted, the EBCDIC collating sequence is used.
// USING phrase
// file-name-2 , file-name-3 , ...
// Specifies the input files.
// During the MERGE operation, all the records on file-name-2, file-name-3, ... (that is,
// the input files) are transferred to file-name-1. At the time the MERGE statement is
// executed, these files must not be open. The input files are automatically opened,
// read, and closed. If DECLARATIVE procedures are specified for these files for
// input operations, the declaratives will be driven for errors if errors occur.
// All input files must specify sequential or dynamic access mode and be described in
// FD entries in the DATA DIVISION.
// If file-name-1 contains variable-length records, the size of the records contained in
// the input files (file-name-2, file-name-3, ...) must be neither less than the smallest
// record nor greater than the largest record described for file-name-1. If file-name-1
// contains fixed-length records, the size of the records contained in the input files
// must not be greater than the largest record described for file-name-1. For more
// information, see Sorting and merging files in the Enterprise COBOL Programming
// Guide.
// GIVING phrase
// file-name-4 , ...
// Specifies the output files.
// When the GIVING phrase is specified, all the merged records in file-name-1 are
// automatically transferred to the output files (file-name-4, ...).
// All output files must specify sequential or dynamic access mode and be described
// in FD entries in the DATA DIVISION.
// If the output files (file-name-4, ...) contain variable-length records, the size of the
// records contained in file-name-1 must be neither less than the smallest record nor
// greater than the largest record described for the output files. If the output files
// contain fixed-length records, the size of the records contained in file-name-1 must
// not be greater than the largest record described for the output files. For more
// information, see Sorting and merging files in the Enterprise COBOL Programming
// Guide.
// At the time the MERGE statement is executed, the output files (file-name-4, ...) must
// not be open. The output files are automatically opened, read, and closed. If
// DECLARATIVE procedures are specified for these files for output operations, the
// declaratives will be driven for errors if errors occur.
// OUTPUT PROCEDURE phrase
// This phrase specifies the name of a procedure that is to select or modify output
// records from the merge operation.
// procedure-name-1
// Specifies the first (or only) section or paragraph in the OUTPUT
// PROCEDURE.
// procedure-name-2
// Identifies the last section or paragraph of the OUTPUT PROCEDURE.
// The OUTPUT PROCEDURE can consist of any procedure needed to select, modify,
// or copy the records that are made available one at time by the RETURN statement
// in merged order from the file referenced by file-name-1. The range includes all
// statements that are executed as the result of a transfer of control by CALL, EXIT,
// GO TO, PERFORM, and XML PARSE statements in the range of the output
// procedure. The range also includes all statements in declarative procedures that are
// executed as a result of the execution of statements in the range of the output
// procedure. The range of the output procedure must not cause the execution of any
// MERGE, RELEASE, or SORT statement.
// If an output procedure is specified, control passes to it after the file referenced by
// file-name-1 has been sequenced by the MERGE statement. The compiler inserts a
// return mechanism at the end of the last statement in the output procedure and
// when control passes the last statement in the output procedure, the return
// mechanism provides the termination of the merge and then passes control to the
// next executable statement after the MERGE statement. Before entering the output
// procedure, the merge procedure reaches a point at which it can select the next
// record in merged order when requested. The RETURN statements in the output
// procedure are the requests for the next record.
// The OUTPUT PROCEDURE phrase is similar to a basic PERFORM statement. For
// example, if you name a procedure in an OUTPUT PROCEDURE, that procedure is
// executed during the merging operation just as if it were named in a PERFORM
// statement. As with the PERFORM statement, execution of the procedure is
// terminated after the last statement completes execution. The last statement in an
// OUTPUT PROCEDURE can be the EXIT statement (see “EXIT statement” on page
// 335).
// MERGE special registers
// The topic describes special registers of the MERGE statement.
// SORT-CONTROL special register
// You identify the sort control file (through which you can specify additional
// options to the sort/merge function) with the SORT-CONTROL special
// register.
// If you use a sort control file to specify control statements, the values
// specified in the sort control file take precedence over those in the other
// SORT special registers.
// For information, see “SORT-CONTROL” on page 22.
// SORT-MESSAGE special register
// For information, see “SORT-MESSAGE” on page 23. The special register
// SORT-MESSAGE is equivalent to an option control statement keyword in
// the sort control file.
// SORT-RETURN special register
// For information, see “SORT-RETURN” on page 23.
// Segmentation considerations
// If a MERGE statement is coded in a fixed segment, any output procedure
// referenced by that MERGE statement must be either totally within a fixed segment
// or wholly contained in a single independent segment.
// If a MERGE statement is coded in an independent segment, any output procedure
// referenced by that MERGE statement must be either totally within a fixed segment
// or wholly contained within the same independent segment as that MERGE
// statement.

mergeStatement:
	MERGE fileNameReference onAscendingDescendingKey+
	collatingSequence?
	usingFilenames
	(givingFilenames | outputProcedure);

// Rules shared with sortStatement

collatingSequence:
	COLLATING? SEQUENCE IS? alphabetNameReference;

onAscendingDescendingKey: 
	ON? (ASCENDING | DESCENDING) KEY? qualifiedDataName+;

usingFilenames:  
	USING fileNameReference+;

givingFilenames: 
	GIVING fileNameReference+;
	
inputProcedure:  
	INPUT PROCEDURE IS? (procedureName | proceduresRange);

outputProcedure: 
	OUTPUT PROCEDURE IS? (procedureName | proceduresRange);

proceduresRange: 
	startProcedure=procedureName (THROUGH | THRU) endProcedure=procedureName;

// p369: MOVE statement
// The MOVE statement transfers data from one area of storage to one or more other
// areas.
//
// p369: Format 1: MOVE statement
// p369: Format 2: MOVE statement with CORRESPONDING phrase
//
// CORR is an abbreviation for, and is equivalent to, CORRESPONDING.
// identifier-1 , literal-1
// The sending area.
// identifier-2
// The receiving areas. identifier-2 must not reference an intrinsic function.
// When format 1 is specified:
// - All identifiers can reference alphanumeric group items, national group items, or
//   elementary items.
// - When one of identifier-1 or identifier-2 references a national group item and the
//   other operand references an alphanumeric group item, the national group is
//   processed as a group item; in all other cases, the national group item is
//   processed as an elementary data item of category national.
// - The data in the sending area is moved into the data item referenced by each
//   identifier-2 in the order in which the identifier-2 data items are specified in the
//   MOVE statement. See “Elementary moves” on page 370 and “Group moves” on
//   page 374 below.
// When format 2 is specified:
// - Both identifiers must be group items.
// - A national group item is processed as a group item (and not as an elementary
//   data item of category national).
// - Selected items in identifier-1 are moved to identifier-2 according to the rules for
//   the “CORRESPONDING phrase” on page 281. The results are the same as if
//   each pair of CORRESPONDING identifiers were referenced in a separate MOVE
//   statement.
// Data items described with the following types of usage cannot be specified in a
// MOVE statement:
// - INDEX
// - POINTER
// - FUNCTION-POINTER
// - PROCEDURE-POINTER
// - OBJECT REFERENCE
//  A data item defined with a usage of INDEX, POINTER, FUNCTION-POINTER,
// PROCEDURE-POINTER, or OBJECT REFERENCE can be part of an alphanumeric
// group item that is referenced in a MOVE CORRESPONDING statement; however,
// no movement of data from those data items takes place.
// The evaluation of the length of the sending or receiving area can be affected by the
// DEPENDING ON phrase of the OCCURS clause (see “OCCURS clause” on page
// 191).
// If the sending field (identifier-1) is reference-modified or subscripted, or is an
// alphanumeric or national function-identifier, the reference-modifier, subscript, or
// function is evaluated only once, immediately before data is moved to the first of
// the receiving operands.
// Any length evaluation, subscripting, or reference-modification associated with a
// receiving field (identifier-2) is evaluated immediately before the data is moved into
// that receiving field.
// For example, the result of the statement:
// MOVE A(B) TO B, C(B).
// is equivalent to:
// MOVE A(B) TO TEMP.
// MOVE TEMP TO B.
// MOVE TEMP TO C(B).
// where TEMP is defined as an intermediate result item. The subscript B has changed
// in value between the time that the first move took place and the time that the final
// move to C(B) is executed.
// For further information about intermediate results, see Appendix A. Intermediate
// results and arithmetic precision in the Enterprise COBOL Programming Guide.
// After execution of a MOVE statement, the sending fields contain the same data as
// before execution.
// Usage note: Overlapping operands in a MOVE statement can cause unpredictable
// results.
// ... more details p370->374 Elementary moves ...
// ... more details p374->375 Group moves ...

moveStatement: 
	moveSimple | moveCorresponding;

moveSimple:
	MOVE variable7 TO storageArea1+;

moveCorresponding:
	MOVE (CORRESPONDING | CORR) fromGroupItem=dataItemReference TO toGroupItem=dataItemReference;

// p376: MULTIPLY statement
// The MULTIPLY statement multiplies numeric items and sets the values of data
// items equal to the results.

multiplyStatement:
	multiplySimple | multiplyGiving;

multiplyStatementEnd: END_MULTIPLY;

// p376: Format 1: MULTIPLY statement
// In format 1, the value of identifier-1 or literal-1 is multiplied by the value of
// identifier-2; the product is then placed in identifier-2. For each successive occurrence
// of identifier-2, the multiplication takes place in the left-to-right order in which
// identifier-2 is specified.

multiplySimple:
	MULTIPLY numericVariable3 BY numericStorageAreaRounded+;

// p377: Format 2: MULTIPLY statement with GIVING phrase
// In format 2, the value of identifier-1 or literal-1 is multiplied by the value of
// identifier-2 or literal-2. The product is then stored in the data items referenced by
// identifier-3.

multiplyGiving:
	MULTIPLY numericVariable3 BY byOperand=numericVariable3 GIVING numericStorageAreaRounded+;

// For all formats:
// identifier-1 , identifier-2
// Must name an elementary numeric item.
// literal-1 , literal-2
// Must be a numeric literal.
// For format-2:
// identifier-3
// Must name an elementary numeric or numeric-edited item.
// Floating-point data items and literals can be used anywhere a numeric data item or
// literal can be specified.
// When the ARITH(COMPAT) compiler option is in effect, the composite of operands
// can contain a maximum of 30 digits. When the ARITH(EXTEND) compiler option
// is in effect, the composite of operands can contain a maximum of 31 digits. For
// more information, see “Arithmetic statement operands” on page 284 and the
// details on arithmetic intermediate results, Appendix A. Intermediate results and
// arithmetic precision in the Enterprise COBOL Programming Guide.
// ROUNDED phrase
// For formats 1 and 2, see “ROUNDED phrase” on page 282.
// Chapter 20. PROCEDURE DIVISION statements 377
// SIZE ERROR phrases
// For formats 1 and 2, see “SIZE ERROR phrases” on page 283.
// END-MULTIPLY phrase
// This explicit scope terminator serves to delimit the scope of the MULTIPLY
// statement. END-MULTIPLY permits a conditional MULTIPLY statement to be
// nested in another conditional statement. END-MULTIPLY can also be used with an
// imperative MULTIPLY statement.
// For more information, see “Delimited scope statements” on page 280. 

// p379: OPEN statement
// The OPEN statement initiates the processing of files. It also checks or writes labels,
// or both. 
//
// p379: Format 1: OPEN statement for sequential files
// Notes: The REVERSED and WITH NO REWIND phrases are not valid for VSAM
// files.
// p379: Format 2: OPEN statement for indexed and relative files
// p380: Format 3: OPEN statement for line-sequential files
//
// The phrases INPUT, OUTPUT, I-O, and EXTEND specify the mode to be used for
// opening the file. At least one of the phrases INPUT, OUTPUT, I-O, or EXTEND
// must be specified with the OPEN keyword. The INPUT, OUTPUT, I-O, and
// EXTEND phrases can appear in any order.
// INPUT
// Permits input operations.
// OUTPUT
// Permits output operations. This phrase can be specified when the file is
// being created.
// Do not specify OUTPUT for files that:
// - Contain records. The file will be replaced by new data.
//   If the OUTPUT phrase is specified for a file that already contains
//   records, the data set must be defined as reusable and cannot have an
//   alternate index. The records in the file will be replaced by the new data
//   and any ALTERNATE RECORD KEY clause in the SELECT statement
//   will be ignored.
// - Are defined with a DD dummy card. Unpredictable results can occur.
// I-O 
// Permits both input and output operations. The I-O phrase can be specified
// only for files assigned to direct access devices.
// The I-O phrase is not valid for line-sequential files.
// EXTEND
// Permits output operations that append to or create a file.
// The EXTEND phrase is allowed for sequential access files only if the new
// data is written in ascending sequence. The EXTEND phrase is allowed for
// files that specify the LINAGE clause.
// For QSAM files, do not specify the EXTEND phrase for a multiple file reel.
// If you want to append to a file, but are unsure if the file exists, use the
// SELECT OPTIONAL clause before opening the file in EXTEND mode. The
// file will be created or appended to, depending on whether the file exists.
// file-name-1, file-name-2, file-name-3, file-name-4
// Designate a file upon which the OPEN statement is to operate. If more
// than one file is specified, the files need not have the same organization or
// access mode. Each file-name must be defined in an FD entry in the DATA
// DIVISION and must not name a sort or merge file. The FD entry must be
// equivalent to the information supplied when the file was defined.
// REVERSED
// Valid only for sequential single-reel files. REVERSED is not valid for
// VSAM files.
// If the concept of reels has no meaning for the storage medium (for
// example, a direct access device), the REVERSED and NO REWIND phrases
// do not apply.
// NO REWIND
// Valid only for sequential single-reel files. It is not valid for VSAM files.
// For information on file sizes, see Appendix B, “Compiler limits,” on page 565.
// ... more details p381 General rules ...
// ... more details p382->383 OPEN statement notes ...

openStatement:
	OPEN (openInput | openOutput | openIO | openExtend)+;

openInput: 
	INPUT fileNameWithNoRewindOrReversed+;

openOutput: 
	OUTPUT fileNameWithNoRewindOrReversed+;

openIO: 
	I_O fileNameReference+;

openExtend: 
	EXTEND fileNameReference+;

fileNameWithNoRewindOrReversed: 
	fileNameReference ((WITH? NO REWIND) | REVERSED)?;

// p384: PERFORM statement
// The PERFORM statement transfers control explicitly to one or more procedures
// and implicitly returns control to the next executable statement after execution of
// the specified procedures is completed.
// The PERFORM statement is:
//  An out-of-line PERFORM statement
//   When procedure-name-1 is specified.
//  An in-line PERFORM statement
//   When procedure-name-1 is omitted.
// An in-line PERFORM must be delimited by the END-PERFORM phrase.
// The in-line and out-of-line formats cannot be combined. For example, if
// procedure-name-1 is specified, imperative statements and the
// END-PERFORM phrase must not be specified.

// * Basic PERFORM statement
// The procedures referenced in the basic PERFORM statement are executed once,
// and control then passes to the next executable statement following the PERFORM
// statement.
// Note: A PERFORM statement must not cause itself to be executed. A recursive
// PERFORM statement can cause unpredictable results.
// p384: Format 1: Basic PERFORM statement

// procedure-name-1 , procedure-name-2
// Must name a section or paragraph in the procedure division.
// When both procedure-name-1 and procedure-name-2 are specified, if either is a
// procedure-name in a declarative procedure, both must be procedure-names
// in the same declarative procedure.
// If procedure-name-1 is specified, imperative-statement-1 and the
// END-PERFORM phrase must not be specified.
// If procedure-name-1 is omitted, imperative-statement-1 and the
// END-PERFORM phrase must be specified.
// imperative-statement-1
// The statements to be executed for an in-line PERFORM
// An in-line PERFORM statement functions according to the same general rules as
// an otherwise identical out-of-line PERFORM statement, except that statements
// contained within the in-line PERFORM are executed in place of the statements
// contained within the range of procedure-name-1 (through procedure-name-2, if
// specified). Unless specifically qualified by the word in-line or the word out-of-line,
// all the rules that apply to the out-of-line PERFORM statement also apply to the
// in-line PERFORM.
// Whenever an out-of-line PERFORM statement is executed, control is transferred to
// the first statement of the procedure named procedure-name-1. Control is always
// returned to the statement following the PERFORM statement. The point from
// which this control is returned is determined as follows:
// - If procedure-name-1 is a paragraph name and procedure-name-2 is not specified, the
//   return is made after the execution of the last statement of the procedure-name-1
//   paragraph.
// - If procedure-name-1 is a section name and procedure-name-2 is not specified, the
//   return is made after the execution of the last statement of the last paragraph in
//   the procedure-name-1 section.
// - If procedure-name-2 is specified and it is a paragraph name, the return is made
//   after the execution of the last statement of the procedure-name-2 paragraph.
// - If procedure-name-2 is specified and it is a section name, the return is made after
//   the execution of the last statement of the last paragraph in the procedure-name-2
//   section.
// The only necessary relationship between procedure-name-1 and procedure-name-2 is
// that a consecutive sequence of operations is executed, beginning at the procedure
// named by procedure-name-1 and ending with the execution of the procedure named
// by procedure-name-2.
// PERFORM statements can be specified within the performed procedure. If there
// are two or more logical paths to the return point, then procedure-name-2 can name a
// paragraph that consists only of an EXIT statement; all the paths to the return point
// must then lead to this paragraph.
// When the performed procedures include another PERFORM statement, the
// sequence of procedures associated with the embedded PERFORM statement must
// be totally included in or totally excluded from the performed procedures of the
// first PERFORM statement. That is, an active PERFORM statement whose execution
// point begins within the range of performed procedures of another active
// PERFORM statement must not allow control to pass through the exit point of the
// other active PERFORM statement. However, two or more active PERFORM
// statements can have a common exit.
// The following figure illustrates valid sequences of execution for PERFORM
// statements.
// ... see figure p386 ...
// When control passes to the sequence of procedures by means other than a
// PERFORM statement, control passes through the exit point to the next executable
// statement, as if no PERFORM statement referred to these procedures.
// END-PERFORM
// Delimits the scope of the in-line PERFORM statement. Execution of an in-line
// PERFORM is completed after the last statement contained within it has been
// executed.

performStatement:
	PERFORM (procedureName | proceduresRange)?
	( performTimesPhrase   |
	  performUntilPhrase   |
	  performVaryingPhrase )?;

performStatementEnd: END_PERFORM;

// * PERFORM with TIMES phrase
// The procedures referred to in the TIMES phrase of the PERFORM statement are
// executed the number of times specified by the value in identifier-1 or integer-1, up
// to a maximum of 999,999,999 times. Control then passes to the next executable
// statement following the PERFORM statement.
// p386: Format 2: PERFORM statement with TIMES phrase

// If procedure-name-1 is specified, imperative-statement-1 and the END-PERFORM
// phrase must not be specified.
// identifier-1
// Must name an integer item.
// If identifier-1 is zero or a negative number at the time the PERFORM
// statement is initiated, control passes to the statement following the
// PERFORM statement.
// After the PERFORM statement has been initiated, any change to identifier-1
// has no effect in varying the number of times the procedures are initiated.
// integer-1
// Can be a positive signed integer.

performTimesPhrase:
	numericVariable3 TIMES;

// * PERFORM with UNTIL phrase
// In the UNTIL phrase format, the procedures referred to are performed until the
// condition specified by the UNTIL phrase is true. Control is then passed to the next
// executable statement following the PERFORM statement.
// p387: Format 3: PERFORM statement with UNTIL phrase

// If procedure-name-1 is specified, imperative-statement-1 and the END-PERFORM
// phrase must not be specified.
// condition-1
// Can be any condition described under “Conditional expressions” on page
// initiated, the specified procedures are not executed.
// Any subscripting associated with the operands specified in condition-1 is
// evaluated each time the condition is tested.
// If the TEST BEFORE phrase is specified or assumed, the condition is tested before
// any statements are executed (corresponds to DO WHILE).
// If the TEST AFTER phrase is specified, the statements to be performed are
// executed at least once before the condition is tested (corresponds to DO UNTIL).
// In either case, if the condition is true, control is transferred to the next executable
// statement following the end of the PERFORM statement. If neither the TEST
// BEFORE nor the TEST AFTER phrase is specified, the TEST BEFORE phrase is
// assumed.

performUntilPhrase:
	conditionTestTime? UNTIL conditionalExpression;

conditionTestTime:
	WITH? TEST (BEFORE | AFTER);

// * PERFORM with VARYING phrase
// The VARYING phrase increases or decreases the value of one or more identifiers or
// index-names, according to certain rules.
// For more information, see “Varying phrase rules” on page 393.
// The format-4 VARYING phrase PERFORM statement can serially search an entire
// seven-dimensional table.
// p388: Format 4: PERFORM statement with VARYING phrase

// If procedure-name-1 is specified, imperative-statement-1 and the END-PERFORM
// phrase must not be specified. If procedure-name-1 is omitted, the AFTER phrase
// must not be specified.
// identifier-2 through identifier-7
// Must name a numeric elementary item.
// literal-1 through literal-4
// Must represent a numeric literal.
// condition-1, condition-2
// Can be any condition described under “Conditional expressions” on page
// 256. If the condition is true at the time the PERFORM statement is
// initiated, the specified procedures are not executed.
// After the conditions specified in the UNTIL phrase are satisfied, control is
// passed to the next executable statement following the PERFORM
// statement.
// If any of the operands specified in condition-1 or condition-2 is subscripted,
// reference modified, or is a function-identifier, the subscript,
// reference-modifier, or function is evaluated each time the condition is
// tested.
// Floating-point data items and literals can be used anywhere a numeric data item or
// literal can be specified.
// When TEST BEFORE is indicated, all specified conditions are tested before the first
// execution, and the statements to be performed are executed, if at all, only when all
// specified tests fail. When TEST AFTER is indicated, the statements to be performed
// are executed at least once, before any condition is tested.
// If neither the TEST BEFORE nor the TEST AFTER phrase is specified, the TEST
// BEFORE phrase is assumed.
// ... more details p389->392 Varying identifiers ...
// Varying phrase rules:
// There are certain rules that apply to this phrase, no matter how many variables are
// specified.
// The rules are:
// - In the VARYING or AFTER phrases, when an index-name is specified:
//   – The index-name is initialized and incremented or decremented according to
//     the rules under “INDEX phrase” on page 233. (See also “SET statement” on
//     page 415.)
//   – In the associated FROM phrase, an identifier must be described as an integer
//     and have a positive value; a literal must be a positive integer.
//   – In the associated BY phrase, an identifier must be described as an integer; a
//     literal must be a nonzero integer.
// - In the FROM phrase, when an index-name is specified:
//   – In the associated VARYING or AFTER phrase, an identifier must be described
//     as an integer. It is initialized as described in the SET statement.
//   – In the associated BY phrase, an identifier must be described as an integer and
//     have a nonzero value; a literal must be a nonzero integer.
// - In the BY phrase, identifiers and literals must have nonzero values.
// - Changing the values of identifiers or index-names in the VARYING, FROM, and
//   BY phrases during execution changes the number of times the procedures are
//   executed.

performVaryingPhrase:
	conditionTestTime? 
	 VARYING loopVariableDescription 
	(AFTER   loopVariableDescription)*;

loopVariableDescription:
	loopVariable=dataOrIndexStorageArea 
	FROM initialValue=numericVariableOrIndex
	BY increment=numericVariable3 
	UNTIL conditionalExpression;

// p393: READ statement
// For sequential access, the READ statement makes the next logical record from a file
// available to the object program. For random access, the READ statement makes a
// specified record from a direct-access file available to the object program.
// When the READ statement is executed, the associated file must be open in INPUT
// or I-O mode.
// p394: Format 1: READ statement for sequential retrieval
// p394: Format 2: READ statement for random retrieval
// file-name-1
// Must be defined in a DATA DIVISION FD entry.
// NEXT RECORD
// Reads the next record in the logical sequence of records. NEXT is optional
// when the access mode is sequential, and has no effect on READ statement
// execution.
// You must specify the NEXT RECORD phrase to retrieve records
// sequentially from files in dynamic access mode.
// INTO  identifier-1
// identifier-1 is the receiving field.
// identifier-1 must be a valid receiving field for the selected sending record
// description entry in accordance with the rules of the MOVE statement.
// The record areas associated with file-name-1 and identifier-1 must not be the
// same storage area.
// When there is only one record description associated with file-name-1 or all
// the records and the data item referenced by identifier-1 describe an
// elementary alphanumeric item or an alphanumeric group item, the result
// of the execution of a READ statement with the INTO phrase is equivalent
// to the application of the following rules in the order specified:
// - The execution of the same READ statement without the INTO phrase.
// - The current record is moved from the record area to the area specified
//   by identifier-1 according to the rules for the MOVE statement without the
//   CORRESPONDING phrase. The size of the current record is determined
//   by rules specified for the RECORD clause. If the file description entry
//   contains a RECORD IS VARYING clause, the implied move is a group
//   move. The implied MOVE statement does not occur if the execution of
//   the READ statement was unsuccessful. Any subscripting or reference
//   modification associated with identifier-1 is evaluated after the record has
//   been read and immediately before it is moved to the data item. The
//   record is available in both the record area and the data item referenced
//   by identifier-1.
// When there are multiple record descriptions associated with file-name-1 and
// they do not all describe an alphanumeric group item or elementary
// alphanumeric item, the following rules apply:
// 1. If the file referenced by file-name-1 is described as containing
//    variable-length records, or as a QSAM file with RECORDING MODE 'S'
//    or 'U', a group move will take place.
// 2. If the file referenced by file-name-1 is described as containing
//    fixed-length records, a move will take place according to the rules for a
//    MOVE statement using, as a sending field description, the record that
//    specifies the largest number of character positions. If more than one
//    such record exists, the sending field record selected will be the one
//    among those records that appears first under the description of
//    file-name-1.
// KEY IS phrase
// The KEY IS phrase can be specified only for indexed files. data-name-1 must
// identify a record key associated with file-name-1. data-name-1 can be qualified; it
// cannot be subscripted.
// AT END phrases
// For sequential access, both the AT END phrase and an applicable
// EXCEPTION/ERROR procedure can be omitted.
// For information about at-end condition processing, see AT END condition.
// INVALID KEY phrases
// Both the INVALID KEY phrase and an applicable EXCEPTION/ERROR procedure
// can be omitted.
// For information about INVALID KEY phrase processing, see “Invalid key
// condition” on page 290.
// END-READ phrase
// This explicit scope terminator serves to delimit the scope of the READ statement.
// END-READ permits a conditional READ statement to be nested in another
// conditional statement. END-READ can also be used with an imperative READ
// statement. For more information, see “Delimited scope statements” on page 280.
// ... more details p396 : Processing files with variable length records or multiple record descriptions ...
// ... more details p396->398 : Sequential access mode ...
// ... more details p398->399 : Random access mode ...
// ... more details p399->400 : READ statement notes ...

readStatement:
	READ fileNameReference 
	NEXT? RECORD? (INTO storageArea1)? 
	(KEY IS? qualifiedDataName)?;

readStatementEnd: END_READ;

// p401: RELEASE statement
// The RELEASE statement transfers records from an input/output area to the initial
// phase of a sorting operation.
// The RELEASE statement can be used only within the range of an INPUT
// PROCEDURE associated with a SORT statement.
// Within an INPUT PROCEDURE, at least one RELEASE statement must be
// specified.
// When the RELEASE statement is executed, the current contents of record-name-1 are
// placed in the sort file. This makes the record available to the initial phase of the
// sorting operation.
// record-name-1
// Must specify the name of a logical record in a sort-merge file description
// entry (SD). record-name-1 can be qualified.
// FROM phrase
// The result of the execution of the RELEASE statement with the FROM
// identifier-1 phrase is equivalent to the execution of the following statements
// in the order specified.
// MOVE identifier-1 to record-name-1.
// RELEASE record-name-1.
// The MOVE is performed according to the rules for the MOVE statement
// without the CORRESPONDING phrase.
// identifier-1
// identifier-1 must reference one of the following items:
// - An entry in the WORKING-STORAGE SECTION, the LOCAL-STORAGE
//   SECTION, or the LINKAGE SECTION
// - A record description for another previously opened file
// - An alphanumeric or national function.
// identifier-1 must be a valid sending item with record-name-1 as the receiving
// item in accordance with the rules of the MOVE statement.
// identifier-1 and record-name-1 must not refer to the same storage area.
// After the RELEASE statement is executed, the information is still available
// in identifier-1. (See “INTO and FROM phrases” on page 291 under
// "Common processing facilities".)
// If the RELEASE statement is executed without specifying the SD entry for
// file-name-1 in a SAME RECORD AREA clause, the information in record-name-1 is
// no longer available.
// If the SD entry is specified in a SAME RECORD AREA clause, record-name-1 is still
// available as a record of the other files named in that clause.
// When FROM identifier-1 is specified, the information is still available in identifier-1.
// When control passes from the INPUT PROCEDURE, the sort file consists of all
// those records placed in it by execution of RELEASE statements.

releaseStatement:
	RELEASE recordName (FROM variable1)?;

// record-name-1
// Must specify the name of a logical record in a sort-merge file description
// entry (SD). record-name-1 can be qualified.

// p403: RETURN statement
// The RETURN statement transfers records from the final phase of a sorting or
// merging operation to an OUTPUT PROCEDURE.
// The RETURN statement can be used only within the range of an OUTPUT
// PROCEDURE associated with a SORT or MERGE statement.
// Within an OUTPUT PROCEDURE, at least one RETURN statement must be
// specified.
// When the RETURN statement is executed, the next record from file-name-1 is made
// available for processing by the OUTPUT PROCEDURE.
// file-name-1
// Must be described in a DATA DIVISION SD entry.
// If more than one record description is associated with file-name-1, those
// records automatically share the same storage; that is, the area is implicitly
// redefined. After RETURN statement execution, only the contents of the
// current record are available. If any data items lie beyond the length of the
// current record, their contents are undefined.
// INTO phrase
// When there is only one record description associated with file-name-1 or all
// the records and the data item referenced by identifier-1 describe an
// elementary alphanumeric item or an alphanumeric group item, the result
// of the execution of a RETURN statement with the INTO phrase is
// equivalent to the application of the following rules in the order specified:
// - The execution of the same RETURN statement without the INTO phrase.
// - The current record is moved from the record area to the area specified
//   by identifier-1 according to the rules for the MOVE statement without the
//   CORRESPONDING phrase. The size of the current record is determined
//   by rules specified for the RECORD clause. If the file description entry
//   contains a RECORD IS VARYING clause, the implied move is a group
//   move. The implied MOVE statement does not occur if the execution of
//   the RETURN statement was unsuccessful. Any subscripting or reference
//   modification associated with identifier-1 is evaluated after the record has
//   been read and immediately before it is moved to the data item. The
//   record is available in both the record area and the data item referenced
//   by identifier-1.
// When there are multiple record descriptions associated with file-name-1 and
// they do not all describe an alphanumeric group item or elementary
// alphanumeric item, the following rules apply:
// 1. If the file referenced by file-name-1 contains variable-length records, a
//    group move takes place.
// 2. If the file referenced by file-name-1 contains fixed-length records, a
//    move takes place according to the rules for a MOVE statement using,
//    as a sending field description, the record that specifies the largest
//    number of character positions. If more than one such record exists, the
//    sending field record selected will be the one among those records that
//    appears first under the description of file-name-1.
// identifier-1 must be a valid receiving field for the selected sending record
// description entry in accordance with the rules of the MOVE statement.
// The record areas associated with file-name-1 and identifier-1 must not be the same
// storage area.
// AT END phrases
// The imperative-statement specified on the AT END phrase executes after all
// records have been returned from file-name-1. No more RETURN statements can be
// executed as part of the current output procedure.
// If an at-end condition does not occur during the execution of a RETURN
// statement, then after the record is made available and after executing any implicit
// move resulting from the presence of an INTO phrase, control is transferred to the
// imperative statement specified by the NOT AT END phrase. If an at-end condition
// does occur, control is transferred to the end of the RETURN statement.
// END-RETURN phrase
// This explicit scope terminator serves to delimit the scope of the RETURN
// statement. END-RETURN permits a conditional RETURN statement to be nested in
// another conditional statement. END-RETURN can also be used with an imperative
// RETURN statement.
// For more information, see “Delimited scope statements” on page 280.

returnStatement:
	RETURN fileNameReference RECORD? (INTO storageArea1)?;

returnStatementEnd: END_RETURN;

// p405: REWRITE statement
// The REWRITE statement logically replaces an existing record in a direct-access file.
// When the REWRITE statement is executed, the associated direct-access file must be
// open in I-O mode.
// The REWRITE statement is not supported for line-sequential files.
// record-name-1
// Must be the name of a logical record in a DATA DIVISION FD entry. The
// record-name can be qualified.
// FROM phrase
// The result of the execution of the REWRITE statement with the FROM
// identifier-1 phrase is equivalent to the execution of the following statements
// in the order specified.
// MOVE identifier-1 TO record-name-1.
// REWRITE record-name-1
// The MOVE is performed according to the rules for the MOVE statement
// without the CORRESPONDING phrase.
// identifier-1
// identifier-1 can reference one of the following items:
// - A record description for another previously opened file
// - An alphanumeric or national function
// - A data item defined in the WORKING-STORAGE SECTION, the
//   LOCAL-STORAGE SECTION, or the LINKAGE SECTION
// identifier-1 must be a valid sending item with record-name-1 as the receiving
// item in accordance with the rules of the MOVE statement.
// identifier-1 and record-name-1 must not refer to the same storage area.
// After the REWRITE statement is executed, the information is still available
// in identifier-1 (“INTO and FROM phrases” on page 291 under "Common
// processing facilities").
// INVALID KEY phrases
// An INVALID KEY condition exists when:
// - The access mode is sequential, and the value contained in the prime RECORD
//   KEY of the record to be replaced does not equal the value of the prime RECORD
//   KEY data item of the last-retrieved record from the file
// - The value contained in the prime RECORD KEY does not equal that of any
//   record in the file
// - The value of an ALTERNATE RECORD KEY data item for which DUPLICATES
//   is not specified is equal to that of a record already in the file
// For details of invalid key processing, see Invalid key condition.
// END-REWRITE phrase
// This explicit scope terminator serves to delimit the scope of the REWRITE
// statement. END-REWRITE permits a conditional REWRITE statement to be nested
// in another conditional statement. END-REWRITE can also be used with an
// imperative REWRITE statement.
// For more information, see “Delimited scope statements” on page 280.
// ... more details p406->407 Reusing a logical record, Sequential / Indexed / Relative files ...

rewriteStatement:
	REWRITE recordName (FROM sendingField=variable1)?;

rewriteStatementEnd: END_REWRITE;

// p408: SEARCH statement
// The SEARCH statement searches a table for an element that satisfies the specified
// condition and adjusts the associated index to indicate that element.
//
// p408: Format 1: SEARCH statement for serial search
// p408: Format 2: SEARCH statement for binary search
// 
// Use format 1 (serial search) when the table that you want to search has not been
// sorted. Use format 1 to search a sorted table when you want to search serially
// through the table or you want to control subscripts or indexes.
// Use format 2 (binary search) when you want to efficiently search across all
// occurrences in a table. The table must previously have been sorted.
// AT END and WHEN phrases
// After imperative-statement-1 or imperative-statement-2 is executed, control passes to
// the end of the SEARCH statement, unless imperative-statement-1 or
// imperative-statement-2 ends with a GO TO statement.
// The function of the AT END phrase is the same for a serial search and a binary
// search.
// NEXT SENTENCE
// NEXT SENTENCE transfers control to the first statement following the closest
// separator period.
// When NEXT SENTENCE is specified with END-SEARCH, control does not pass to
// the statement following the END-SEARCH. Instead, control passes to the statement
// after the closest following period.
// For the format-2 SEARCH ALL statement, neither imperative-statement-2 nor NEXT
// SENTENCE is required. Without them, the SEARCH statement sets the index to
// the value in the table that matched the condition.
// The function of the NEXT SENTENCE phrase is the same for a serial search and a
// binary search.
// END-SEARCH phrase
// This explicit scope terminator delimits the scope of the SEARCH statement.
// END-SEARCH permits a conditional SEARCH statement to be nested in another
// conditional statement.
// For more information, see “Delimited scope statements” on page 280.
// The function of END-SEARCH is the same for a serial search and a binary search.
// ... more details p409->412 Serial search ...
// ... more details p412->414 Binary search ...
// ... more details p414 Search statement considerations ...

searchStatement: serialSearch | binarySearch;

serialSearch: SEARCH variable1 (VARYING dataOrIndexStorageArea)?;
binarySearch: SEARCH ALL variable1;

whenSearchCondition: WHEN conditionalExpression;

// whenSearchCondition must be declared BEFORE whenCondition,
// because the latter is a general case of the former, thus
// if we leave it there whenSearchCondition will never be detected
// and we'll get ambiguity at ProgramClass phase [issue #285]

// IMPORTANT :
// The more restrictive syntax for binary search can not be distinguished 
// from the full syntax allowed for serial search at this parsing stage.
// We need to check the following restriction at the second parsing stage :
//
// whenBinarySearchCondition :
//     WHEN binarySearchCondition (AND searchCondition)*
//
// binarySearchCondition:
//     (variable2 IS? ((EQUAL TO?) | EqualOperator) variableOrExpression2) | 
//     conditionNameConditionOrSwitchStatusCondition;

searchStatementEnd: END_SEARCH;

// p415: SET statement
// The SET statement is used to perform an operation as described in this topic.
// The operations are:
// - Placing values associated with table elements into indexes associated with
//   index-names
// - Incrementing or decrementing an occurrence number
// - Setting the status of an external switch to ON or OFF
// - Moving data to condition names to make conditions true
// - Setting USAGE POINTER data items to a data address
// - Setting USAGE PROCEDURE-POINTER data items to an entry address
// - Setting USAGE FUNCTION-POINTER data items to an entry address
// - Setting USAGE OBJECT REFERENCE data items to refer to an object instance
// Index-names are related to a given table through the INDEXED BY phrase of the
// OCCURS clause; they are not further defined in the program.
// When the sending and receiving fields in a SET statement share part of their
// storage (that is, the operands overlap), the result of the execution of that SET
// statement is undefined.
//
// p415: Format 1: SET for basic table handling
// When this form of the SET statement is executed, the current value of the receiving
// field is replaced by the value of the sending field (with conversion).
// index-name-1
// Receiving field.
// Must name an index that is specified in the INDEXED BY phrase of an
// OCCURS clause.
// identifier-1
// Receiving field.
// Must name either an index data item or an elementary numeric integer
// item.
// index-name-2
// Sending field.
// Must name an index that is specified in the INDEXED BY phrase of an
// OCCURS clause. The value of the index before the SET statement is
// executed must correspond to an occurrence number of its associated table.
// identifier-2
// Sending field.
// Must name either an index data item or an elementary numeric integer
// item.
// integer-1
// Sending field.
// Must be a positive integer.
// The following table shows valid combinations of sending and receiving fields in a
// format-1 SET statement.
// ... more information p416: Table 47. Sending and receiving fields for format-1 SET statement ...
// Receiving fields are acted upon in the left-to-right order in which they are
// specified. Any subscripting or indexing associated with identifier-1 is evaluated
// immediately before that receiving field is acted upon.
// The value used for the sending field is the value at the beginning of SET statement
// execution.
// The value of an index after execution of a SEARCH or PERFORM statement can be
// undefined; therefore, use a format-1 SET statement to reinitialize such indexes
// before you attempt other table-handling operations.
// If index-name-2 is for a table that has a subordinate item that contains an OCCURS
// DEPENDING ON clause, then undefined values can be received into identifier-1.
// For more information about complex OCCURS DEPENDING ON, see Complex
// OCCURS DEPENDING ON in the Enterprise COBOL Programming Guide.
//
// p416: Format 2: SET for adjusting indexes
// When this form of the SET statement is executed, the value of the receiving index
// is increased (UP BY) or decreased (DOWN BY) by a value that corresponds to the
// value in the sending field.
// The receiving field is an index specified by index-name-3. The index value both
// before and after the SET statement execution must correspond to an occurrence
// number in an associated table.
// The sending field can be specified as identifier-3, which must be an elementary
// integer data item, or as integer-2, which must be a nonzero integer.
// When the format-2 SET statement is executed, the contents of the receiving field
// are increased (UP BY) or decreased (DOWN BY) by a value that corresponds to the
// number of occurrences represented by the value of identifier-3 or integer-2.
// Receiving fields are acted upon in the left-to-right order in which they are
// specified. The value of the incrementing or decrementing field at the beginning of
// SET statement execution is used for all receiving fields.
// If index-name-3 is for a table that has a subordinate item that contains an OCCURS
// DEPENDING ON clause, and if the ODO object is changed before executing a
// format-2 SET Statement, then index-name-3 cannot contain a value that corresponds
// to an occurrence number of its associated table.
// For more information about complex OCCURS DEPENDING ON, see Complex
// OCCURS DEPENDING ON in the Enterprise COBOL Programming Guide.
//
// p417: Format 3: SET for external switches
// When this form of the SET statement is executed, the status of each external switch
// associated with the specified mnemonic-name is turned ON or OFF.
// mnemonic-name-1
// Must be associated with an external switch, the status of which can be
// altered.
//
// p417: Format 4: SET for condition-names
// When this form of the SET statement is executed, the value associated with a
// condition-name is placed in its conditional variable according to the rules of the
// VALUE clause.
// condition-name-1
// Must be associated with a conditional variable.
// If more than one literal is specified in the VALUE clause of condition-name-1, its
// associated conditional variable is set equal to the first literal.
// If multiple condition-names are specified, the results are the same as if a separate
// SET statement had been written for each condition-name in the same order in
// which they are specified in the SET statement.
//
// p418: Format 5: SET for USAGE IS POINTER data items
// When this form of the SET statement is executed, the current value of the receiving
// field is replaced by the address value contained in the sending field.
// identifier-4
// Receiving fields.
// Must be described as USAGE IS POINTER.
// ADDRESS OF identifier-5
// Receiving fields.
// identifier-5 must be level-01 or level-77 items defined in the LINKAGE
// SECTION. The addresses of these items are set to the value of the operand
// specified in the TO phrase.
// identifier-5 must not be reference-modified.
// identifier-6
// Sending field.
// Must be described as USAGE IS POINTER.
// ADDRESS OF identifier-7
// Sending field. identifier-7 must name an item of any level except 66 or 88 in
// the LINKAGE SECTION, the WORKING-STORAGE SECTION, or the
// LOCAL-STORAGE SECTION. ADDRESS OF identifier-7 contains the
// address of the identifier, and not the content of the identifier.
// NULL, NULLS
// Sending field.
// Sets the receiving field to contain the value of an invalid address.
// The following table shows valid combinations of sending and receiving fields in a
// format-5 SET statement.
// Table 48. Sending and receiving fields for format-5 SET statement
//
// p419: Format 6: SET for procedure-pointer and function-pointer data items
// When this format of the SET statement is executed, the current value of the
// receiving field is replaced by the address value specified by the sending field.
// At run time, function-pointers and procedure-pointers can reference the address of
// the primary entry point of a COBOL program, an alternate entry point in a
// COBOL program, or an entry point in a non-COBOL program; or they can be
// NULL.
// COBOL function-pointers are more easily used than procedure-pointers for
// interoperation with C functions.
// procedure-pointer-data-item-1 , procedure-pointer-data-item-2
// Must be described as USAGE IS PROCEDURE-POINTER.
// procedure-pointer-data-item-1 is a receiving field; procedure-pointer-data-item-2
// is a sending field.
// function-pointer-data-item-1 , function-pointer-data-item-2
// Must be described as USAGE IS FUNCTION-POINTER.
// function-pointer-data-item-1 is a receiving field; function-pointer-data-item-2 is
// a sending field.
// identifier-8
// Must be defined as an alphabetic or alphanumeric item such that the value
// can be a program name. For more information, see “PROGRAM-ID
// paragraph” on page 100. For entry points in non-COBOL programs,
// identifier-8 can contain the characters @, #, and, $.
// literal-1
// Must be alphanumeric and must conform to the rules for formation of
// program-names. For details on formation rules, see the discussion of
// program-name under “PROGRAM-ID paragraph” on page 100.
// identifier-8 or literal-1 must refer to one of the following types of entry
// points:
// - The primary entry point of a COBOL program as defined by the
//   PROGRAM-ID paragraph. The PROGRAM-ID must reference the
//   outermost program of a compilation unit; it must not reference a nested
//   program.
// - An alternate entry point of a COBOL program as defined by a COBOL
//   ENTRY statement.
// - An entry point in a non-COBOL program.
// The program-name referenced by the SET ... TO ENTRY statement can be
// affected by the PGMNAME compiler option. For details, see PGMNAME in
// the Enterprise COBOL Programming Guide.
// NULL, NULLS
// Sets the receiving field to contain the value of an invalid address.
// pointer-data-item-3
// Must be defined with USAGE POINTER. You must set pointer-data-item-3 in
// a non-COBOL program to point to a valid program entry point.
// Example of COBOL/C interoperability
// The following example demonstrates a COBOL CALL to a C function that returns
// a function-pointer to a service, followed by a COBOL CALL to the service:
// IDENTIFICATION DIVISION.
// PROGRAM-ID DEMO.
// DATA DIVISION.
// WORKING-STORAGE SECTION.
// 01 FP USAGE FUNCTION-POINTER.
// PROCEDURE DIVISION.
// CALL "c-function" RETURNING FP.
// CALL FP.
//
// p420: Format 7: SET for USAGE OBJECT REFERENCE data items
// When this format of the SET statement is executed the value in the receiving item
// is replaced by the value in the sending item.
// object-reference-id-1 and object-reference-id-2 must be defined as USAGE OBJECT
// REFERENCE. object-reference-id-1 is the receiving item and object-reference-id-2 is the
// sending item. If object-reference-id-1 is defined as an object reference of a certain
// class (defined as "USAGE OBJECT REFERENCE class-name"), object-reference-id-2
// must be an object reference of the same class or a class derived from that class.
// If the figurative constant NULL is specified, the receiving object-reference-id-1 is set
// to the NULL value.
// If SELF is specified, the SET statement must appear in the PROCEDURE DIVISION
// of a method. object-reference-id-1 is set to reference the object upon which the
// currently executing method was invoked.

setStatement:
	setStatementForAssignment	       // SET format 1 for basic table handling
								       // SET format 5 for USAGE IS POINTER
								       // SET format 6 for procedure-pointer and function-pointer data items
								       // SET format 7 for USAGE OBJECT REFERENCE data items
	| setStatementForIndexes	       // SET format 2 for adjusting indexes
	| setStatementForSwitches	       // SET format 3 for external switches
	| setStatementForConditions;       // SET format 4 for condition-names

// Format 1: SET for basic table handling
// Format 5: SET for USAGE IS POINTER
// Format 6: SET for procedure-pointer and function-pointer data items
// Format 7: SET for USAGE OBJECT REFERENCE data items
// => setReceivingField can be a index name, procedure pointer, function pointer or an object reference Id
	
setStatementForAssignment:
	SET setReceivingField=dataOrIndexStorageArea+ TO setSendingField;
	 
setSendingField:
	  integerVariableOrIndex1                       // identifier can also be an index name	//Format 1 + 5
	| nullPointerValue                              // pointer data item //Format 5 + 6 + 7
	| ENTRY programNameOrProgramEntryVariable       // procedure pointer, function pointer or a pointer data item //Format 6 (+NULL | NULLS)
	| selfObjectIdentifier;                         // object reference id 	//Format 7 (+NULL)

// Format 2: SET for adjusting indexes

setStatementForIndexes:
	SET indexStorageArea+ (UP | DOWN) BY integerVariable1;

// Format 3: SET for external switches

setStatementForSwitches:
	SET setSwitchPosition+;

setSwitchPosition:
	mnemonicForUPSISwitchNameReference+ TO (ON | OFF);

// Format 4: SET for condition names

setStatementForConditions:
	SET conditionStorageArea+ TO TRUE;

// p422: SORT statement
// The SORT statement accepts records from one or more files, sorts them according
// to the specified keys, and makes the sorted records available either through an
// output procedure or in an output file.
// The SORT statement can appear anywhere in the PROCEDURE DIVISION except
// in the declarative portion.
// See also “MERGE statement” on page 364.
// The SORT statement is not supported for programs compiled with the THREAD
// option.
// file-name-1
// The name given in the SD entry that describes the records to be sorted.
// No pair of file-names in a SORT statement can be specified in the same SAME
// SORT AREA clause or the SAME SORT-MERGE AREA clause. File-names
// associated with the GIVING clause (file-name-3, ...) cannot be specified in the SAME
// AREA clause; however, they can be associated with the SAME RECORD AREA
// clause.
// ASCENDING KEY and DESCENDING KEY phrases
// This phrase specifies that records are to be processed in ascending or descending
// sequence (depending on the phrase specified), based on the specified sort keys.
// data-name-1
// Specifies a KEY data item on which the SORT statement will be based.
// Each such data-name must identify a data item in a record associated with
// file-name-1. The data-names following the word KEY are listed from left to
// right in the SORT statement in order of decreasing significance without
// regard to how they are divided into KEY phrases. The leftmost data-name
// is the major key, the next data-name is the next most significant key, and
// so forth. The following rules apply:
// - A specific KEY data item must be physically located in the same position
//   and have the same data format in each input file. However, it need not
//   have the same data-name.
// - If file-name-1 has more than one record description, the KEY data items
//   need be described in only one of the record descriptions.
// - If file-name-1 contains variable-length records, all of the KEY data-items
//   must be contained within the first n character positions of the record,
//   where n equals the minimum records size specified for file-name-1.
// - KEY data items must not contain an OCCURS clause or be subordinate
//   to an item that contains an OCCURS clause.
// - KEY data items cannot be:
//   – Variably located
//   – Group items that contain variable-occurrence data items
//   – Category numeric described with usage NATIONAL (national decimal
//     item)
//   – Category external floating-point described with usage NATIONAL
//     (national floating-point item)
//   – Category DBCS
// - KEY data items can be qualified.
// - KEY data items can be any of the following data categories:
//   – Alphabetic, alphanumeric, alphanumeric-edited
//   – Numeric (except numeric with usage NATIONAL)
//   – Numeric-edited (with usage DISPLAY or NATIONAL)
//   – Internal floating-point or display floating-point
//   – National or national-edited
// If file-name-3 references an indexed file, the first specification of data-name-1 must
// be associated with an ASCENDING phrase and the data item referenced by that
// data-name-1 must occupy the same character positions in this record as the data
// item associated with the major record key for that file.
// The direction of the sorting operation depends on the specification of the
// ASCENDING or DESCENDING keywords as follows:
// - When ASCENDING is specified, the sequence is from the lowest key value to
//   the highest key value.
// - When DESCENDING is specified, the sequence is from the highest key value to
//   the lowest.
// - If the KEY data item is described with usage NATIONAL, the sequence of the
//   KEY values is based on the binary values of the national characters.
// - If the KEY data item is internal floating point, the sequence of key values will be
//   in numeric order.
// - When the COLLATING SEQUENCE phrase is not specified, the key
//   comparisons are performed according to the rules for comparison of operands in
//   a relation condition. See “General relation conditions” on page 260).
// - When the COLLATING SEQUENCE phrase is specified, the indicated collating
//   sequence is used for key data items of alphabetic, alphanumeric,
//   alphanumeric-edited, external floating-point, and numeric-edited categories. For
//   all other key data items, the comparisons are performed according to the rules
//   for comparison of operands in a relation condition.
// DUPLICATES phrase
// If the DUPLICATES phrase is specified, and the contents of all the key elements
// associated with one record are equal to the corresponding key elements in one or
// more other records, the order of return of these records is as follows:
// - The order of the associated input files as specified in the SORT statement.
// Within a given file the order is that in which the records are accessed from that
// file.
// - The order in which these records are released by an input procedure, when an
//   input procedure is specified.
// If the DUPLICATES phrase is not specified, the order of these records is undefined.
// COLLATING SEQUENCE phrase
// This phrase specifies the collating sequence to be used in alphanumeric
// comparisons for the KEY data items in this sorting operation.
// The COLLATING SEQUENCE phrase has no effect for keys that are not alphabetic
// or alphanumeric.
// alphabet-name-1
// Must be specified in the ALPHABET clause of the SPECIAL-NAMES
// paragraph. alphabet-name-1 can be associated with any one of the
// ALPHABET clause phrases, with the following results:
// STANDARD-1
// The ASCII collating sequence is used for all alphanumeric
// comparisons. (The ASCII collating sequence is shown in
// Appendix C, “EBCDIC and ASCII collating sequences,” on page
// 569.)
// STANDARD-2
// The International Reference Version of ISO/IEC 646, 7-bit coded
// character set for information processing interchange is used for all
// alphanumeric comparisons.
// NATIVE
// The EBCDIC collating sequence is used for all alphanumeric
// comparisons. (The EBCDIC collating sequence is shown in
// Appendix C, “EBCDIC and ASCII collating sequences,” on page
// 569.)
// EBCDIC
// The EBCDIC collating sequence is used for all alphanumeric
// comparisons. (The EBCDIC collating sequence is shown in
// Appendix C, “EBCDIC and ASCII collating sequences,” on page
// 569.)
// literal
// The collating sequence established by the specification of literals in
// the alphabet-name clause is used for all alphanumeric
// comparisons.
// When the COLLATING SEQUENCE phrase is omitted, the PROGRAM
// COLLATING SEQUENCE clause (if specified) in the OBJECT-COMPUTER
// paragraph specifies the collating sequence to be used. When both the COLLATING
// SEQUENCE phrase and the PROGRAM COLLATING SEQUENCE clause are
// omitted, the EBCDIC collating sequence is used.
// USING phrase
// file-name-2 , ...
// The input files.
// When the USING phrase is specified, all the records in file-name-2, ..., (that
// is, the input files) are transferred automatically to file-name-1. At the time
// the SORT statement is executed, these files must not be open. The compiler
// opens, reads, makes records available, and closes these files automatically.
// If EXCEPTION/ERROR procedures are specified for these files, the
// compiler makes the necessary linkage to these procedures.
// All input files must be described in FD entries in the DATA DIVISION.
// If the USING phrase is specified and if file-name-1 contains variable-length
// records, the size of the records contained in the input files (file-name-2, ...)
// must be neither less than the smallest record nor greater than the largest
// record described for file-name-1. If file-name-1 contains fixed-length records,
// the size of the records contained in the input files must not be greater than
// the largest record described for file-name-1. For more information, see
// Describing the input to sorting or merging in the Enterprise COBOL
// Programming Guide.
// INPUT PROCEDURE phrase
// This phrase specifies the name of a procedure that is to select or modify input
// records before the sorting operation begins.
// procedure-name-1
// Specifies the first (or only) section or paragraph in the input procedure.
// procedure-name-2
// Identifies the last section or paragraph of the input procedure.
// The input procedure can consist of any procedure needed to select, modify,
// or copy the records that are made available one at a time by the RELEASE
// statement to the file referenced by file-name-1. The range includes all
// statements that are executed as the result of a transfer of control by CALL,
// EXIT, GO TO, PERFORM, and XML PARSE statements in the range of the
// input procedure, as well as all statements in declarative procedures that are
// executed as a result of the execution of statements in the range of the input
// procedure. The range of the input procedure must not cause the execution
// of any MERGE, RETURN, or SORT statement.
// If an input procedure is specified, control is passed to the input procedure
// before the file referenced by file-name-1 is sequenced by the SORT
// statement. The compiler inserts a return mechanism at the end of the last
// statement in the input procedure. When control passes the last statement in
// the input procedure, the records that have been released to the file
// referenced by file-name-1 are sorted.
// GIVING phrase
// file-name-3 , ...
// The output files.
// When the GIVING phrase is specified, all the sorted records in file-name-1
// are automatically transferred to the output files (file-name-3, ...).
// All output files must be described in FD entries in the DATA DIVISION.
// If the output files (file-name-3, ...) contain variable-length records, the size
// of the records contained in file-name-1 must be neither less than the
// smallest record nor greater than the largest record described for the output
// files. If the output files contain fixed-length records, the size of the records
// contained in file-name-1 must not be greater than the largest record
// described for the output files. For more information, see Describing the
// output from sorting or merging in the Enterprise COBOL Programming Guide.
// At the time the SORT statement is executed, the output files (file-name-3, ...)
// must not be open. For each of the output files, the execution of the SORT
// statement causes the following actions to be taken:
// - The processing of the file is initiated. The initiation is performed as if an
//   OPEN statement with the OUTPUT phrase had been executed.
// - The sorted logical records are returned and written onto the file. Each
//   record is written as if a WRITE statement without any optional phrases
//   had been executed.
// For a relative file, the relative key data item for the first record returned
// contains the value '1'; for the second record returned, the value '2'. After
// execution of the SORT statement, the content of the relative key data
// item indicates the last record returned to the file.
// - The processing of the file is terminated. The termination is performed as
//   if a CLOSE statement without optional phrases had been executed.
// These implicit functions are performed such that any associated USE
// AFTER EXCEPTION/ERROR procedures are executed; however, the
// execution of such a USE procedure must not cause the execution of any
// statement manipulating the file referenced by, or accessing the record area
// associated with, file-name-3. On the first attempt to write beyond the
// externally defined boundaries of the file, any USE AFTER STANDARD
// EXCEPTION/ERROR procedure specified for the file is executed. If control
// is returned from that USE procedure or if no such USE procedure is
// specified, the processing of the file is terminated.
// OUTPUT PROCEDURE phrase
// This phrase specifies the name of a procedure that is to select or modify output
// records from the sorting operation.
// procedure-name-3
// Specifies the first (or only) section or paragraph in the output procedure.
// procedure-name-4
// Identifies the last section or paragraph of the output procedure.
// The output procedure can consist of any procedure needed to select,
// modify, or copy the records that are made available one at a time by the
// RETURN statement in sorted order from the file referenced by file-name-1.
// The range includes all statements that are executed as the result of a
// transfer of control by CALL, EXIT, GO TO, PERFORM, and XML PARSE
// statements in the range of the output procedure. The range also includes
// all statements in declarative procedures that are executed as a result of the
// execution of statements in the range of the output procedure. The range of
// the output procedure must not cause the execution of any MERGE,
// RELEASE, or SORT statement.
// If an output procedure is specified, control passes to it after the file
// referenced by file-name-1 has been sequenced by the SORT statement. The
// compiler inserts a return mechanism at the end of the last statement in the
// output procedure and when control passes the last statement in the output
// procedure, the return mechanism provides the termination of the sort and
// then passes control to the next executable statement after the SORT
// statement. Before entering the output procedure, the sort procedure reaches
// a point at which it can select the next record in sorted order when
// requested. The RETURN statements in the output procedure are the
// requests for the next record.
// The INPUT PROCEDURE and OUTPUT PROCEDURE phrases are similar
// to those for a basic PERFORM statement. For example, if you name a
// procedure in an output procedure, that procedure is executed during the
// sorting operation just as if it were named in a PERFORM statement. As
// with the PERFORM statement, execution of the procedure is terminated
// after the last statement completes execution. The last statement in an input
// or output procedure can be the EXIT statement (see “EXIT statement” on
// page 335).
// SORT special registers
// The special registers, SORT-CORE-SIZE, SORT-MESSAGE, and SORT-MODE-SIZE,
// are equivalent to option control statement keywords in the sort control file. You
// define the sort control data set with the SORT-CONTROL special register.
// Usage note: If you use a sort control file to specify control statements, the values
// specified in the sort control file take precedence over those in the special register.
// SORT-MESSAGE special register
// See “SORT-MESSAGE” on page 23.
// SORT-CORE-SIZE special register
// See “SORT-CORE-SIZE” on page 22.
// SORT-FILE-SIZE special register
// See “SORT-FILE-SIZE” on page 22.
// SORT-MODE-SIZE special register
// See “SORT-MODE-SIZE” on page 23.
// SORT-CONTROL special register
// See “SORT-CONTROL” on page 22.
// SORT-RETURN special register
// See “SORT-RETURN” on page 23.
// Segmentation considerations
// The topic lists considerations of using the SORT statement.
// If a SORT statement is coded in a fixed segment, any input or output procedure
// referenced by that SORT statement must be either totally within a fixed segment or
// wholly contained in a single independent segment.
// If a SORT statement is coded in an independent segment, any input or output
// procedure referenced by that SORT statement must be either totally within a fixed
// segment or wholly contained within the same independent segment as that SORT
// statement.

sortStatement:
	SORT fileNameReference onAscendingDescendingKey+ 
	(WITH? DUPLICATES IN? ORDER?)?
	collatingSequence?
	(usingFilenames  | inputProcedure)
	(givingFilenames | outputProcedure);

// Rules shared with mergeStatement

// p429: START statement
// The START statement provides a means of positioning within an indexed or
// relative file for subsequent sequential record retrieval.
// When the START statement is executed, the associated indexed or relative file must
// be open in either INPUT or I-O mode.
// file-name-1
// Must name a file with sequential or dynamic access. file-name-1 must be
// defined in an FD entry in the DATA DIVISION and must not name a sort
// file.
// KEY phrase
// When the KEY phrase is specified, the file position indicator is positioned at the
// logical record in the file whose key field satisfies the comparison.
// When the KEY phrase is not specified, KEY IS EQUAL (to the prime record key) is
// implied.
// data-name-1
// Can be qualified; it cannot be subscripted.
// When the START statement is executed, a comparison is made between the current
// value in the key data-name and the corresponding key field in the file's index.
// If the FILE STATUS clause is specified in the file-control entry, the associated file
// status key is updated when the START statement is executed (See “File status key”
// on page 287).
// INVALID KEY phrases
// If the comparison is not satisfied by any record in the file, an invalid key condition
// exists; the position of the file position indicator is undefined, and (if specified) the
// INVALID KEY imperative-statement is executed. (See “INTO and FROM phrases”
// on page 291 under "Common processing facilities".)
// Both the INVALID KEY phrase and an applicable EXCEPTION/ERROR procedure
// can be omitted.
// END-START phrase
// This explicit scope terminator serves to delimit the scope of the START statement.
// END-START permits a conditional START statement to be nested in another
// conditional statement. END-START can also be used with an imperative START
// statement.
// For more information, see “Delimited scope statements” on page 280.
// Indexed files
// When the KEY phrase is specified, the key data item used for the comparison is
// data-name-1.
// When the KEY phrase is not specified, the key data item used for the EQUAL TO
// comparison is the prime RECORD KEY.
// When START statement execution is successful, the RECORD KEY or ALTERNATE
// RECORD KEY with which data-name-1 is associated becomes the key of reference
// for subsequent READ statements.
// data-name-1
// Can be any of the following items:
// - The prime RECORD KEY.
// - Any ALTERNATE RECORD KEY.
// - A data item within a record description for a file whose leftmost
//   character position corresponds to the leftmost character position of that
//   record key; it can be qualified. The size of the data item must be less
//   than or equal to the length of the record key for the file.
// Regardless of its category, data-name-1 is treated as an alphanumeric item
// for purposes of the comparison operation.
// Note: If your key is numeric, you must specify the EQUAL TO condition,
// otherwise, unexpected results can happen.
// The file position indicator points to the first record in the file whose key field
// satisfies the comparison. If the operands in the comparison are of unequal lengths,
// the comparison proceeds as if the longer field were truncated on the right to the
// length of the shorter field. All other numeric and alphanumeric comparison rules
// apply, except that the PROGRAM COLLATING SEQUENCE clause, if specified,
// has no effect.
// When START statement execution is successful, the RECORD KEY with which
// data-name-1 is associated becomes the key of reference for subsequent READ
// statements.
// When START statement execution is unsuccessful, the key of reference is
// undefined.
// Relative files
// When the KEY phrase is specified, data-name-1 must specify the RELATIVE KEY.
// Whether or not the KEY phrase is specified, the key data item used in the
// comparison is the RELATIVE KEY data item. Numeric comparison rules apply.
// The file position indicator points to the logical record in the file whose key satisfies
// the specified comparison.

startStatement:
	START fileNameReference (KEY IS? relationalOperator variable1)?;

startStatementEnd: END_START;

// p432: STOP statement
// The STOP statement halts execution of the object program either permanently or
// temporarily.
// literal
// Can be a fixed-point numeric literal (signed or unsigned) or an
// alphanumeric literal. It can be any figurative constant except ALL literal.
// When STOP literal is specified, the literal is communicated to the operator, and
// object program execution is suspended. Program execution is resumed only after
// operator intervention, and continues at the next executable statement in sequence.
// The STOP literal statement is useful for special situations when operator
// intervention is needed during program execution; for example, when a special tape
// or disk must be mounted or a specific daily code must be entered. However, the
// ACCEPT and DISPLAY statements are preferred when operator intervention is
// needed.
// Do not use the STOP literal statement in programs compiled with the THREAD
// compiler option.
// When STOP RUN is specified, execution is terminated and control is returned to
// the system. When STOP RUN is not the last or only statement in a sequence of
// imperative statements within a sentence, the statements following STOP RUN are
// not executed.
// The STOP RUN statement closes all files defined in any of the programs in the run
// unit.
// For use of the STOP RUN statement in calling and called programs, see the
// following table.
// Termination statement : STOP RUN
// Main program : Returns to the calling program. (Can be the system, which causes the application to end.)
// Subprogram : Returns directly to the program that called the main program. (Can be the system, which causes the application to end.)

stopStatement:
	STOP (RUN | messageToOperator);

messageToOperator: 
	numericValue | alphanumericValue3 | nullFigurativeConstant;

// p433: STRING statement
// The STRING statement strings together the partial or complete contents of two or
// more data items or literals into one single data item.
// One STRING statement can be written instead of a series of MOVE statements.
// identifier-1, literal-1
// Represents the sending fields.
// DELIMITED BY phrase
// Sets the limits of the string.
// identifier-2, literal-2
// Are delimiters; that is, characters that delimit the data to be
// transferred.
// SIZE Transfers the complete sending area.
// INTO phrase
// Identifies the receiving field.
// identifier-3
// Represents the receiving field.
// POINTER phrase
// Points to a character position in the receiving field. The pointer field
// indicates a relative alphanumeric character position, DBCS character
// position, or national character position when the receiving field is of usage
// DISPLAY, DISPLAY-1, or NATIONAL, respectively.
// identifier-4
// Represents the pointer field. identifier-4 must be large enough to
// contain a value equal to the length of the receiving field plus 1.
// You must initialize identifier-4 to a nonzero value before execution
// of the STRING statement begins.
// The following rules apply:
// - All identifiers except identifier-4 must reference data items described explicitly
//   or implicitly as usage DISPLAY, DISPLAY-1, or NATIONAL.
// - literal-1 or literal-2 must be of category alphanumeric, DBCS, or national and can
//   be any figurative constant that does not begin with the word ALL (except
//   NULL).
// - If identifier-1 or identifer-2 references a data item of category numeric, each
//   numeric item must be described as an integer without the symbol 'P' in its
//   PICTURE character-string.
// - identifier-3 must not reference a data item of category numeric-edited,
//   alphanumeric-edited, or national-edited; an external floating-point data item of
//   usage DISPLAY, or an external floating-point data item of usage NATIONAL.
// - identifier-3 must not described with the JUSTIFIED clause.
// - If identifier-3 is of usage DISPLAY, identifier-1 and identifier-2 must be of usage
//   DISPLAY and all literals must be alphanumeric literals. Any figurative constant
//   can be specified except one that begins with the word ALL. Each figurative
//   constant represents a 1-character alphanumeric literal.
// - If identifier-3 is of usage DISPLAY-1, identifier-1 and identifier-2 must be of usage
//   DISPLAY-1 and all literals must be DBCS literals. The only figurative constant
//   that can be specified is SPACE, which represents a 1-character DBCS literal. ALL
//   DBCS-literal must not be specified.
// - If identifier-3 is of usage NATIONAL, identifier-1 and identifier-2 must be of usage
//   NATIONAL and all literals must be national literals. Any figurative constant can
//   be specified except symbolic-character and one that begins with the word ALL.
//   Each figurative constant represents a 1-character national literal.
// - If identifier-1 or identifier-2 references an elementary data item of usage DISPLAY
//   that is described as category numeric, numeric-edited, or alphanumeric-edited,
//   the item is treated as if it were redefined as category alphanumeric.
// - If identifier-1 or identifier-2 references an elementary data item of usage
//   NATIONAL that is described as category numeric, numeric-edited, or
//   national-edited item, the item is treated as if it were redefined as category
//   national.
// - identifier-4 must not be described with the symbol P in its PICTURE
//   character-string.
// Evaluation of subscripts, reference modification, variable-lengths, variable
// locations, and function-identifiers is performed only once, at the beginning of the
// execution of the STRING statement. Therefore, if identifier-3 or identifier-4 is used as
// a subscript, reference-modifier, or function argument in the STRING statement, or
// affects the length or location of any of the identifiers in the STRING statement, the
// values calculated for those subscripts, reference-modifiers, variable lengths,
// variable locations, and functions are not affected by any results of the STRING
// statement.
// If identifier-3 and identifier-4 occupy the same storage area, undefined results will
// occur, even if the identifiers are defined by the same data description entry.
// If identifier-1 or identifier-2 occupies the same storage area as identifier-3 or
// identifier-4, undefined results will occur, even if the identifiers are defined by the
// same data description entry.
// See “Data flow” for details of STRING statement processing.
// ON OVERFLOW phrases
// imperative-statement-1
// Executed when the pointer value (explicit or implicit):
// - Is less than 1
// - Exceeds a value equal to the length of the receiving field
// When either of the above conditions occurs, an overflow condition exists,
// and no more data is transferred. Then the STRING operation is terminated,
// the NOT ON OVERFLOW phrase, if specified, is ignored, and control is
// transferred to the end of the STRING statement or, if the ON OVERFLOW
// phrase is specified, to imperative-statement-1.
// If control is transferred to imperative-statement-1, execution continues
// according to the rules for each statement specified in imperative-statement-1.
// If a procedure branching or conditional statement that causes explicit
// transfer of control is executed, control is transferred according to the rules
// for that statement; otherwise, upon completion of the execution of
// imperative-statement-1, control is transferred to the end of the STRING
// statement.
// If at the time of execution of a STRING statement, conditions that would
// cause an overflow condition are not encountered, then after completion of
// the transfer of data, the ON OVERFLOW phrase, if specified, is ignored.
// Control is then transferred to the end of the STRING statement, or if the
// NOT ON OVERFLOW phrase is specified, to imperative-statement-2.
// If control is transferred to imperative-statement-2, execution continues
// according to the rules for each statement specified in imperative-statement-2.
// If a procedure branching or conditional statement that causes explicit
// transfer of control is executed, control is transferred according to the rules
// for that statement. Otherwise, upon completion of the execution of
// imperative-statement-2, control is transferred to the end of the STRING
// statement.
// END-STRING phrase
// This explicit scope terminator serves to delimit the scope of the STRING statement.
// END-STRING permits a conditional STRING statement to be nested in another
// conditional statement. END-STRING can also be used with an imperative STRING
// statement.
// For more information, see “Delimited scope statements” on page 280. 
// ... more details p435->437 Data flow / Example of the STRING statement ...

stringStatement:
	STRING contentToConcatenate+ 
	INTO receivingField=storageArea1
	(WITH? POINTER characterPositionInReceivingField=storageArea1)?;
	
contentToConcatenate:
	sendingField=variable6+ DELIMITED BY? (delimiterCharacters=variable4 | SIZE);

stringStatementEnd: END_STRING;

// p438: SUBTRACT statement
// The SUBTRACT statement subtracts one numeric item, or the sum of two or more
// numeric items, from one or more numeric items, and stores the result.

subtractStatement:
	subtractSimple | subtractGiving | subtractCorresponding;

// p438: Format 1: SUBTRACT statement
// All identifiers or literals preceding the keyword FROM are added together and
// their sum is subtracted from and stored immediately in identifier-2. This process is
// repeated for each successive occurrence of identifier-2, in the left-to-right order in
// which identifier-2 is specified.

subtractSimple:
	SUBTRACT numericVariable3+ FROM numericStorageAreaRounded+;

// p439: Format 2: SUBTRACT statement with GIVING phrase
// All identifiers or literals preceding the keyword FROM are added together and
// their sum is subtracted from identifier-2 or literal-2. The result of the subtraction is
// stored as the new value of each data item referenced by identifier-3.

subtractGiving:
	SUBTRACT numericVariable3+ FROM fromOperand=numericVariable3 GIVING numericStorageAreaRounded+;

// p439: Format 3: SUBTRACT statement with CORRESPONDING phrase
// Elementary data items within identifier-1 are subtracted from, and the results are
// stored in, the corresponding elementary data items within identifier-2.

subtractCorresponding:
	SUBTRACT (CORRESPONDING | CORR) groupItem=dataItemReference FROM fromGroupItem=dataItemReference ROUNDED?;

subtractStatementEnd: END_SUBTRACT;

// When the ARITH(COMPAT) compiler option is in effect, the composite of operands
// can contain a maximum of 30 digits. When the ARITH(EXTEND) compiler option
// is in effect, the composite of operands can contain a maximum of 31 digits. For
// more information about arithmetic intermediate results, see Appendix A.
// Intermediate results and arithmetic precision in the Enterprise COBOL Programming
// Guide.
//
// For all formats:
// identifier
// In format 1, must name an elementary numeric data item.
// In format 2, must name an elementary numeric data item, unless the
// identifier follows the word GIVING. Each identifier following the word
// GIVING must name a numeric or numeric-edited elementary data item.
// In format 3, must name an alphanumeric group item or a national group
// item.
// literal
// Must be a numeric literal.
// Floating-point data items and literals can be used anywhere numeric data items
// and literals can be specified.
// ROUNDED phrase
// For information about the ROUNDED phrase, and for operand considerations, see
// “ROUNDED phrase” on page 282.
// SIZE ERROR phrases
// For information about the SIZE ERROR phrases, and for operand considerations,
// see “SIZE ERROR phrases” on page 283.
// CORRESPONDING phrase (format 3)
// See “CORRESPONDING phrase” on page 281.
// END-SUBTRACT phrase
// This explicit scope terminator serves to delimit the scope of the SUBTRACT
// statement. END-SUBTRACT permits a conditional SUBTRACT statement to be
// nested in another conditional statement. END-SUBTRACT can also be used with
// an imperative SUBTRACT statement.
// For more information, see “Delimited scope statements” on page 280.

// p441: UNSTRING statement
// The UNSTRING statement causes contiguous data in a sending field to be
// separated and placed into multiple receiving fields.
// identifier-1
// Represents the sending field. Data is transferred from this field to the data
// receiving fields (identifier-4).
// identifier-1 must reference a data item of category alphabetic, alphanumeric,
// alphanumeric-edited, DBCS, national, or national-edited.
// identifier-2, literal-1, identifier-3, literal-2
// Specifies one or more delimiters.
// identifier-2 and identifier-3 must reference data items of category alphabetic,
// alphanumeric, alphanumeric-edited, DBCS, national, or national-edited.
// literal-1 or literal-2 must be of category alphanumeric, DBCS, or national
// and must not be a figurative constant that begins with the word ALL.
// identifier-4
// Specifies one or more receiving fields.
// identifier-4 must reference a data item of category alphabetic, alphanumeric,
// numeric, DBCS, or national. If the referenced data item is of category
// numeric, its picture character-string must not contain the picture symbol P,
// and its usage must be DISPLAY or NATIONAL.
// identifier-5
// Specifies a field to receive the delimiter associated with identifier-4.
// Identifier-5 must reference a data item of category alphabetic,
// alphanumeric, DBCS, or national.
// identifier-6
// Specifies a field to hold the count of characters that are transferred to
// identifier-4.
// identifier-6 must be an integer data item defined without the symbol P in
// its PICTURE character-string.
// identifier-7
// Specifies a field to hold a relative character position during UNSTRING
// processing.
// identifier-7 must be an integer data item defined without the symbol P in
// the PICTURE string.identifier-7 must be described as a data item of
// sufficient size to contain a value equal to 1 plus the number of character
// positions in the data item referenced by identifier-1.
// identifier-8
// Specifies a field that is incremented by the number of delimited fields
// processed.
// identifier-8 must be an integer data item defined without the symbol P in
// its PICTURE character-string.
// The following rules apply
// - If identifier-4 references a data item of usage DISPLAY, identifier-1, identifier-2,
//   identifier-3, and identifier-5 must also reference data items of usage DISPLAY and
//   all literals must be alphanumeric literals. Any figurative constant can be
//   specified except NULL or one that begins with the word ALL. Each figurative
//   constant represents a 1-character alphanumeric literal.
// - If identifier-4 references a data item of usage DISPLAY-1, identifier-1, identifier-2,
//   identifier-3, and identifier-5 must also reference data items of usage DISPLAY-1
//   and all literals must be DBCS literals. Figurative constant SPACE is the only
//   figurative constant that can be specified. Each figurative constant represents a
//   1-character DBCS literal.
// - If identifier-4 references a data item of usage NATIONAL, identifier-1, identifier-2,
//   identifier-3, and identifier-5 must also reference data items of usage NATIONAL
//   and all literals must be national literals. Any figurative constant can be specified
//   except NULL or one that begins with the word ALL. Each figurative constant
//   represents a 1-character national literal.
// Count fields (identifier-6) and pointer fields (identifier-7) are incremented by number
// of character positions (alphanumeric, DBCS, or national), not by number of bytes.
// One UNSTRING statement can take the place of a series of MOVE statements,
// except that evaluation or calculation of certain elements is performed only once, at
// the beginning of the execution of the UNSTRING statement. For more information,
// see “Values at the end of execution of the UNSTRING statement” on page 447.
// The rules for moving are the same as those for a MOVE statement for an
// elementary sending item of the category of identifier-1, with the appropriate
// identifier-4 as the receiving item (see “MOVE statement” on page 369). For example,
// rules for moving a DBCS item are used when identifier-1 is a DBCS item.
// DELIMITED BY phrase
// This phrase specifies delimiters within the data that control the data transfer.
// Each identifier-2, identifier-3, literal-1, or literal-2 represents one delimiter.
// If the DELIMITED BY phrase is not specified, the DELIMITER IN and COUNT IN
// phrases must not be specified.
// ALL Multiple contiguous occurrences of any delimiters are treated as if there
// were only one occurrence; this one occurrence is moved to the delimiter
// receiving field (identifier-5), if specified. The delimiting characters in the
// sending field are treated as an elementary item of the same usage and
// category as identifier-1 and are moved into the current delimiter receiving
// field according to the rules of the MOVE statement.
// When DELIMITED BY ALL is not specified, and two or more contiguous
// occurrences of any delimiter are encountered, the current data receiving
// field (identifier-4) is filled with spaces or zeros, according to the description
// of the data receiving field.
// Delimiter with two or more characters
// A delimiter that contains two or more characters is recognized as a delimiter only
// if the delimiting characters are in both of the following cases:
// - Contiguous
// - In the sequence specified in the sending field
// Two or more delimiters
// When two or more delimiters are specified, an OR condition exists, and each
// nonoverlapping occurrence of any one of the delimiters is recognized in the
// sending field in the sequence specified.
// For example:
// DELIMITED BY "AB" or "BC"
// An occurrence of either AB or BC in the sending field is considered a delimiter. An
// occurrence of ABC is considered an occurrence of AB.
// INTO phrase
// This phrase specifies the fields where the data is to be moved.
// identifier-4 represents the data receiving fields.
// DELIMITER IN
// This phrase specifies the fields where the delimiters are to be moved.
// identifier-5 represents the delimiter receiving fields.
// The DELIMITER IN phrase must not be specified if the DELIMITED BY
// phrase is not specified.
// COUNT IN
// This phrase specifies the field where the count of examined character
// positions is held.
// identifier-6 is the data count field for each data transfer. Each field holds the
// count of examined character positions in the sending field, terminated by
// the delimiters or the end of the sending field, for the move to this
// receiving field. The delimiters are not included in this count.
// The COUNT IN phrase must not be specified if the DELIMITED BY phrase
// is not specified.
// POINTER phrase
// When the POINTER phrase is specified, the value of the pointer field, identifier-7,
// behaves as if it were increased by 1 for each examined character position in the
// sending field. When execution of the UNSTRING statement is completed, the
// pointer field contains a value equal to its initial value plus the number of character
// positions examined in the sending field.
// When this phrase is specified, the user must initialize the pointer field before
// execution of the UNSTRING statement begins.
// TALLYING IN phrase
// When the TALLYING phrase is specified, the area count field, identifier-8, contains
// (at the end of execution of the UNSTRING statement) a value equal to the initial
// value plus the number of data receiving areas acted upon.
// When this phrase is specified, the user must initialize the area count field before
// execution of the UNSTRING statement begins.
// ON OVERFLOW phrases
// An overflow condition exists when:
// - The pointer value (explicit or implicit) is less than 1.
// - The pointer value (explicit or implicit) exceeds a value equal to the length of the
//   sending field.
// - All data receiving fields have been acted upon and the sending field still
//   contains unexamined character positions.
// When an overflow condition occurs
// An overflow condition results in the following actions:
// 1. No more data is transferred.
// 2. The UNSTRING operation is terminated.
// 3. The NOT ON OVERFLOW phrase, if specified, is ignored.
// 4. Control is transferred to the end of the UNSTRING statement or, if the ON
//    OVERFLOW phrase is specified, to imperative-statement-1.
// imperative-statement-1
// Statement or statements for dealing with an overflow condition.
// If control is transferred to imperative-statement-1, execution continues
// according to the rules for each statement specified in imperativestatement-
// 1. If a procedure branching or conditional statement that causes
//    explicit transfer of control is executed, control is transferred according to
//    the rules for that statement. Otherwise, upon completion of the execution
//    of imperative-statement-1, control is transferred to the end of the UNSTRING
//    statement.
// When an overflow condition does not occur
// When, during execution of an UNSTRING statement, conditions that would cause
// an overflow condition are not encountered, then:
// 1. The transfer of data is completed.
// 2. The ON OVERFLOW phrase, if specified, is ignored.
// 3. Control is transferred to the end of the UNSTRING statement or, if the NOT
// ON OVERFLOW phrase is specified, to imperative-statement-2.
// imperative-statement-2
// Statement or statements for dealing with an overflow condition that does
// not occur.
// If control is transferred to imperative-statement-2, execution continues
// according to the rules for each statement specified in imperativestatement-
// 2. If a procedure branching or conditional statement that causes
// explicit transfer of control is executed, control is transferred according to
// the rules for that statement. Otherwise, upon completion of the execution
// of imperative-statement-2, control is transferred to the end of the UNSTRING
// statement.
// END-UNSTRING phrase
// This explicit scope terminator serves to delimit the scope of the UNSTRING
// statement. END-UNSTRING permits a conditional UNSTRING statement to be
// nested in another conditional statement. END-UNSTRING can also be used with
// an imperative UNSTRING statement.
// For more information, see “Delimited scope statements” on page 280.
// ... more details p445->447 Data flow ...
// ... more details p447 Values at the end of execution of the UNSTRING statement ...
// ... more details p447->448 Example of the UNSTRING statement ...

unstringStatement:
	UNSTRING sendingField=variable1
	(DELIMITED BY? unstringDelimiter (OR unstringDelimiter)*)? 
	INTO unstringReceivingFields+ 
	(WITH? POINTER relativeCharacterPositionDuringProcessing=storageArea1)? 
	(TALLYING IN? incrementByNumberOfDelimitedFields=storageArea1)?;

unstringDelimiter:
	ALL? delimiterCharacters=variable4;
	
unstringReceivingFields:
	receivingField=storageArea1 
	(DELIMITER IN? associatedDelimiter=storageArea1)? 
	(COUNT IN? charsTransferredCount=storageArea1)?;
	
unstringStatementEnd: END_UNSTRING;

// p449: WRITE statement
// The WRITE statement releases a logical record to an output or input/output file.
// When the WRITE statement is executed:
// - The associated sequential file must be open in OUTPUT or EXTEND mode.
// - The associated indexed or relative file must be open in OUTPUT, I-O, or
//   EXTEND mode.
//
// p449: Format 1: WRITE statement for sequential files
// p450: Format 2: WRITE statement for indexed and relative files
// p450: Format 3: WRITE statement for line-sequential files
//
// record-name-1
// Must be defined in a DATA DIVISION FD entry. record-name-1 can be
// qualified. It must not be associated with a sort or merge file.
// For relative files, the number of character positions in the record being
// written can be different from the number of character positions in the
// record being replaced.
// FROM phrase
// The result of the execution of the WRITE statement with the FROM
// identifier-1 phrase is equivalent to the execution of the following statements
// in the order specified:
// MOVE identifier-1 TO record-name-1.
// WRITE record-name-1.
// The MOVE is performed according to the rules for a MOVE statement
// without the CORRESPONDING phrase.
// identifier-1
// identifier-1 can reference any of the following items:
// - A data item defined in the WORKING-STORAGE SECTION, the
//   LOCAL-STORAGE SECTION, or the LINKAGE SECTION
// - A record description for another previously opened file
// - An alphanumeric function
// - A national function
// identifier-1 must be a valid sending item for a MOVE statement with
// record-name-1 as the receiving item.
// identifier-1 and record-name-1 must not refer to the same storage area.
// After the WRITE statement is executed, the information is still available in
// identifier-1. (See “INTO and FROM phrases” on page 291 under "Common
// processing facilities".)
// identifier-2
// Must be an integer data item.
// ADVANCING phrase
// The ADVANCING phrase controls positioning of the output record on the page.
// The BEFORE and AFTER phrases are not supported for VSAM files. QSAM files
// are sequentially organized. The ADVANCING and END-OF-PAGE phrases control
// the vertical positioning of each line on a printed page.
// You can specify the ADVANCING PAGE and END-OF-PAGE phrases in a single
// WRITE statement.
// If the printed page is held on an intermediate device (a disk, for example), the
// format can appear different from the expected format when the output is edited or
// browsed.
// ADVANCING phrase rules
// When the ADVANCING phrase is specified, the following rules apply:
// 1. When BEFORE ADVANCING is specified, the line is printed before the page is
// advanced.
// 2. When AFTER ADVANCING is specified, the page is advanced before the line is
// printed.
// 3. When identifier-2 is specified, the page is advanced the number of lines equal to
// the current value in identifier-2. identifier-2 must name an elementary integer
// data item.
// 4. When integer is specified, the page is advanced the number of lines equal to
// the value of integer.
// 5. Integer or the value in identifier-2 can be zero.
// 6. When PAGE is specified, the record is printed on the logical page BEFORE or
// AFTER (depending on the phrase used) the device is positioned to the next
// logical page. If PAGE has no meaning for the device used, then BEFORE or
// AFTER (depending on the phrase specified) ADVANCING 1 LINE is provided.
// If the FD entry contains a LINAGE clause, the repositioning is to the first
// printable line of the next page, as specified in that clause. If the LINAGE clause
// is omitted, the repositioning is to line 1 of the next succeeding page.
// 7. When mnemonic-name is specified, a skip to channels 1 through 12, or space
// suppression, takes place. mnemonic-name must be equated with
// environment-name-1 in the SPECIAL-NAMES paragraph.
// The mnemonic-name phrase can also be specified for stacker selection with a
// card punch file. When using stacker selection, WRITE AFTER ADVANCING
// must be used.
// The ADVANCING phrase of the WRITE statement, or the presence of a LINAGE
// clause on the file, causes a carriage control character to be generated in the record
// that is written. If the corresponding file is described with the EXTERNAL clause,
// all file connectors within the run unit must be defined such that carriage control
// characters will be generated for records that are written. That is, if all the files have
// a LINAGE clause, some of the programs can use the WRITE statement with the
// ADVANCING phrase and other programs can use the WRITE statement without
// the ADVANCING phrase. However, if none of the files has a LINAGE clause, then
// if any of the programs use the WRITE statement with the ADVANCING phrase, all
// of the programs in the run unit that have a WRITE statement must use the WRITE
// statement with the ADVANCING phrase.
// When the ADVANCING phrase is omitted, automatic line advancing is provided,
// as if AFTER ADVANCING 1 LINE had been specified.
// LINAGE-COUNTER rules
// If the LINAGE clause is specified for this file, the associated LINAGE-COUNTER
// special register is modified during the execution of the WRITE statement,
// according to the following rules:
// 1. If ADVANCING PAGE is specified, LINAGE-COUNTER is reset to 1.
// 2. If ADVANCING identifier-2 or integer is specified, LINAGE-COUNTER is
// increased by the value in identifier-2 or integer.
// 3. If the ADVANCING phrase is omitted, LINAGE-COUNTER is increased by 1.
// 4. When the device is repositioned to the first available line of a new page,
// LINAGE-COUNTER is reset to 1.
// Usage note: If you use the ADV compiler option, the compiler adds 1 byte to the
// record length in order to allow for the control character. If in your record definition
// you already reserve the first byte for the control character, you should use the
// NOADV option. For files defined with the LINAGE clause, the NOADV option has
// no effect. The compiler processes these files as if the ADV option were specified.
// END-OF-PAGE phrases
// The AT END-OF-PAGE phrase is not supported for VSAM files.
// When END-OF-PAGE is specified, and the logical end of the printed page is
// reached during execution of the WRITE statement, the END-OF-PAGE
// imperative-statement is executed. When the END-OF-PAGE phrase is specified, the
// FD entry for this file must contain a LINAGE clause.
// The logical end of the printed page is specified in the associated LINAGE clause.
// An END-OF-PAGE condition is reached when execution of a WRITE
// END-OF-PAGE statement causes printing or spacing within the footing area of a
// page body. This occurs when execution of such a WRITE statement causes the
// value in the LINAGE-COUNTER special register to equal or exceed the value
// specified in the WITH FOOTING phrase of the LINAGE clause. The WRITE
// statement is executed, and then the END-OF-PAGE imperative-statement is
// executed.
// An automatic page overflow condition is reached whenever the execution of any
// given WRITE statement (with or without the END-OF-PAGE phrase) cannot be
// completely executed within the current page body. This occurs when a WRITE
// statement, if executed, would cause the value in the LINAGE-COUNTER to exceed
// the number of lines for the page body specified in the LINAGE clause. In this case,
// the line is printed BEFORE or AFTER (depending on the option specified) the
// device is repositioned to the first printable line on the next logical page, as
// specified in the LINAGE clause. If the END-OF-PAGE phrase is specified, the
// END-OF-PAGE imperative-statement is then executed.
// If the WITH FOOTING phrase of the LINAGE clause is not specified, the
// automatic page overflow condition exists because no end-of-page condition (as
// distinct from the page overflow condition) can be detected.
// If the WITH FOOTING phrase is specified, but the execution of a given WRITE
// statement would cause the LINAGE-COUNTER to exceed both the footing value
// and the page body value specified in the LINAGE clause, then both the
// end-of-page condition and the automatic page overflow condition occur
// simultaneously.
// The keywords END-OF-PAGE and EOP are equivalent.
// You can specify both the ADVANCING PAGE phrase and the END-OF-PAGE
// phrase in a single WRITE statement.
// INVALID KEY phrases
// The INVALID KEY phrase is not supported for VSAM sequential files.
// An invalid key condition is caused by the following cases:
// - For sequential files, an attempt is made to write beyond the externally defined
//   boundary of the file.
// - For indexed files:
//   – An attempt is made to write beyond the externally defined boundary of the
//     file.
//   – ACCESS SEQUENTIAL is specified and the file is opened OUTPUT, and the
//     value of the prime record key is not greater than that of the previous record.
//   – The file is opened OUTPUT or I-O and the value of the prime record key
//     equals that of an already existing record.
// - For relative files:
//   – An attempt is made to write beyond the externally defined boundary of the
//     file.
//   – When the access mode is random or dynamic and the RELATIVE KEY data
//     item specifies a record that already exists in the file.
//   – The number of significant digits in the relative record number is larger than
//     the size of the relative key data item for the file.
// When an invalid key condition occurs:
// - If the INVALID KEY phrase is specified, imperative-statement-1 is executed. For
//   details of invalid key processing, see Invalid key condition.
// - Otherwise, the WRITE statement is unsuccessful and the contents of record-name
//   are unaffected (except for QSAM files) and the following occurs:
// – For sequential files, the file status key, if specified, is updated and an
//   EXCEPTION/ERROR condition exists.
// If an explicit or implicit EXCEPTION/ERROR procedure is specified for the
// file, the procedure is executed. If no such procedure is specified, the results
// are unpredictable.
// – For relative and indexed files, program execution proceeds according to the
// rules described by Invalid key condition under "Common processing
// facilities".
// The INVALID KEY conditions that apply to a relative file in OPEN OUTPUT
// mode also apply to one in OPEN EXTEND mode.
// - If the NOT INVALID KEY phrase is specified and a valid key condition exists at
//   the end of the execution of the WRITE statement, control is passed to
// imperative-statement-4.
// Both the INVALID KEY phrase and an applicable EXCEPTION/ERROR procedure
// can be omitted.
// END-WRITE phrase
// This explicit scope terminator serves to delimit the scope of the WRITE statement.
// END-WRITE permits a conditional WRITE statement to be nested in another
// conditional statement. END-WRITE can also be used with an imperative WRITE
// statement.
// For more information, see “Delimited scope statements” on page 280. 
// ... more details p454->456 WRITE for sequential files ...
// ... more details p456 WRITE for indexed files ...
// ... more details p456 WRITE for relative files ...

writeStatement:
	WRITE recordName (FROM sendingField=variable1)?
	((BEFORE | AFTER) ADVANCING? (
		(numberOfLines=integerVariable1 (LINE | LINES)?)  | 
		 mnemonicForEnvironmentNameReference              | 
		 PAGE                                             )? )?;

writeStatementEnd: END_WRITE;

// p457: XML GENERATE statement
// The XML GENERATE statement converts data to XML format.
// identifier-1
// The receiving area for a generated XML document. identifier-1 must
// reference one of the following items:
// - An elementary data item of category alphanumeric
// - An alphanumeric group item
// - An elementary data item of category national
// - A national group item
// When identifier-1 references a national group item, identifier-1 is processed
// as an elementary data item of category national. When identifier-1
// references an alphanumeric group item, identifier-1 is treated as though it
// were an elementary data item of category alphanumeric.
// identifier-1 must not be described with the JUSTIFIED clause, and cannot be
// a function identifier. identifier-1 can be subscripted or reference modified.
// identifier-1 must not overlap identifier-2, identifier-3, codepage (if an
// identifier), identifier-4, or identifier-5.
// The generated XML output is encoded as described in the following
// documentation of the ENCODING phrase.
// identifier-1 must reference a data item of category national, or the
// ENCODING phrase must specify 1208, if any of the following statements
// are true:
// - The CODEPAGE compiler option specifies an EBCDIC DBCS code page.
// - identifier-4 or identifier-5 references a data item of category national.
// - literal-4 or literal-5 is of category national.
// - The generated XML includes data from identifier-2 for:
//   – Any data item of class national or class DBCS
//   – Any data item with a DBCS name (that is, a data item whose name
//     consists of DBCS characters)
//   – Any data item of class alphanumeric that contains DBCS characters
// identifier-1 must be large enough to contain the generated XML document.
// Typically, it must be from 5 to 10 times the size of identifier-2, depending
// on the length of the data-name or data-names within identifier-2. If
// identifier-1 is not large enough, an error condition exists at the end of the
// XML GENERATE statement.
// identifier-2
// The group or elementary data item to be converted to XML format.
// If identifier-2 references a national group item, identifier-2 is processed as a
// group item. When identifier-2 includes a subordinate national group item,
// that subordinate item is processed as a group item.
// identifier-2 cannot be a function identifier or be reference modified, but it
// can be subscripted.
// identifier-2 must not overlap identifier-1 or identifier-3.
// identifier-2 must not specify the RENAMES clause.
// The following data items that are specified by identifier-2 are ignored by
// the XML GENERATE statement:
// - Any subordinate unnamed elementary data items or elementary FILLER
//   data items
// - Any slack bytes inserted for SYNCHRONIZED items
// - Any data item subordinate to identifier-2 that is described with the
//   REDEFINES clause or that is subordinate to such a redefining item
// - Any data item subordinate to identifier-2 that is described with the
//   RENAMES clause
// - Any group data item all of whose subordinate data items are ignored
// All data items specified by identifier-2 that are not ignored according to the
// previous rules must satisfy the following conditions:
// - Each elementary data item must either have class alphabetic,
//   alphanumeric, numeric, or national, or be an index data item. (That is,
//   no elementary data item can be described with the USAGE POINTER,
//   USAGE FUNCTION-POINTER, USAGE PROCEDURE-POINTER, or
//   USAGE OBJECT REFERENCE phrase.)
// - There must be at least one such elementary data item.
// - Each non-FILLER data-name must be unique within any immediately
//   superordinate group data item.
// - Any DBCS data-names, when converted to Unicode, must be legal as
//   names in the XML specification, version 1.0. For details about the XML
//   specification, see XML specification.
// For example, consider the following data declaration:
// 01 STRUCT.
// 02 STAT PIC X(4).
// 02 IN-AREA PIC X(100).
// 02 OK-AREA REDEFINES IN-AREA.
// 03 FLAGS PIC X.
// 03 PIC X(3).
// 03 COUNTER USAGE COMP-5 PIC S9(9).
// 03 ASFNPTR REDEFINES COUNTER USAGE FUNCTION-POINTER.
// 03 UNREFERENCED PIC X(92).
// 02 NG-AREA1 REDEFINES IN-AREA.
// 03 FLAGS PIC X.
// 03 PIC X(3).
// 03 PTR USAGE POINTER.
// 03 ASNUM REDEFINES PTR USAGE COMP-5 PIC S9(9).
// 03 PIC X(92).
// 02 NG-AREA2 REDEFINES IN-AREA.
// 03 FN-CODE PIC X.
// 03 UNREFERENCED PIC X(3).
// 03 QTYONHAND USAGE BINARY PIC 9(5).
// 03 DESC USAGE NATIONAL PIC N(40).
// 03 UNREFERENCED PIC X(12).
// The following data items from the previous example can be specified as
// identifier-2:
// - STRUCT, of which subordinate data items STAT and IN-AREA would be
//   converted to XML format. (OK-AREA, NG-AREA1, and NG-AREA2 are ignored
//   because they specify the REDEFINES clause.)
// - OK-AREA, of which subordinate data items FLAGS, COUNTER, and
//   UNREFERENCED would be converted. (The item whose data description
//   entry specifies 03 PIC X(3) is ignored because it is an elementary
//   FILLER data item. ASFNPTR is ignored because it specifies the
//   REDEFINES clause.)
// - Any of the elementary data items that are subordinate to STRUCT except:
//   – ASFNPTR or PTR (disallowed usage)
//   – UNREFERENCED OF NG-AREA2 (nonunique names for data items that are
//     otherwise eligible)
// – Any FILLER data items
// The following data items cannot be specified as identifier-2:
// - NG-AREA1, because subordinate data item PTR specifies USAGE POINTER
//   but does not specify the REDEFINES clause. (PTR would be ignored if it
//   specified the REDEFINES clause.)
// - NG-AREA2, because subordinate elementary data items have the
//   nonunique name UNREFERENCED.
// COUNT IN phrase
// If the COUNT IN phrase is specified, identifier-3 contains (after execution
// of the XML GENERATE statement) the count of generated XML character
// encoding units. If identifier-1 (the receiver) has category national, the count
// is in UTF-16 character encoding units. For all other encodings (including
// UTF-8), the count is in bytes.
// identifier-3
// The data count field. Must be an integer data item defined without
// the symbol P in its picture string.
// identifier-3 must not overlap identifier-1, identifier-2, codepage (if an
// identifier), identifier-4, or identifier-5.
// ENCODING phrase
// The ENCODING phrase, if specified, determines the encoding of the
// generated XML document.
// codepage
// Must be an unsigned integer data item or unsigned integer literal
// and must represent a valid coded character set identifier (CCSID).
// Must identify one of the code pages supported for COBOL XML
// processing as described in The encoding of XML documents in the
// Enterprise COBOL Programming Guide.
// If identifier-1 references a data item of category national, codepage
// must specify 1200, the CCSID for Unicode UTF-16.
// If identifier-1 references a data item of category alphanumeric,
// codepage must specify 1208 or the CCSID of a supported EBCDIC
// code page as listed in The encoding of XML documents in the
// Enterprise COBOL Programming Guide.
// If codepage is an identifier, it must not overlap identifier-1 or
// identifier-3.
// If the ENCODING phrase is omitted and identifier-1 is of category national,
// the document encoding is Unicode UTF-16, CCSID 1200.
// If the ENCODING phrase is omitted and identifier-1 is of category
// alphanumeric, the XML document is encoded using the code page
// specified by the CODEPAGE compiler option in effect when the source
// code was compiled.
// XML-DECLARATION phrase
// If the XML-DECLARATION phrase is specified, the generated XML
// document starts with an XML declaration that includes the XML version
// information and an encoding declaration.
// If identifier-1 is of category national, the encoding declaration has the value
// UTF-16 (encoding="UTF-16").
// If identifier-1 is of category alphanumeric, the encoding declaration is
// derived from the ENCODING phrase, if specified, or from the CODEPAGE
// compiler option in effect for the program if the ENCODING phrase is not
// specified. See the description of the ENCODING phrase for further details.
// For an example of the effect of coding the XML-DECLARATION phrase,
// see Generating XML output in the Enterprise COBOL Programming Guide.
// If the XML-DECLARATION phrase is omitted, the generated XML
// document does not include an XML declaration.
// ATTRIBUTES phrase
// If the ATTRIBUTES phrase is specified, each eligible item included in the
// generated XML document is expressed as an attribute of the XML element
// that corresponds to the data item immediately superordinate to that
// eligible item, rather than as a child element of the XML element. To be
// eligible, a data item must be elementary, must have a name other than
// FILLER, and must not specify an OCCURS clause in its data description
// entry.
// If the TYPE phrase is specified for particular identifiers, the TYPE phrase
// takes precedence for those identifiers over the WITH ATTRIBUTES phrase.
// For an example of the effect of the ATTRIBUTES phrase, see Generating
// XML output in the Enterprise COBOL Programming Guide.
// NAMESPACE and NAMESPACE-PREFIX phrases
// Use the NAMESPACE phrase to identify a namespace for the generated
// XML document. If the NAMESPACE phrase is not specified, or if
// identifier-4 has length zero or contains all spaces, the element names of
// XML documents produced by the XML GENERATE statement are not in
// any namespace.
// Use the NAMESPACE-PREFIX phrase to qualify the start and end tag of
// each element in the generated XML document with a prefix.
// If the NAMESPACE-PREFIX phrase is not specified, or if identifier-5 is of
// length zero or contains all spaces, the namespace specified by the
// NAMESPACE phrase specifies the default namespace for the document. In
// this case, the namespace declared on the root element applies by default to
// each element name in the document, including that of the root element.
// (Default namespace declarations do not apply directly to attribute names.)
// If the NAMESPACE-PREFIX phrase is specified, and identifier-5 is not of
// length zero and does not contain all spaces, then the start and end tag of
// each element in the generated document is qualified with the specified
// prefix. The prefix should therefore preferably be short. When the XML
// GENERATE statement is executed, the prefix must be a valid XML name,
// but without the colon (:), as defined in Namespaces in XML 1.0. The prefix
// can have trailing spaces, which are removed before use.
// identifier-4, literal-4; identifier-5, literal-5
// identifier-4, literal-4: The namespace identifier, which must be a
// valid Uniform Resource Identifier (URI) as defined in Uniform
// Resource Identifier (URI): Generic Syntax.
// identifier-5, literal-5: The namespace prefix, which serves as an alias
// for the namespace identifier.
// identifier-4 and identifier-5 must reference data items of category
// alphanumeric or national.
// identifier-4 and identifier-5 must not overlap identifier-1 or
// identifier-3.
// literal-4 and literal-5 must be of category alphanumeric or national,
// and must not be figurative constants.
// For full details about namespaces, see Namespaces in XML 1.0.
// For examples that show the use of the NAMESPACE and
// NAMESPACE-PREFIX phrases, see Generating XML output in the Enterprise
// COBOL Programming Guide.
// NAME phrase
// Allows you to supply element and attribute names.
// identifier-6 must reference identifier-2 or one of its subordinate data items.
// It cannot be a function identifier and cannot be reference modified or
// subscripted. It must not specify any data item which is ignored by the
// XML GENERATE statement. For more information about identifier-2, see
// the description of identifier-2. If identifier-6 is specified more than once in
// the NAME phrase, the last specification is used.
// literal-6 must be an alphanumeric or national literal containing the
// attribute or element name to be generated in the XML document
// corresponding to identifier-6. It must be a valid XML local name. If literal-6
// is a national literal, identifier-1 must reference a data item of category
// national or the encoding phrase must specify 1208.
// TYPE phrase
// Allows you to control attribute and element generation.
// identifier-7 must reference an elementary data item that is subordinate to
// identifier-2. It cannot be a function identifier and cannot be reference
// modified or subscripted. It must not specify any data item which is
// ignored by the XML GENERATE statement. For more information about
// identifier-2, see the description of identifier-2. If identifier-7 is specified more
// than once in the TYPE phrase, the last specification is used.
// - If the XML GENERATE statement also includes a WITH ATTRIBUTES
//   phrase, the TYPE phrase has precedence for identifier-7.
// - When ATTRIBUTE is specified, identifier-7 must be eligible to be an XML
//   attribute. identifier-7 is expressed in the generated XML as an attribute of
//   the XML element immediately superordinate to identifier-7 rather than as
//   a child element.
// - When ELEMENT is specified, identifier-7 is expressed in the generated
//   XML as an element. The XML element name is derived from identifier-7
//   and the element character content is derived from the converted content
//   of identifier-7 as described in “Operation of XML GENERATE” on page
//   465.
// - When CONTENT is specified, identifier-7 is expressed in the generated
//   XML as element character content of the XML element that corresponds
//   to the data item immediately superordinate to identifier-7. The value of
//   the element character content is derived from the converted content of
//   identifier-7 as described in “Operation of XML GENERATE” on page 465.
// When CONTENT is specified for multiple identifiers all corresponding
// to the same superordinate identifier, the multiple contributions to the
// element character content are concatenated.
// SUPPRESS phrase
// Allows you to identify items that are subordinate to identifier-2 and must
// be suppressed when generating the XML if they contain values that are
// specified in the WHEN clause. If the SUPPRESS phrase is specified,
// identifier-1 must be large enough to contain the generated XML document
// before any suppression.
// With the generic-suppression-phrase, elementary items subordinate to
// identifier-2 that are not otherwise ignored by XML GENERATE operations
// are identified generically for potential suppression. Either items of class
// numeric, if the NUMERIC keyword is specified, or items that are not of
// class numeric, if the NONNUMERIC keyword is specified, or both, may be
// suppressed. If the ATTRIBUTE keyword is specified, only items that would
// be expressed in the generated XML document as an XML attribute are
// identified for potential suppression. If the ELEMENT keyword is specified,
// only items that would be expressed in the generated XML document as an
// XML element are identified.
// If multiple generic-suppression-phrase are specified, the effect is cumulative.
// identifier-8 explicitly identifies items for potential suppression. It must
// reference an elementary data item that is subordinate to identifier-2 and that
// is not otherwise ignored by the XML GENERATE operations. For more
// information about that element, see the description of identifier-2. It cannot
// be a function identifier and cannot be reference modified or subscripted. If
// identifier-8 is specified more than once in the SUPPRESS phrase, the last
// specification is used. The explicit suppression specification for identifier-8
// overrides the suppression specification that is implied by any
// generic-suppression-phrase, if identifier-8 is also one of the identifiers
// generically identified.
// - If ZERO, ZEROES, or ZEROS is specified in the WHEN phrase,
//   identifier-8 or all the data items that are identified by the
//   generic-suppression-phrase must not be of USAGE DISPLAY-1.
// - If SPACE or SPACES is specified in the WHEN phrase, identifier-8 or all
//   the data items that are identified by the generic-suppression-phrase must
//   be of USAGE DISPLAY, DISPLAY-1 or NATIONAL.
// - If LOW-VALUE, LOW-VALUES, HIGH-VALUE, or HIGH-VALUES is
//   specified in the WHEN phrase, identifier-8 or all the data items that are
//   identified by the generic-suppression-phrase must be of class alphanumeric
//   or national.
// The comparison operation that determines if an item will be suppressed is
// a numeric comparison if the value specified is ZERO, ZEROS, or ZEROES,
// and the item is of category numeric or internal floating-point. For all other
// cases, the comparison operation is an alphanumeric, DBCS, or national
// comparison depending on whether the item is of usage DISPLAY,
// DISPLAY-1 or NATIONAL respectively.
// When the SUPPRESS phrase is specified, a group item subordinate to
// identifier-2 is suppressed in the generated XML document if all the eligible
// items subordinate to the group item are suppressed or if the group item
// has zero length. The root element is always generated, even if all the items
// subordinate to identifier-2 are suppressed.
// ON EXCEPTION phrase
// An exception condition exists when an error occurs during generation of
// the XML document, for example if identifier-1 is not large enough to
// contain the generated XML document. In this case, XML generation stops
// and the content of the receiver, identifier-1, is undefined. If the COUNT IN
// phrase is specified, identifier-3 contains the number of character positions
// that were generated, which can range from 0 to the length of identifier-1.
// If the ON EXCEPTION phrase is specified, control is transferred to
// imperative-statement-1. If the ON EXCEPTION phrase is not specified, the
// NOT ON EXCEPTION phrase, if any, is ignored, and control is transferred
// to the end of the XML GENERATE statement. Special register XML-CODE
// contains an exception code, as detailed in Handling XML GENERATE
// exceptions in the Enterprise COBOL Programming Guide.
// NOT ON EXCEPTION phrase
// If an exception condition does not occur during generation of the XML
// document, control is passed to imperative-statement-2, if specified, otherwise
// to the end of the XML GENERATE statement. The ON EXCEPTION
// phrase, if specified, is ignored. Special register XML-CODE contains zero
// after execution of the XML GENERATE statement.
// END-XML phrase
// This explicit scope terminator delimits the scope of XML GENERATE or
// XML PARSE statements. END-XML permits a conditional XML GENERATE
// or XML PARSE statement (that is, an XML GENERATE or XML PARSE
// statement that specifies the ON EXCEPTION or NOT ON EXCEPTION
// phrase) to be nested in another conditional statement.
// The scope of a conditional XML GENERATE or XML PARSE statement can
// be terminated by:
// - An END-XML phrase at the same level of nesting
// - A separator period
// END-XML can also be used with an XML GENERATE or XML PARSE
// statement that does not specify either the ON EXCEPTION or the NOT ON
// EXCEPTION
// ... more details p465 Nested XML GENERATE or XML PARSE statements ...
// ... more details p465->466 Operation of XML GENERATE ...
// ... more details p466->467 Format conversion of elementary data ...
// ... more details p467->468 Trimming of generated XML data ...
// ... more details p468 XML element name and attribute name formation ...

xmlGenerateStatement:
	XML GENERATE receivingField=storageArea1 
	FROM dataItemToConvertToXml=variable1
	(COUNT IN? generatedXmlCharsCount=storageArea1)?
	(WITH? ENCODING codepage)?
	(WITH? XML_DECLARATION)?
	(WITH? ATTRIBUTES)?
	(NAMESPACE IS? namespace=alphanumericVariable2 
		(NAMESPACE_PREFIX IS? namespacePrefix=alphanumericVariable2)? )?
	(NAME OF? xmlNameMapping+)?
	(TYPE OF? xmlTypeMapping+)?
	(SUPPRESS xmlSuppressDirective+)?;
		
xmlNameMapping:
	subordinateDataItem=variable1 IS? xmlNameToGenerate=alphanumericValue2;

xmlTypeMapping:
	subordinateDataItem=variable1 IS? (ATTRIBUTE | ELEMENT | CONTENT);

xmlSuppressDirective:	
	( subordinateDataItem=variable1 |
	(EVERY (ATTRIBUTE | ELEMENT | ((NUMERIC | NONNUMERIC) (ATTRIBUTE | ELEMENT)?)))?)
	// Only figurative constants are allowed: ZERO | ZEROES | ZEROS | SPACE | SPACES | LOW_VALUE | LOW_VALUES | HIGH_VALUE | HIGH_VALUES
	WHEN repeatedCharacterValue3 (OR? repeatedCharacterValue3)*;
	
xmlStatementEnd: END_XML;

// codepage
// Must be an unsigned integer data item or unsigned integer literal
// and must represent a valid coded character set identifier (CCSID).
// Must identify one of the code pages supported for COBOL XML
// processing as described in The encoding of XML documents in the
// Enterprise COBOL Programming Guide.

codepage: integerVariable1;

// p469: XML PARSE statement
// The XML PARSE statement is the COBOL language interface to the high-speed
// XML parser that is part of the COBOL run time.
// The XML PARSE statement parses an XML document into its individual pieces and
// passes each piece, one at a time, to a user-written processing procedure.
// identifier-1
// identifier-1 must be an elementary data item of category national, a national
// group, an elementary data item of category alphanumeric, or an
// alphanumeric group item. identifier-1 cannot be a function-identifier.
// identifier-1 contains the XML document character stream.
// If identifier-1 is a national group item, identifier-1 is processed as an
// elementary data item of category national.
// If identifier-1 is of category national, its content must be encoded using
// Unicode UTF-16BE (CCSID 1200). identifier-1 must not contain any
// character entities that are represented using multiple encoding units. Use a
// character reference to represent any such characters, for example:
// - "&#x67603;" or
// - "&#x10813;"
// The letter x must be lowercase.
// If identifier-1 is of category alphanumeric, its content must be encoded
// using one of the character sets listed in Coded character sets for XML
// documents in the Enterprise COBOL Programming Guide. If identifier-1 is
// alphanumeric and contains an XML document that does not specify an
// encoding declaration, the XML document is parsed with the code page
// specified by the CODEPAGE compiler option.
// RETURNING NATIONAL phrase
// When identifier-1 references a data item of category alphanumeric and the
// RETURNING NATIONAL phrase is specified, XML document fragments
// are automatically converted to Unicode UTF-16 representation and
// returned to the processing procedure in the national special registers
// XML-NTEXT, XML-NNAMESPACE, and XML-NNAMESPACE-PREFIX.
// When the RETURNING NATIONAL phrase is not specified and identifier-1
// references a data item of category alphanumeric, the XML document
// fragments are returned to the processing procedure in the alphanumeric
// special registers XML-TEXT, XML-NAMESPACE, and XML-NAMESPACEPREFIX.
// When identifier-1 references a national data item, XML document fragments
// are always returned in Unicode UTF-16 representation in the national
// special registers XML-NTEXT, XML-NNAMESPACE, and
// XML-NNAMESPACE-PREFIX.
// VALIDATING phrase
// The VALIDATING phrase specifies that the parser should validate the
// XML document against an XML schema while parsing it. In Enterprise
// COBOL, the schema used for XML validation is in a preprocessed format
// known as Optimized Schema Representation or OSR.
// See Parsing XML documents with validation in the Enterprise COBOL
// Programming Guide for details.
// If the FILE keyword is not specified, identifier-2 must reference a data item
// that contains the optimized XML schema. identifier-2 must be of category
// alphanumeric and cannot be a function-identifier.
// If the FILE keyword is specified, xml-schema-name-1 identifies an existing
// z/OS UNIX file or MVS data set that contains the optimized XML schema.
// xml-schema-name-1 must be associated with the external file name of the
// schema by using the XML-SCHEMA clause. For more information about
// the XML-SCHEMA clause, see “SPECIAL-NAMES paragraph” on page 112.
// Restriction: XML validation using the FILE keyword is not supported
// under CICS®.
// During parsing with validation, normal XML events are returned as for
// nonvalidating parsing until an exception occurs due to a validation error
// or other error in the document.
// When an XML document is not valid, the parser signals an XML exception
// and passes control to the processing procedure with special register
// XML-EVENT containing 'EXCEPTION' and special-register XML-CODE
// containing return code 24 in the high-order halfword and a reason code in
// the low-order halfword.
// For information about the return code and reason code for exceptions that
// might occur when parsing XML documents with validation, see XML
// PARSE exceptions in the Enterprise COBOL Programming Guide.
// ENCODING phrase
// The ENCODING phrase specifies an encoding that is assumed for the
// source XML document in identifier-1. codepage must be an unsigned integer
// data item or an unsigned integer literal that represents a valid coded
// character set identifier (CCSID). The ENCODING phrase specification
// overrides the encoding specified by the CODEPAGE compiler option. The
// encoding specified in any XML declaration is always ignored.
// If identifier-1 references a data item of category national, codepage must
// specify CCSID 1200, for Unicode UTF-16.
// If identifier-1 references a data item of category alphanumeric, codepage
// must specify CCSID 1208 for UTF-8 or a CCSID for a supported EBCDIC
// or ASCII codepage. See Coded character sets for XML documents in the
// Enterprise COBOL Programming Guide for details.
// PROCESSING PROCEDURE phrase
// Specifies the name of a procedure to handle the various events that the
// XML parser generates.
// procedure-name-1, procedure-name-2
// Must name a section or paragraph in the PROCEDURE DIVISION.
// When both procedure-name-1 and procedure-name-2 are specified, if
// either is a procedure name in a declarative procedure, both must
// be procedure names in the same declarative procedure.
// procedure-name-1
// Specifies the first (or only) section or paragraph in the processing
// procedure.
// procedure-name-2
// Specifies the last section or paragraph in the processing procedure.
// For each XML event, the parser transfers control to the first statement of
// the procedure named procedure-name-1. Control is always returned from the
// processing procedure to the XML parser. The point from which control is
// returned is determined as follows:
// - If procedure-name-1 is a paragraph name and procedure-name-2 is not
//   specified, the return is made after the execution of the last statement of
//   the procedure-name-1 paragraph.
// - If procedure-name-1 is a section name and procedure-name-2 is not
//   specified, the return is made after the execution of the last statement of
//   the last paragraph in the procedure-name-1 section.
// - If procedure-name-2 is specified and it is a paragraph name, the return is
//   made after the execution of the last statement of the procedure-name-2
//   paragraph.
// - If procedure-name-2 is specified and it is a section name, the return is
//   made after the execution of the last statement of the last paragraph in
//   the procedure-name-2 section.
// The only necessary relationship between procedure-name-1 and
// procedure-name-2 is that they define a consecutive sequence of operations to
// execute, beginning at the procedure named by procedure-name-1 and ending
// with the execution of the procedure named by procedure-name-2.
// If there are two or more logical paths to the return point, then
// procedure-name-2 can name a paragraph that consists of only an EXIT
// statement; all the paths to the return point must then lead to this
// paragraph.
// The processing procedure consists of all the statements at which XML
// events are handled. The range of the processing procedure includes all
// statements executed by CALL, EXIT, GO TO, GOBACK, INVOKE, MERGE,
// PERFORM, and SORT statements that are in the range of the processing
// procedure, as well as all statements in declarative procedures that are
// executed as a result of the execution of statements in the range of the
// processing procedure.
// The range of the processing procedure must not cause the execution of any
// GOBACK or EXIT PROGRAM statement, except to return control from a
// method or program to which control was passed by an INVOKE or CALL
// statement, respectively, that is executed in the range of the processing
// procedure.
// The range of the processing procedure must not cause the execution of an
// XML PARSE statement, unless the XML PARSE statement is executed in a
// method or outermost program to which control was passed by an INVOKE
// or CALL statement that is executed in the range of the processing
// procedure.
// A program executing on multiple threads can execute the same XML
// statement or different XML statements simultaneously.
// The processing procedure can terminate the run unit with a STOP RUN
// statement.
// For more details about the processing procedure, see “Control flow” on
// page 473.
// ON EXCEPTION
// The ON EXCEPTION phrase specifies imperative statements that are
// executed when the XML PARSE statement raises an exception condition.
// An exception condition exists when the XML parser detects an error in
// processing the XML document. The parser first signals an XML exception
// by passing control to the processing procedure with special register
// XML-EVENT containing 'EXCEPTION'. The parser also provides a numeric
// error code in special register XML-CODE, as detailed in Handling XML
// PARSE exceptions in the Enterprise COBOL Programming Guide.
// An exception condition also exists if the processing procedure sets
// XML-CODE to -1 before returning to the parser for any normal XML event.
// In this case, the parser does not signal an EXCEPTION XML event and
// parsing is terminated.
// If the ON EXCEPTION phrase is specified, the parser transfers control to
// imperative-statement-1. If the ON EXCEPTION phrase is not specified, the
// NOT ON EXCEPTION phrase, if any, is ignored and control is transferred
// to the end of the XML PARSE statement.
// Special register XML-CODE contains the numeric error code for the XML
// exception or -1 after execution of the XML PARSE statement.
// If the processing procedure handles the XML exception event and sets
// XML-CODE to zero before returning control to the parser, the exception
// condition no longer exists. If no other unhandled exceptions occur before
// termination of the parser, control is transferred to imperative-statement-2 of
// the NOT ON EXCEPTION phrase, if specified.
// NOT ON EXCEPTION
// The NOT ON EXCEPTION phrase specifies imperative statements that are
// executed when no exception condition exists at the termination of XML
// PARSE processing.
// If an exception condition does not exist at termination of XML PARSE
// processing, control is transferred to imperative-statement-2 of the NOT ON
// EXCEPTION phrase, if specified. If the NOT ON EXCEPTION phrase is
// not specified, control is transferred to the end of the XML PARSE
// statement. The ON EXCEPTION phrase, if specified, is ignored.
// Special register XML-CODE contains zero after execution of the XML
// PARSE statement.
// END-XML phrase
// This explicit scope terminator delimits the scope of XML GENERATE or
// XML PARSE statements. END-XML permits a conditional XML GENERATE
// or XML PARSE statement (that is, an XML GENERATE or XML PARSE
// statement that specifies the ON EXCEPTION or NOT ON EXCEPTION
// phrase) to be nested in another conditional statement.
// The scope of a conditional XML GENERATE or XML PARSE statement can
// be terminated by:
// - An END-XML phrase at the same level of nesting
// - A separator period
// END-XML can also be used with an XML GENERATE or XML PARSE
// statement that does not specify either the ON EXCEPTION or NOT ON
// EXCEPTION phrase.
// For more information about explicit scope terminators, see “Delimited
// scope statements” on page 280.
// ... more information p473 Nested XML GENERATE or XML PARSE statements ...
// ... more information p473->474 Control flow ...

xmlParseStatement:
                     XML PARSE xmlTextToParse=variable1
                     (WITH? ENCODING codepage)? 
                     (RETURNING NATIONAL)?
                     (VALIDATING WITH? (optimizedXmlSchemaData=variable1 | (FILE optimizedXmlSchemaFile=xmlSchemaNameReference)))?
                     PROCESSING PROCEDURE IS? (procedureName | proceduresRange);


// --- Conditions code elements syntax ---

atEndCondition:
	AT? END;

notAtEndCondition:
	NOT AT? END;

atEndOfPageCondition:
	AT? (END_OF_PAGE | EOP);

notAtEndOfPageCondition:
	NOT AT? (END_OF_PAGE | EOP);

invalidKeyCondition:
	INVALID KEY?;

notInvalidKeyCondition:
	NOT INVALID KEY?;

onExceptionCondition:
	ON? EXCEPTION;

notOnExceptionCondition:
	NOT ON? EXCEPTION;

onOverflowCondition:
	ON? OVERFLOW;

notOnOverflowCondition:
	NOT ON? OVERFLOW;

onSizeErrorCondition:
	ON? SIZE ERROR;

notOnSizeErrorCondition:
	NOT ON? SIZE ERROR;
            

// ------------------------------
// Start of DB2 coprocessor
// IBM Enterprise Cobol 5.1 for zOS - Programming Guide.pdf
// ------------------------------

// p423: To communicate with DB2, do these steps:
// - Code any SQL statements that you need, delimiting them with EXEC SQL and
//   END-EXEC statements.
// - Either use the DB2 stand-alone precompiler, or compile with the SQL compiler
//   option and use the DB2 coprocessor.
// When you use the DB2 coprocessor (called SQL statement coprocessor by DB2), the
// compiler handles your source programs that contain embedded SQL statements
// without your having to use a separate precompile step.
// To use the DB2 coprocessor, specify the SQL compiler option.
// When the compiler encounters SQL statements in the source program, it interfaces
// with the DB2 coprocessor. All text between EXEC SQL and END-EXEC statements is
// passed to the coprocessor. The coprocessor takes appropriate actions for the SQL
// statements and indicates to the compiler which native COBOL statements to
// generate for them.
// Although the use of a separate precompile step continues to be supported, it is
// recommended that you use the coprocessor instead:
// - Interactive debugging with Debug Tool is enhanced when you use the
//   coprocessor because you see the SQL statements (not the generated COBOL
//   source) in the listing.
// - The COBOL compiler listing includes the error diagnostics (such as syntax errors
//   in the SQL statements) that the DB2 coprocessor generates.
// - Certain restrictions on the use of COBOL language that apply when you use the
//   precompile step do not apply when you use the DB2 coprocessor. With the
//   coprocessor:
//   – You can use SQL statements in any nested program. (With the precompiler,
//     SQL statements are restricted to the outermost program.)
//   – You can use SQL statements in copybooks.
//   – REPLACE statements work in SQL statements.

// ... more details p427->429 Compiling with the SQL option ...

// DB2 11 for zOs - Application Programming and SQL Guide - p326:
// ** You can code SQL statements in certain COBOL program sections.
// The allowable sections are shown in the following table.
// Table 64. Allowable SQL statements for COBOL program sections
// SQL statement Program section
// BEGIN DECLARE SECTION
// END DECLARE SECTION
// WORKING-STORAGE SECTION1 or LINKAGE
// SECTION
// INCLUDE SQLCA WORKING-STORAGE SECTION1 or LINKAGE
// SECTION
// INCLUDE text-file-name PROCEDURE DIVISION or DATA DIVISION2
// DECLARE TABLE
// DECLARE CURSOR
// DATA DIVISION or PROCEDURE DIVISION
// DECLARE VARIABLE WORKING-STORAGE SECTION1
// Other PROCEDURE DIVISION
// Notes:
// 1. If you use the DB2 coprocessor, you can use the LOCAL-STORAGE SECTION wherever
// WORKING-STORAGE SECTION is listed in the table.
// 2. When including host variable declarations, the INCLUDE statement must be in the
// WORKING-STORAGE SECTION or the LINKAGE SECTION.
// You cannot put SQL statements in the DECLARATIVES section of a COBOL
// program.
// ** Each SQL statement in a COBOL program must begin with EXEC SQL and end
// with END-EXEC. If you are using the DB2 precompiler, the EXEC and SQL
// keywords must appear on one line, but the remainder of the statement can appear
// on subsequent lines. If you are using the DB2 coprocessor, the EXEC and SQL
// keywords can be on different lines. Do not include any tokens between the two
// keywords EXEC and SQL except for COBOL comments, including debugging lines.
// Do not include SQL comments between the keywords EXEC and SQL.
// ** If the SQL statement appears between two COBOL statements, the period after
// END-EXEC is optional and might not be appropriate. If the statement appears in
// an IF...THEN set of COBOL statements, omit the ending period to avoid
// inadvertently ending the IF statement.
// You might code an UPDATE statement in a COBOL program as follows:
// EXEC SQL
// UPDATE DSN8B10.DEPT
// SET MGRNO = :MGR-NUM
// WHERE DEPTNO = :INT-DEPT
// END-EXEC.
// ** Comments: You can include COBOL comment lines (* in column 7) in SQL
// statements wherever you can use a blank. If you are using the DB2 precompiler,
// you cannot include COBOL comment lines between the keywords EXEC and SQL.
// The precompiler treats COBOL debugging lines and page-eject lines (/ in column
// 7) as comment lines. The DB2 coprocessor treats the debugging lines based on the
// COBOL rules, which depend on the WITH DEBUGGING mode setting.
// For an SQL INCLUDE statement, the DB2 precompiler treats any text that follows
// the period after END-EXEC, and on the same line as END-EXEC, as a comment.
// The DB2 coprocessor treats this text as part of the COBOL program syntax.
// In addition, you can include SQL comments ('--') in any embedded SQL statement.
// ** Debugging lines: The DB2 precompiler ignores the 'D' in column 7 on debugging
// lines and treats it as a blank. The DB2 coprocessor follows the COBOL language
// rules regarding debugging lines.
// ** Continuation for SQL statements: The rules for continuing a character string
// constant from one line to the next in an SQL statement embedded in a COBOL
// program are the same as those for continuing a non-numeric literal in COBOL.
// However, you can use either a quote or an apostrophe as the first nonblank
// character in area B of the continuation line. The same rule applies for the
// continuation of delimited identifiers and does not depend on the string delimiter
// option.
// To conform with SQL standard, delimit a character string constant with an
// apostrophe, and use a quote as the first nonblank character in area B of the
// continuation line for a character string constant.
// Continued lines of an SQL statement can be in columns 8 through 72 when using
// the DB2 precompiler and columns 12 through 72 when using the DB2 coprocessor.
// ** COPY: If you use the DB2 precompiler, do not use a COBOL COPY statement
// within host variable declarations. If you use the DB2 coprocessor, you can use
// COBOL COPY.
// ** REPLACE: If you use the DB2 precompiler, the REPLACE statement has no effect
// on SQL statements. It affects only the COBOL statements that the precompiler
// generates.
// If you use the DB2 coprocessor, the REPLACE statement replaces text strings in
// SQL statements as well as in generated COBOL statements.
// Declaring tables and views: Your COBOL program should include the statement
// DECLARE TABLE to describe each table and view the program accesses. You can
// use the DB2 declarations generator (DCLGEN) to generate the DECLARE TABLE
// statements. You should include the DCLGEN members in the DATA DIVISION.
// ** Dynamic SQL in a COBOL program: In general, COBOL programs can easily
// handle dynamic SQL statements. COBOL programs can handle SELECT statements
// if the data types and the number of fields returned are fixed. If you want to use
// variable-list SELECT statements, use an SQLDA.
// ** Including code: To include SQL statements or COBOL host variable declarations
// from a member of a partitioned data set, use the following SQL statement in the
// source code where you want to include the statements:
// EXEC SQL INCLUDE member-name END-EXEC.
// If you are using the DB2 precompiler, you cannot nest SQL INCLUDE statements.
// In this case, do not use COBOL verbs to include SQL statements or host variable
// declarations, and do not use the SQL INCLUDE statement to include CICS
// preprocessor related code. In general, if you are using the DB2 precompiler, use the
// SQL INCLUDE statement only for SQL-related coding. If you are using the COBOL
// DB2 coprocessor, none of these restrictions apply.
// ** Use the 'EXEC SQL' and 'END-EXEC' keyword pair to include SQL statements
// only. COBOL statements, such as COPY or REPLACE, are not allowed.
// ** Margins: You must code SQL statements that begin with EXEC SQL in columns 12
// through 72. Otherwise the DB2 precompiler does not recognize the SQL statement.
// ** Names: You can use any valid COBOL name for a host variable. Do not use
// external entry names or access plan names that begin with 'DSN', and do not use
// host variable names that begin with 'SQL'. These names are reserved for DB2.
// ** Sequence numbers: The source statements that the DB2 precompiler generates do
// not include sequence numbers.
// ** Statement labels: You can precede executable SQL statements in the PROCEDURE
// DIVISION with a paragraph name.
// ** WHENEVER statement: The target for the GOTO clause in an SQL statement
// WHENEVER must be a section name or unqualified paragraph name in the
// PROCEDURE DIVISION.
// ** Special COBOL considerations: The following considerations apply to programs
// written in COBOL:
// - In a COBOL program that uses elements in a multi-level structure as host
//   variable names, the DB2 precompiler generates the lowest two-level names.
// - To avoid truncating numeric values, use either of the following methods:
//   – Use the COMP-5 data type for binary integer host variables.
//   – Specify the COBOL compiler option:
//   - TRUNC(OPT) if you are certain that the data being moved to each binary
//     variable by the application does not have a larger precision than is defined
//     in the PICTURE clause of the binary variable.
//   - TRUNC(BIN) if the precision of data being moved to each binary variable
//     might exceed the value in the PICTURE clause.
//     DB2 assigns values to binary integer host variables as if you had specified the
//     COBOL compiler option TRUNC(BIN) or used the COMP-5 data type.
// - If you are using the DB2 precompiler and your COBOL program contains
//   several entry points or is called several times, the USING clause of the entry
//   statement that executes before the first SQL statement executes must contain the
//   SQLCA and all linkage section entries that any SQL statement uses as host
//   variables.
// - If you use the DB2 precompiler, no compiler directives should appear between
//   the PROCEDURE DIVISION and the DECLARATIVES statement.
// - Do not use COBOL figurative constants (such as ZERO and SPACE), symbolic
//   characters, reference modification, and subscripts within SQL statements.
// - Observe the rules for naming SQL identifiers. However, for COBOL only, the
//   names of SQL identifiers can follow the rules for naming COBOL words, if the
//   names do not exceed the allowable length for the DB2 object. For example, the
//   name 1ST-TIME is a valid cursor name because it is a valid COBOL word, but
//   the name 1_TIME is not valid because it is not a valid SQL identifier or a valid
//   COBOL word.
// - Observe these rules for hyphens:
//   – Surround hyphens used as subtraction operators with spaces. DB2 usually
//     interprets a hyphen with no spaces around it as part of a host variable name.
//   – You can use hyphens in SQL identifiers under either of the following
//     circumstances:
//     - The application program is a local application that runs on DB2 for z/OS
//        Version 11 or later.
//     - The application program accesses remote sites, and the local site and
//       remote sites are DB2 for z/OS Version 11 or later.
// - If you include an SQL statement in a COBOL PERFORM ... THRU paragraph and
//   also specify the SQL statement WHENEVER ... GO, the COBOL compiler returns
//   the warning message IGYOP3094. That message might indicate a problem. This
//   usage is not recommended.
// - If you are using the DB2 precompiler, all SQL statements and any host variables
//   they reference must be within the first program when using nested programs or
//   batch compilation.
// - If you are using the DB2 precompiler, your COBOL programs must have a
//   DATA DIVISION and a PROCEDURE DIVISION. Both divisions and the
//   WORKING-STORAGE SECTION must be present in programs that contain SQL
//   statements. However, if your COBOL programs requires the LOCAL-STORAGE
//   SECTION, then the DB2 coprocessor should be used instead of the DB2
//   precompiler.

// p424: Declare all host variables that you use in SQL statements in the WORKING-STORAGE
// SECTION, LOCAL-STORAGE SECTION, or LINKAGE SECTION. However, you do not need
// to identify them with EXEC SQL BEGIN DECLARE SECTION and EXEC SQL END
// DECLARE SECTION.

// p425: You can code any of the following USAGE clauses to describe host variables for
// character data that you use in EXEC SQL statements: USAGE DISPLAY for single-byte
// or UTF-8 data, USAGE DISPLAY-1 for DBCS data, or USAGE NATIONAL for UTF-16
// data.
// When you use the stand-alone DB2 precompiler, you must specify the code page
// (CCSID) in EXEC SQL DECLARE statements for host variables that are declared with
// USAGE NATIONAL. You must specify the code page for host variables that are
// declared with USAGE DISPLAY or DISPLAY-1 only if the CCSID that is in effect for
// the COBOL CODEPAGE compiler option does not match the CCSIDs that are used by
// DB2 for character and graphic data.
// Consider the following code. The two highlighted statements are unnecessary
// when you use the integrated DB2 coprocessor (with the SQLCCSID compiler option,
// as detailed in the related concept below), because the code-page information is
// handled implicitly.
// CBL CODEPAGE(1140) NSYMBOL(NATIONAL)
// . . .
// WORKING-STORAGE SECTION.
// EXEC SQL INCLUDE SQLCA END-EXEC.
// 01 INT1 PIC S9(4) USAGE COMP.
// 01 C1140.
// 49 C1140-LEN PIC S9(4) USAGE COMP.
// 49 C1140-TEXT PIC X(50).
// EXEC SQL DECLARE :C1140 VARIABLE CCSID 1140 END-EXEC.
// 01 G1200.
// 49 G1200-LEN PIC S9(4) USAGE COMP.
// 49 G1200-TEXT PIC N(50) USAGE NATIONAL.
// EXEC SQL DECLARE :G1200 VARIABLE CCSID 1200 END-EXEC.
// . . .
// EXEC SQL FETCH C1 INTO :INT1, :C1140, :G1200 END-EXEC.
// If you specify EXEC SQL DECLARE variable-name VARIABLE CCSID nnnn END-EXEC, that
// specification overrides the implied CCSID. For example, the following code would
// cause DB2 to treat C1208-TEXT as encoded in UTF-8 (CCSID 1208) rather than as
// encoded in the CCSID in effect for the COBOL CODEPAGE compiler option:
// 01 C1208.
// 49 C1208-LEN PIC S9(4) USAGE COMP.
// 49 C1208-TEXT PIC X(50).
// EXEC SQL DECLARE :C1208 VARIABLE CCSID 1208 END-EXEC.
// The NSYMBOL compiler option has no effect on a character literal inside an EXEC SQL
// statement. Character literals in an EXEC SQL statement follow the SQL rules for
// character constants. 

// p426: You can use national decimal host variables in EXEC SQL statements when you use
// either the integrated DB2 coprocessor or the DB2 precompiler. You do not need to
// specify the CCSID in EXEC SQL DECLARE statements in either case. CCSID 1200 is
// used automatically.
// Any national decimal host variable that you specify in an EXEC SQL statement must
// have the following characteristics:
// - It must be signed.
// - It must be specified with the SIGN LEADING SEPARATE clause.
// - USAGE NATIONAL must be in effect implicitly or explicitly.
// You can use a national group item as a host variable in an EXEC SQL statement. The
// national group item is treated with group semantics (that is, as shorthand for the
// set of host variables that are subordinate to the group item) rather than as an
// elementary item.
// Because all subordinate items in a national group must have USAGE NATIONAL, a
// national group item cannot describe a variable-length string.
// For binary data items that you specify in an EXEC SQL statement, you can declare
// the data items as either USAGE COMP-5 or as USAGE BINARY, COMP, or COMP-4.
// If you declare the binary data items as USAGE BINARY, COMP, or COMP-4, use the
// TRUNC(BIN) option. (This technique might have a larger effect on performance than
// using USAGE COMP-5 on individual data items.) If instead TRUNC(OPT) or TRUNC(STD)
// is in effect, the compiler accepts the items but the data might not be valid because
// of the decimal truncation rules. You need to ensure that truncation does not affect
// the validity of the data.

// ... more details p429-> COBOL and DB2 CCSID determination ...

// p432: Precompiler: With the DB2 precompiler, a COBOL alphanumeric data item can be
// used as a host variable to hold DB2 character data that has subtype FOR BIT DATA.
// An explicit EXEC SQL DECLARE VARIABLE statement that declares that host variable
// as FOR BIT DATA is not required.
// Coprocessor: With the DB2 coprocessor, a COBOL alphanumeric data item can be
// used as a host variable to hold DB2 character data that has subtype FOR BIT DATA
// if an explicit EXEC SQL DECLARE VARIABLE statement for that host variable is
// specified in the COBOL program. For example:
// EXEC SQL DECLARE :HV1 VARIABLE FOR BIT DATA END-EXEC.
// As an alternative to adding EXEC SQL DECLARE . . . FOR BIT DATA statements, you
// can use the NOSQLCCSID compiler option. For details, see the related reference about
// code-page determination below.

// => see CobolCompilerDirectives.g4

// p424: Delimit SQL statements with EXEC SQL and END-EXEC. The EXEC SQL and END-EXEC
// delimiters must each be complete on one line. You cannot continue them across
// multiple lines. Do not code COBOL statements within EXEC SQL statements.
// Restriction: You cannot use SQL statements in object-oriented classes or methods.

// p427; When DB2 finishes executing an SQL statement, DB2 sends a return code in the
// SQLCA structure, with one exception, to indicate whether the operation succeeded
// or failed. In your program, test the return code and take any necessary action.
// The exception occurs when a program runs under DSN from one of the alternate
// entry points of the TSO batch mode module IKJEFT01 (IKJEFT1A or IKJEFT1B). In
// this case, the return code is passed in register 15.
// After execution of SQL statements, the content of the RETURN-CODE special register
// might not be valid. Therefore, even if your COBOL program terminates normally
// after successfully using SQL statements, the job step could end with an undefined
// return code. To ensure that a meaningful return code is given at termination, set
// the RETURN-CODE special register before terminating your program.

// p432: Precompiler: With the DB2 precompiler, COBOL REPLACE statements and the
// REPLACING phrase of the COPY statement act on the expanded source created from
// the EXEC SQL statement. COBOL rules for REPLACE and REPLACING are used.
// Coprocessor: With the DB2 coprocessor, REPLACE and COPY . . . REPLACING
// statements act on the original source program, including EXEC SQL statements.
// Different behavior can result, as in the following example:
// REPLACE == ABC == By == XYZ ==.
// 01 G.
// 02 ABC PIC X(10).
// . . .
// EXEC SQL SELECT * INTO :G.ABC FROM TABLE1 END-EXEC
// With the precompiler, the reference to G.ABC will appear as ABC of G in the
// expanded source and will be replaced with XYZ of G. With the coprocessor,
// replacement will not occur, because ABC is not delimited by separators in the
// original source string G.ABC.

// p432: Precompiler: The DB2 precompiler ignores any code that follows END-EXEC
// statements on the same line.
// Coprocessor: The DB2 coprocessor processes code that follows END-EXEC statements
// on the same line.

// p432: Precompiler: The DB2 precompiler does not require that host variable references be
// unique. The first definition that maps to a valid DB2 data type is used.
// Coprocessor: The DB2 coprocessor requires that each host variable reference be
// unique. The coprocessor diagnoses nonunique references to host variables. You
// must fully qualify host variable references to make them unique.

// p432: Precompiler: The DB2 precompiler requires that EXEC SQL statements start in
// columns 12 through 72. Continuation lines of the statements can start anywhere in
// columns 8 through 72.
// Coprocessor: The DB2 coprocessor requires that all lines of an EXEC SQL statement,
// including continuation lines, be coded in columns 12 through 72.

execStatement:
                 (EXEC | EXECUTE) execTranslatorName 
                 alphanumericValue8* 
                 execStatementEnd;

execStatementEnd: END_EXEC;

// ------------------------------
// End of DB2 coprocessor
// ------------------------------
