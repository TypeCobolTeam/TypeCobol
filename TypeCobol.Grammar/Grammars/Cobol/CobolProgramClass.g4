// IBM Enterprise Cobol 5.1 for zOS

// -----------------------------------------------------------------------
// Grammar for the SECOND step of the Cobol parser : build a syntax
// TREE representing a Cobol program or a Cobol class from the list of 
// elementary CodeElements recognized by the first step of the Cobol parser.
// -----------------------------------------------------------------------

grammar CobolProgramClass;

// Code Elements produced by the first parsing step (see CobolCodeElements.g4 grammar)
// are listed as TOKEN TYPES in this grammar.

tokens 
{ 
    // Code structure

    // -- Program --
    ProgramIdentification,
    ProgramEnd,
    // -- Class --
    ClassIdentification,
    ClassEnd,
    FactoryIdentification,
    FactoryEnd,
    ObjectIdentification,
    ObjectEnd,
    MethodIdentification,
    MethodEnd,
    // -- Division --
    EnvironmentDivisionHeader,
    DataDivisionHeader,
    ProcedureDivisionHeader,
    DeclarativesHeader,
    DeclarativesEnd,
    // -- Section --
    SectionHeader,
    ConfigurationSectionHeader,
    InputOutputSectionHeader,
    FileSectionHeader,
    WorkingStorageSectionHeader,
    LocalStorageSectionHeader,
    LinkageSectionHeader,
    // -- Paragraph --
    ParagraphHeader,
    FileControlParagraphHeader,
    IOControlParagraphHeader,
    // -- Sentence --
    SentenceEnd,

    // Entries

    // -- Data Division --
    FileDescriptionEntry,
    DataDescriptionEntry,
    // -- InputOutput Section --
    FileControlEntry,
    IOControlEntry,

    // Paragraphs

    // --Configuration Section --
    SourceComputerParagraph,
    ObjectComputerParagraph,
    SpecialNamesParagraph,
    RepositoryParagraph,          

    // Statements

    AcceptStatement,
    AddStatement,
    AlterStatement,
    CallStatement,
    CancelStatement,
    CloseStatement,
    ComputeStatement,
    ContinueStatement,
    DeleteStatement,
    DisplayStatement,
    DivideStatement,
    EntryStatement,
    EvaluateStatement,
    ExecStatement,
    ExitMethodStatement,
    ExitProgramStatement,
    ExitStatement,
    GobackStatement,
    GotoStatement,
    IfStatement,
    InitializeStatement,
    InspectStatement,
    InvokeStatement,
    MergeStatement,
    MoveStatement,
    MultiplyStatement,
    NextSentenceStatement,
    OpenStatement,
    PerformProcedureStatement,
    PerformStatement,
    ReadStatement,
    ReleaseStatement,
    ReturnStatement,
    RewriteStatement,
    SearchStatement,
    SetStatement,
    SortStatement,
    StartStatement,
    StopStatement,
    StringStatement,
    SubtractStatement,
    UnstringStatement,
    UseStatement,
    WriteStatement,
    XmlGenerateStatement,
    XmlParseStatement,

    // Statement conditions

    AtEndCondition,
    NotAtEndCondition,
    AtEndOfPageCondition,
    NotAtEndOfPageCondition,
    OnExceptionCondition,
    NotOnExceptionCondition,
    OnOverflowCondition,
    NotOnOverflowCondition,
    InvalidKeyCondition,
    NotInvalidKeyCondition,
    OnSizeErrorCondition,
    NotOnSizeErrorCondition,
    ElseCondition,
    WhenEvaluateCondition,
    WhenOtherCondition,
    WhenConditionalExpression,

    // Statement ends

    AddStatementEnd,
    CallStatementEnd,
    ComputeStatementEnd,
    DeleteStatementEnd,
    DivideStatementEnd,
    EvaluateStatementEnd,
    IfStatementEnd,
    InvokeStatementEnd,
    MultiplyStatementEnd,
    PerformStatementEnd,
    ReadStatementEnd,
    ReturnStatementEnd,
    RewriteStatementEnd,
    SearchStatementEnd,
    StartStatementEnd,
    StringStatementEnd,
    SubtractStatementEnd,
    UnstringStatementEnd,
    WriteStatementEnd,
    XmlStatementEnd
}

// --- Starting parser rule for PHASE 2 of parsing ---

// p103 : You cannot include a class definition in a sequence of programs or other class
// definitions in a single compilation group. Each class must be specified as a
// separate source file; that is, a class definition cannot be included in a batch
// compile.

cobolCompilationUnit : 
                         (cobolProgram | cobolClass) EOF;

// --- COBOL PROGRAM ---

// p83 : A COBOL source program is a syntactically correct set of COBOL statements.
// p83 : A nested program is a program that is contained in another program.
// p83 : Sibling programs are programs that are directly contained in the same program.

// p83 : With the exception of the COPY and REPLACE statements and the end program marker,
//       the statements, entries, paragraphs, and sections of a COBOL source program 
//       are grouped into the following four divisions : 
//       IDENTIFICATION DIVISION,ENVIRONMENT DIVISION,DATA DIVISION,PROCEDURE DIVISION

// p83 : The end of a COBOL source program is indicated by the END PROGRAM marker.
//       If there are no nested programs, the absence of additional source program lines
//       also indicates the end of a COBOL program.

// p97 : The IDENTIFICATION DIVISION must be the first division in each COBOL source
// program, factory definition, object definition, and method definition. The
// identification division names the program, class, or method and identifies the
// factory definition and object definition. The IDENTIFICATION DIVISION can
// include the date a program, class, or method was written, the date of compilation,
// and other such documentary information.

// p84 : Format: COBOL source program

cobolProgram:
	ProgramIdentification
	environmentDivision?
	dataDivision?
	procedureDivision?
                cobolProgram* 
                ProgramEnd?
                ;

environmentDivision:
	EnvironmentDivisionHeader  configurationSection? inputOutputSection?;

dataDivision:
	DataDivisionHeader fileSection? workingStorageSection? localStorageSection? linkageSection?;

procedureDivision:
	ProcedureDivisionHeader declaratives? section*;

// p85 : An end program marker separates each program in the sequence of programs. 
//       program-name must be identical to a program-name declared in a preceding program-ID paragraph.

// p85 : An end program marker is optional for the last program in the sequence only if that program does not contain any nested source programs.

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

// p102 : 

//nestedSourceProgram :
//                       programIdentification
//                       (environmentDivisionHeader environmentDivisionContent)?
//                       (dataDivisionHeader dataDivisionContent)?
//                       (procedureDivisionHeader procedureDivisionContent?)?
//                       (nestedSourceProgram)*  
//                       programEnd
//                   ;

// --- COBOL CLASS ---

// p89 : Enterprise COBOL provides object-oriented syntax to facilitate interoperation of 
//       COBOL and Java programs.
//       You can use object-oriented syntax to:
//       - Define classes, with methods and data implemented in COBOL
//       - Create instances of Java or COBOL classes
//       - Invoke methods on Java or COBOL objects
//       - Write classes that inherit from Java classes or from other COBOL classes
//       - Define and invoke overloaded methods

// p91 : With the exception of the COPY and REPLACE statements and the END CLASS
//       marker, the statements, entries, paragraphs, and sections of a COBOL class
//       definition are grouped into the following structure:
// - IDENTIFICATION DIVISION
// - ENVIRONMENT DIVISION (configuration section only)
// - Factory definition
//   . IDENTIFICATION DIVISION
//   . DATA DIVISION
//   . PROCEDURE DIVISION (containing one or more method definitions)
// - Object definition
//   . IDENTIFICATION DIVISION
//   . DATA DIVISION
//   . PROCEDURE DIVISION (containing one or more method definitions)

// p91 : The end of a COBOL class definition is indicated by the END CLASS marker.

// p97 : Class IDENTIFICATION DIVISION
// For a class, the first paragraph of the IDENTIFICATION DIVISION must
// be the CLASS-ID paragraph. The other paragraphs are optional and can
// appear in any order.

// p103 : The CLASS-ID paragraph specifies the name by which the class is known and
// assigns selected attributes to that class. The CLASS-ID paragraph is required and
// must be the first paragraph in a class IDENTIFICATION DIVISION.

cobolClass:
              ClassIdentification
              EnvironmentDivisionHeader
                  configurationSection?
             (FactoryIdentification
                 (DataDivisionHeader 
                      workingStorageSection?
                 )?
                 (ProcedureDivisionHeader 
                      methodDefinition*
                 )?
              FactoryEnd)?
             (ObjectIdentification
                 (DataDivisionHeader 
                      workingStorageSection?
                 )?
                 (ProcedureDivisionHeader 
                      methodDefinition*
                 )?             
              ObjectEnd)?
              ClassEnd?;

// p93 : A COBOL method definition describes a method. 
//       You can specify method definitions only within the factory paragraph and the object paragraph of a class definition.

// p93 : With the exception of COPY and REPLACE statements and the END METHOD
//       marker, the statements, entries, paragraphs, and sections of a COBOL method
//       definition are grouped into the following four divisions:
// - IDENTIFICATION DIVISION
// - ENVIRONMENT DIVISION (input-output section only)
// - DATA DIVISION
// - PROCEDURE DIVISION

// p93 : The end of a COBOL method definition is indicated by the END METHOD marker.

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

// p93 : Format: method definition 

methodDefinition : 
                     MethodIdentification
                    (EnvironmentDivisionHeader
                         inputOutputSection?
                    )?
                    (DataDivisionHeader 
                         fileSection?
                         workingStorageSection?
                         localStorageSection?
                         linkageSection?
                    )?
                    (ProcedureDivisionHeader 
                         declaratives?
                         section*
                    )?
                     MethodEnd;

// --- ENVIRONMENT DIVISION ---

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

// p109 : CONFIGURATION SECTION Format:

configurationSection : 
                     ConfigurationSectionHeader
                   ( SourceComputerParagraph
                   | ObjectComputerParagraph
                   | SpecialNamesParagraph
                   | RepositoryParagraph )*;

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

// p125: Format: input-output section

inputOutputSection: 
                  InputOutputSectionHeader
                  fileControlParagraph?
                  ioControlParagraph?;

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

// p126: The FILE-CONTROL paragraph begins with the word FILE-CONTROL followed
// by a separator period. It must contain one and only one entry for each file
// described in an FD or SD entry in the DATA DIVISION.

fileControlParagraph :
                         FileControlParagraphHeader 
                         FileControlEntry*;

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

ioControlParagraph : 
                       IOControlParagraphHeader
                       (IOControlEntry+ SentenceEnd)?;

// --- DATA DIVISION ---

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

fileSection :
                FileSectionHeader 
                (FileDescriptionEntry 
                 DataDescriptionEntry+)*;

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

workingStorageSection :
                          WorkingStorageSectionHeader 
                          (DataDescriptionEntry | ExecStatement SentenceEnd?)*;

// p156: The LOCAL-STORAGE SECTION defines storage that is allocated and freed on a per-invocation basis.
// On each invocation, data items defined in the LOCAL-STORAGE SECTION are reallocated. Each data item that has a VALUE clause is initialized to the value specified in that clause.

// ... more details on LOCAL-STORAGE p156 ...

// p156: Data items defined in the LOCAL-STORAGE SECTION cannot specify the EXTERNAL clause.
// The LOCAL-STORAGE SECTION must begin with the header LOCAL-STORAGE SECTION, followed by a separator period.
// You can specify the LOCAL-STORAGE SECTION in recursive programs, in nonrecursive programs, and in methods.
// Method LOCAL-STORAGE content is the same as program LOCAL-STORAGE content except that the GLOBAL clause has no effect (because methods cannot be nested).

localStorageSection:
                        LocalStorageSectionHeader 
                        (DataDescriptionEntry | ExecStatement SentenceEnd?)*;

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

linkageSection:
                   LinkageSectionHeader 
                   DataDescriptionEntry*;

// --- PROCEDURE DIVISION ---

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

declaratives:
                DeclarativesHeader
                   (SectionHeader UseStatement
                        paragraph* 
                   )+
                DeclarativesEnd;

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

section:
	  ((SectionHeader | ParagraphHeader) paragraph*)
	| sentence+;

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

paragraph:
            (ParagraphHeader | sentence)
            sentence*;

// Sentence
// One or more statements terminated by a separator period.

sentence:
	(statement* SentenceEnd) | ExecStatement;

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
// The following lists contain the COBOL imperative statements.
// Arithmetic
// - ADD 1
// - COMPUTE 1
// - DIVIDE 1
// - MULTIPLY 1
// - SUBTRACT1
//   1. Without the ON SIZE ERROR or the NOT ON SIZE ERROR phrase.
// Data movement
// - ACCEPT (DATE, DAY, DAY-OF-WEEK, TIME)
// - INITIALIZE
// - INSPECT
// - MOVE
// - SET
// - STRING 2
// - UNSTRING 2
// - XML GENERATE 8
// - XML PARSE 8
//   2. Without the ON OVERFLOW or the NOT ON OVERFLOW phrase.
//   8. Without the ON EXCEPTION or NOT ON EXCEPTION phrase.
// Ending
// - STOP RUN
// - EXIT PROGRAM
// - EXIT METHOD
// - GOBACK
// Input-output
// - ACCEPT identifier
// - CLOSE
// - DELETE 3
// - DISPLAY
// - OPEN
// - READ 4
// - REWRITE 3
// - START 3
// - STOP literal
// - WRITE 5
//   3. Without the INVALID KEY or the NOT INVALID KEY phrase.
//   4. Without the AT END or NOT AT END, and INVALID KEY or NOT INVALID KEY phrases.
//   5. Without the INVALID KEY or NOT INVALID KEY, and END-OF-PAGE or NOT END-OF-PAGE phrases.
// Ordering
// - MERGE
// - RELEASE
// - RETURN 6
// - SORT
//   6. Without the AT END or NOT AT END phrase.
// Procedure-branching
// - ALTER
// - EXIT
// - GO TO
// - PERFORM
// Program or method linkage
// - CALL 7
// - CANCEL
// - INVOKE
//   7. Without the ON OVERFLOW phrase, and without the ON EXCEPTION or NOT ON EXCEPTION phrase.
// Table-handling
// - SET

// p278: Conditional statements
// A conditional statement specifies that the truth value of a condition is to be
// determined and that the subsequent action of the object program is dependent on
// this truth value.
// For more information about conditional expressions, see “Conditional expressions”
// on page 256.)
// The following lists contain COBOL statements that become conditional when a
// condition (for example, ON SIZE ERROR or ON OVERFLOW) is included and
// when the statement is not terminated by its explicit scope terminator.
// Arithmetic
// - ADD ... ON SIZE ERROR
// - ADD ... NOT ON SIZE ERROR
// - COMPUTE ... ON SIZE ERROR
// - COMPUTE ... NOT ON SIZE ERROR
// - DIVIDE ... ON SIZE ERROR
// - DIVIDE ... NOT ON SIZE ERROR
// - MULTIPLY ... ON SIZE ERROR
// - MULTIPLY ... NOT ON SIZE ERROR
// - SUBTRACT ... ON SIZE ERROR
// - SUBTRACT ... NOT ON SIZE ERROR
// Data movement
// - STRING ... ON OVERFLOW
// - STRING ... NOT ON OVERFLOW
// - UNSTRING ... ON OVERFLOW
// - UNSTRING ... NOT ON OVERFLOW
// - XML GENERATE ... ON EXCEPTION
// - XML GENERATE ... NOT ON EXCEPTION
// - XML PARSE ... ON EXCEPTION
// - XML PARSE ... NOT ON EXCEPTION
// Decision
// - IF
// - EVALUATE
// Input-output
// - DELETE ... INVALID KEY
// - DELETE ... NOT INVALID KEY
// - READ ... AT END
// - READ ... NOT AT END
// - READ ... INVALID KEY
// - READ ... NOT INVALID KEY
// - REWRITE ... INVALID KEY
// - REWRITE ... NOT INVALID KEY
// - START ... INVALID KEY
// - START ... NOT INVALID KEY
// - WRITE ... AT END-OF-PAGE
// - WRITE ... NOT AT END-OF-PAGE
// - WRITE ... INVALID KEY
// - WRITE ... NOT INVALID KEY
// Ordering
// - RETURN ... AT END
// - RETURN ... NOT AT END
// Program or method linkage
// - CALL ... ON OVERFLOW
// - CALL ... ON EXCEPTION
// - CALL ... NOT ON EXCEPTION
// - INVOKE ... ON EXCEPTION
// - INVOKE ... NOT ON EXCEPTION
// Table-handling
// - SEARCH

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
// These are the explicit scope terminators:
// - END-ADD
// - END-CALL
// - END-COMPUTE
// - END-DELETE
// - END-DIVIDE
// - END-EVALUATE
// - END-IF
// - END-INVOKE
// - END-MULTIPLY
// - END-PERFORM
// - END-READ
// - END-RETURN
// - END-REWRITE
// - END-SEARCH
// - END-START
// - END-STRING
// - END-SUBTRACT
// - END-UNSTRING
// - END-WRITE
// - END-XML
// Implicit scope terminators
// At the end of any sentence, an implicit scope terminator is a separator period that
// terminates the scope of all previous statements not yet terminated.
// An unterminated conditional statement cannot be contained by another statement.
// Except for nesting conditional statements within IF statements, nested statements
// must be imperative statements and must follow the rules for imperative
// statements. You should not nest conditional statements.

statement:
// (see the documentation for these statements in CobolCodeElements.g4)
             
// Statements without optional body
	( ContinueStatement
        | EntryStatement
// -- arithmetic --
	| AddStatement
	| ComputeStatement
	| DivideStatement
	| MultiplyStatement
	| SubtractStatement
// -- data movement --
	| AcceptStatement // (DATE, DAY, DAY-OF-WEEK, TIME)
	| InitializeStatement
	| InspectStatement
	| MoveStatement
	| SetStatement // "table-handling" too
	| StringStatement
	| UnstringStatement
	| XmlGenerateStatement
	| XmlParseStatement
// -- ending --
	| StopStatement // RUN
	| ExitMethodStatement
	| ExitProgramStatement
	| GobackStatement
// -- input-output --
//	| AcceptStatement // identifier
	| CloseStatement
	| DeleteStatement
	| DisplayStatement
	| OpenStatement
	| ReadStatement
	| RewriteStatement
	| StartStatement
//	StopStatement // literal
	| WriteStatement
// -- ordering --
	| MergeStatement
	| ReleaseStatement
	| ReturnStatement
	| SortStatement
// -- procedure-branching --
	| AlterStatement
	| ExitStatement
	| GotoStatement
	| PerformProcedureStatement
// -- program or method linkage --
	| CallStatement
	| CancelStatement
	| InvokeStatement
// -- DB2 & CICS integration --
	| ExecStatement)
      
// Statements with optional body      
      	| evaluateStatementWithBody
	| ifStatementWithBody
	| searchStatementWithBody
// -- arithmetic --
	| addStatementConditional
	| computeStatementConditional
	| divideStatementConditional
	| multiplyStatementConditional
	| subtractStatementConditional
// -- data movement --
	| stringStatementConditional
	| unstringStatementConditional
	| xmlGenerateStatementConditional
	| xmlParseStatementConditional
// -- input-output --
	| deleteStatementConditional
	| readStatementConditional
	| rewriteStatementConditional
	| startStatementConditional
	| writeStatementConditional
// -- ordering --
	| returnStatementConditional
// -- procedure-branching --
	| performStatementWithBody
// -- program or method linkage --
	| callStatementConditional
	| invokeStatementConditional
	;

// Statements with optional body  

addStatementConditional:			
                        AddStatement 
                            sizeErrorConditions? 
                        AddStatementEnd?;

callStatementConditional:			
                        CallStatement 
                            (exceptionConditions | (OnOverflowCondition statement+))? 
                        CallStatementEnd?;

computeStatementConditional:		
                        ComputeStatement 
                            sizeErrorConditions? 
                        ComputeStatementEnd?;

deleteStatementConditional:			
                        DeleteStatement 
                            invalidKeyConditions? 
                        DeleteStatementEnd?;

divideStatementConditional:			
                        DivideStatement 
                            sizeErrorConditions? 
                        DivideStatementEnd?;

evaluateStatementWithBody:                             
                        EvaluateStatement
                            ((WhenConditionalExpression | WhenEvaluateCondition)+ statement+)+
                            (WhenOtherCondition statement+)?
                        EvaluateStatementEnd?;

ifStatementWithBody:
                        IfStatement 
                            (statement+ | NextSentenceStatement)
                        (ElseCondition 
                            (statement+ | NextSentenceStatement) )?
                        IfStatementEnd?;

invokeStatementConditional:			
                        InvokeStatement 
                            exceptionConditions? 
                        InvokeStatementEnd?;

multiplyStatementConditional:		
                        MultiplyStatement
                            sizeErrorConditions? 
                        MultiplyStatementEnd?;

performStatementWithBody:			
                        PerformStatement 
                            statement* 
                        PerformStatementEnd?;

readStatementConditional:			
                        ReadStatement 
                            ((atEndConditions? invalidKeyConditions?) | 
                             (invalidKeyConditions? atEndConditions?))
                        ReadStatementEnd?;

returnStatementConditional:			
                        ReturnStatement 
                            atEndConditions? 
                        ReturnStatementEnd?;

rewriteStatementConditional:		
                        RewriteStatement 
                            invalidKeyConditions? 
                        RewriteStatementEnd?;

searchStatementWithBody:
                        SearchStatement 
                            (AtEndCondition statement+)?
                            (WhenConditionalExpression (statement+ | NextSentenceStatement))+
                        SearchStatementEnd?;

startStatementConditional:			
                        StartStatement 
                            invalidKeyConditions? 
                        StartStatementEnd?;

stringStatementConditional:			
                        StringStatement 
                            overflowConditions?
                        StringStatementEnd?;

subtractStatementConditional:		
                        SubtractStatement 
                            sizeErrorConditions?
                        SubtractStatementEnd?;

unstringStatementConditional:		
                        UnstringStatement 
                            overflowConditions? 
                        UnstringStatementEnd?;

writeStatementConditional:			
                        WriteStatement 
                            ((atEndConditions? invalidKeyConditions?) | 
                             (invalidKeyConditions? atEndConditions?) )
                        WriteStatementEnd?;

xmlGenerateStatementConditional:	
                        XmlGenerateStatement 
                            exceptionConditions?
                        XmlStatementEnd?;

xmlParseStatementConditional:		
                        XmlParseStatement
                            exceptionConditions?
                        XmlStatementEnd?;

// Conditional execution of statements

atEndConditions:
	(AtEndCondition statement+) |
	(NotAtEndCondition statement+) |
	((AtEndCondition statement+) (NotAtEndCondition statement+));

exceptionConditions:
	(OnExceptionCondition statement+) |
	(NotOnExceptionCondition statement+) |
	((OnExceptionCondition statement+) (NotOnExceptionCondition statement+));

invalidKeyConditions:
	(InvalidKeyCondition statement+) |
	(NotInvalidKeyCondition statement+) |
	((InvalidKeyCondition statement+) (NotInvalidKeyCondition statement+));

overflowConditions:
	(OnOverflowCondition statement+) |
	(NotOnOverflowCondition statement+) |
	((OnOverflowCondition statement+) (NotOnOverflowCondition statement+));

sizeErrorConditions:
	(OnSizeErrorCondition statement+) |
	(NotOnSizeErrorCondition statement+) |
	((OnSizeErrorCondition statement+) (NotOnSizeErrorCondition statement+));