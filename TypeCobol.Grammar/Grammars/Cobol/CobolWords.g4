// IBM Enterprise Cobol 5.1 for zOS

// ------------------------------------------------------------------------
// List of all Cobol Token types and Token families produced by the Scanner
// ------------------------------------------------------------------------

grammar CobolWords;

// --- Cobol Token types ---

// The token types are recognized by the Scanner before the parsing step.
// All token types names start with a capital letter.
// => see the complete documentation of the Cobol text format and the tokenizing process 
//    in the comments below, at the end of this file.

// The grammar rules defined in the Parser match sequences of theses token types.
// All grammar rule names start with a lower case letter.

// IMPORTANT : HOW TO INSERT, REMOVE, or REORDER a TOKEN TYPE in the list of tokens below ?

// 1. The token types list below is GENERATED from the following Excel table :
// -> TypeCobol.Grammar/Grammars/Cobol/TokenTypes.xlsx
// This Excel table should be updated first, and then be used to generate a new
// version of the list.

// 2. The ORDERED list of token types in this grammar file MUST ALWAYS 
// STAY IN SYNC with the TokenType and TokenFamily enumerations in the C# file :
// -> TypeCobol/Compiler/Scanner/TokenType.cs 
// You should also use the Excel table to generate the TokenType enumeration.

// 3. Each time you update the TokenType or TokenFamily enumerations, 
// you MUST ALSO update another C# file :
// -> TypeCobol/Compiler/Scanner/TokenUtils.cs 
// . update the total number of token types
// . map new token types to token families
// . register the character strings corresponding to each new token type (for keywords only)

// 4. Each time you	add a token type, you must update the token families defined below in this grammar file : 
// -> figurativeConstant, externalName1, literalOrUserDefinedWordOReservedWordExceptCopy

tokens 
{ 
    // Separators - Whitespace
    SpaceSeparator,
    CommaSeparator,
    SemicolonSeparator,
    // Comments
    FloatingComment,
    CommentLine,
    // Separators - Syntax
    PeriodSeparator,
    ColonSeparator,
    QualifiedNameSeparator,
    LeftParenthesisSeparator,
    RightParenthesisSeparator,
    PseudoTextDelimiter,
    // Special character word - Arithmetic operators
    PlusOperator,
    MinusOperator,
    DivideOperator,
    MultiplyOperator,
    PowerOperator,
    // Special character word - Relational operators
    LessThanOperator,
    GreaterThanOperator,
    LessThanOrEqualOperator,
    GreaterThanOrEqualOperator,
    EqualOperator,
    // Literals - Alphanumeric
    AlphanumericLiteral,
    HexadecimalAlphanumericLiteral,
    NullTerminatedAlphanumericLiteral,
    NationalLiteral,
    HexadecimalNationalLiteral,
    DBCSLiteral,
    // Literals - Numeric
    LevelNumber,
    IntegerLiteral,
    DecimalLiteral,
    FloatingPointLiteral,
    // Literals - Syntax tokens
    PictureCharacterString,
    CommentEntry,
    ExecStatementText,
    // Symbols
    SectionParagraphName,
    IntrinsicFunctionName,
    ExecTranslatorName,
    PartialCobolWord,
    UserDefinedWord,
    // Keywords - Compiler directive starting tokens
    ASTERISK_CBL,
    ASTERISK_CONTROL,
    BASIS,
    CBL,
    COPY,
    DELETE_CD,
    EJECT,
    ENTER,
    EXEC_SQL_INCLUDE,
    INSERT,
    PROCESS,
    READY,
    RESET,
    REPLACE,
    SERVICE_CD,
    SKIP1,
    SKIP2,
    SKIP3,
    TITLE,
    // Keywords - Code element starting tokens
    ACCEPT,
    ADD,
    ALTER,
    APPLY,
    CALL,
    CANCEL,
    CLOSE,
    COMPUTE,
    CONFIGURATION,
    CONTINUE,
    DATA,
    DECLARATIVES,
    DECLARE,
    DELETE,
    DISPLAY,
    DIVIDE,
    ELSE,
    END,
    END_ADD,
    END_CALL,
    END_COMPUTE,
    END_DECLARE,
    END_DELETE,
    END_DIVIDE,
    END_EVALUATE,
    END_EXEC,
    END_IF,
    END_INVOKE,
    END_MULTIPLY,
    END_PERFORM,
    END_READ,
    END_RETURN,
    END_REWRITE,
    END_SEARCH,
    END_START,
    END_STRING,
    END_SUBTRACT,
    END_UNSTRING,
    END_WRITE,
    END_XML,
    ENTRY,
    ENVIRONMENT,
    EVALUATE,
    EXEC,
    EXECUTE,
    EXIT,
    FD,
    FILE,
    FILE_CONTROL,
    GO,
    GOBACK,
    I_O_CONTROL,
    ID,
    IDENTIFICATION,
    IF,
    INITIALIZE,
    INPUT_OUTPUT,
    INSPECT,
    INVOKE,
    LINKAGE,
    LOCAL_STORAGE,
    MERGE,
    MOVE,
    MULTIPLE,
    MULTIPLY,
    NEXT,
    OBJECT_COMPUTER,
    OPEN,
    PERFORM,
    PROCEDURE,
    READ,
    RELEASE,
    REPOSITORY,
    RERUN,
    RETURN,
    REWRITE,
    SAME,
    SD,
    SEARCH,
    SELECT,
    SERVICE,
    SET,
    SORT,
    SOURCE_COMPUTER,
    SPECIAL_NAMES,
    START,
    STOP,
    STRING,
    SUBTRACT,
    UNSTRING,
    USE,
    WHEN,
    WORKING_STORAGE,
    WRITE,
    XML,
    // Keywords - Special registers
    ADDRESS,
    DEBUG_CONTENTS,
    DEBUG_ITEM,
    DEBUG_LINE,
    DEBUG_NAME,
    DEBUG_SUB_1,
    DEBUG_SUB_2,
    DEBUG_SUB_3,
    JNIENVPTR,
    LENGTH,
    LINAGE_COUNTER,
    RETURN_CODE,
    SHIFT_IN,
    SHIFT_OUT,
    SORT_CONTROL,
    SORT_CORE_SIZE,
    SORT_FILE_SIZE,
    SORT_MESSAGE,
    SORT_MODE_SIZE,
    SORT_RETURN,
    TALLY,
    WHEN_COMPILED,
    XML_CODE,
    XML_EVENT,
    XML_INFORMATION,
    XML_NAMESPACE,
    XML_NAMESPACE_PREFIX,
    XML_NNAMESPACE,
    XML_NNAMESPACE_PREFIX,
    XML_NTEXT,
    XML_TEXT,
    // Keywords - Figurative constants
    HIGH_VALUE,
    HIGH_VALUES,
    LOW_VALUE,
    LOW_VALUES,
    NULL,
    NULLS,
    QUOTE,
    QUOTES,
    SPACE,
    SPACES,
    ZERO,
    ZEROES,
    ZEROS,
    SymbolicCharacter,
    // Keywords - Special object identifiers
    SELF,
    SUPER,
    // Keywords - Syntax tokens
    ACCESS,
    ADVANCING,
    AFTER,
    ALL,
    ALPHABET,
    ALPHABETIC,
    ALPHABETIC_LOWER,
    ALPHABETIC_UPPER,
    ALPHANUMERIC,
    ALPHANUMERIC_EDITED,
    ALSO,
    ALTERNATE,
    AND,
    ANY,
    ARE,
    AREA,
    AREAS,
    ASCENDING,
    ASSIGN,
    AT,
    ATTRIBUTE,
    ATTRIBUTES,
    AUTHOR,
    BEFORE,
    BEGINNING,
    BINARY,
    BLANK,
    BLOCK,
    BOTTOM,
    BY,
    CHARACTER,
    CHARACTERS,
    CLASS,
    CLASS_ID,
    COBOL,
    CODE,
    CODE_SET,
    COLLATING,
    COM_REG,
    COMMA,
    COMMON,
    COMP,
    COMP_1,
    COMP_2,
    COMP_3,
    COMP_4,
    COMP_5,
    COMPUTATIONAL,
    COMPUTATIONAL_1,
    COMPUTATIONAL_2,
    COMPUTATIONAL_3,
    COMPUTATIONAL_4,
    COMPUTATIONAL_5,
    CONTAINS,
    CONTENT,
    CONVERTING,
    CORR,
    CORRESPONDING,
    COUNT,
    CURRENCY,
    DATE,
    DATE_COMPILED,
    DATE_WRITTEN,
    DAY,
    DAY_OF_WEEK,
    DBCS,
    DEBUGGING,
    DECIMAL_POINT,
    DELIMITED,
    DELIMITER,
    DEPENDING,
    DESCENDING,
    DISPLAY_1,
    DIVISION,
    DOWN,
    DUPLICATES,
    DYNAMIC,
    EBCDIC,
    EGCS,
    ELEMENT,
    ENCODING,
    END_OF_PAGE,
    ENDING,
    EOP,
    EQUAL,
    ERROR,
    EVERY,
    EXCEPTION,
    EXTEND,
    EXTERNAL,
    FACTORY,
    FALSE,
    FILLER,
    FIRST,
    FOOTING,
    FOR,
    FROM,
    FUNCTION,
    FUNCTION_POINTER,
    GENERATE,
    GIVING,
    GLOBAL,
    GREATER,
    GROUP_USAGE,
    I_O,
    IN,
    INDEX,
    INDEXED,
    INHERITS,
    INITIAL,
    INPUT,
    INSTALLATION,
    INTO,
    INVALID,
    IS,
    JUST,
    JUSTIFIED,
    KANJI,
    KEY,
    LABEL,
    LEADING,
    LEFT,
    LESS,
    LINAGE,
    LINE,
    LINES,
    LOCK,
    MEMORY,
    METHOD,
    METHOD_ID,
    MODE,
    MODULES,
    MORE_LABELS,
    NAME,
    NAMESPACE,
    NAMESPACE_PREFIX,
    NATIONAL,
    NATIONAL_EDITED,
    NATIVE,
    NEGATIVE,
    NEW,
    NO,
    NONNUMERIC,
    NOT,
    NUMERIC,
    NUMERIC_EDITED,
    OBJECT,
    OCCURS,
    OF,
    OFF,
    OMITTED,
    ON,
    OPTIONAL,
    OR,
    ORDER,
    ORGANIZATION,
    OTHER,
    OUTPUT,
    OVERFLOW,
    OVERRIDE,
    PACKED_DECIMAL,
    PADDING,
    PAGE,
    PARSE,
    PASSWORD,
    PIC,
    PICTURE,
    POINTER,
    POSITION,
    POSITIVE,
    PROCEDURE_POINTER,
    PROCEDURES,
    PROCEED,
    PROCESSING,
    PROGRAM,
    PROGRAM_ID,
    RANDOM,
    RECORD,
    RECORDING,
    RECORDS,
    RECURSIVE,
    REDEFINES,
    REEL,
    REFERENCE,
    REFERENCES,
    RELATIVE,
    RELOAD,
    REMAINDER,
    REMOVAL,
    RENAMES,
    REPLACING,
    RESERVE,
    RETURNING,
    REVERSED,
    REWIND,
    RIGHT,
    ROUNDED,
    RUN,
    SECTION,
    SECURITY,
    SEGMENT_LIMIT,
    SENTENCE,
    SEPARATE,
    SEQUENCE,
    SEQUENTIAL,
    SIGN,
    SIZE,
    SORT_MERGE,
    SQL,
    SQLIMS,
    STANDARD,
    STANDARD_1,
    STANDARD_2,
    STATUS,
    SUPPRESS,
    SYMBOL,
    SYMBOLIC,
    SYNC,
    SYNCHRONIZED,
    TALLYING,
    TAPE,
    TEST,
    THAN,
    THEN,
    THROUGH,
    THRU,
    TIME,
    TIMES,
    TO,
    TOP,
    TRACE,
    TRAILING,
    TRUE,
    TYPE,
    UNBOUNDED,
    UNIT,
    UNTIL,
    UP,
    UPON,
    USAGE,
    USING,
    VALIDATING,
    VALUE,
    VALUES,
    VARYING,
    WITH,
    WORDS,
    WRITE_ONLY,
    XML_DECLARATION,
    XML_SCHEMA,
    YYYYDDD,
    YYYYMMDD,
    // Keywords - Cobol 2002
    TYPEDEF,
    STRONG,
    // Keywords - TypeCobol
    UNSAFE,
    PUBLIC,
    PRIVATE,
    IN_OUT,
	STRICT,
	QuestionMark
}


// --- Cobol Token families ---

// - 1. Numeric literals -

// p37: In this documentation, the word integer appearing in a format represents a numeric literal of nonzero value
// that contains no sign and no decimal point, except when other rules are included
// with the description of the format.

// integer: IntegerLiteral;

// p38: A numeric literal is a character-string whose characters are selected from the digits 0 through 9, a sign character (+ or -), and the decimal point.
// Numeric literals can be fixed-point or floating-point numbers.

numericLiteralToken: (IntegerLiteral | DecimalLiteral | FloatingPointLiteral);

// p13: Figurative constants are reserved words that name and refer to specific constant values.
// p15: When a numeric literal appears in a syntax diagram, only the figurative constant ZERO (or ZEROS or ZEROES) can be used. 

numericFigurativeConstant: (ZERO | ZEROS | ZEROES);

// ** Syntax rules for NULL/NULLS **
// -- Definition
// p15: NULL, NULLS Represents a value used to indicate that data items defined with USAGE POINTER, USAGE PROCEDURE-POINTER, USAGE FUNCTIONPOINTER, USAGE OBJECT REFERENCE, 
// or the ADDRESS OF special register do not contain a valid address. NULL can be used only where explicitly allowed in the syntax formats. NULL has the value zero.
// -- Type of NULL figurative constant
// p162: Figurative constants (except NULL) have a class and category that depends on the literal or value represented by the figurative constant in the context of its use. 
// p371: Alphanumeric: includes the following items: – Data items of category alphanumeric – Alphanumeric functions – Alphanumeric literals – The figurative constant ALL alphanumeric-literal and all other figurative constants (except NULL) when used in a context that requires an alphanumeric sending item 
// -- Allowed usages in data description VALUE clause
// p233: A VALUE clause for a function-pointer data item can contain only NULL or NULLS.
// p235: A VALUE clause for an object-reference data item can contain only NULL or NULLS.
// p235: A VALUE clause for a pointer data item can contain only NULL or NULLS.
// p236: A VALUE clause for a procedure-pointer data item can contain only NULL or NULLS.
// p242: VALUE clause - Format 3: NULL value 
// p242: VALUE IS NULL can be specified only for elementary items described implicitly or explicitly as USAGE POINTER, USAGE PROCEDURE-POINTER, USAGE FUNCTION-POINTER, or USAGE OBJECT REFERENCE.
// -- Allowed usages in logical relation conditions
// p268: Format 2: data-pointer relation condition
// p268: The following table summarizes the permissible comparisons for USAGE POINTER, NULL, and ADDRESS OF. 
// p269: Format 3: procedure-pointer and function-pointer relation condition
// p269: Format 4: object-reference relation condition
// -- Allowed usages in statements
// p418: Format 5: SET statement for data-pointers
// p419: NULL, NULLS Sending field. Sets the receiving field to contain the value of an invalid address.
// p419: The following table shows valid combinations of sending and receiving fields in a format-5 SET statement.
// p419: Format 6: SET statement for procedure-pointers and function-pointers
// p421: Format 7: SET statement for object references
// p421: If the figurative constant NULL is specified, the receiving object-reference-id-1 is set to the NULL value.
// -- Forbidden usages
// p306: CALL BY REFERENCE phrase - literal-2 Can be: A figurative constant (except ALL literal or NULL/NULLS)
// p371: MOVE Statement - Alphanumeric: includes the following items: The figurative constant ALL alphanumeric-literal and all other figurative constants (except NULL)
// p434: STRING statement - literal-1 or literal-2 can be any figurative constant that does not begin with the word ALL (except NULL).
// p441: UNSTRING statement - Any figurative constant can be specified except NULL or one that begins with the word ALL. 
// 
// ==> NULL/NULLS tokens are referenced explicitely via nullFigurativeConstant in all CodeElements rules where they are allowed, we don't need to include them in the generic figurativeConstant rule

nullFigurativeConstant: (NULL | NULLS);

// - 2. Alphanumeric literals -

// p33: A literal is a character-string whose value is specified either by the characters of
// which it is composed or by the use of a figurative constant.
// p34: The formats of alphanumeric literals are:
// - Format 1: “Basic alphanumeric literals ?
// - Format 2: “Alphanumeric literals with DBCS characters ? on page 35
// - Format 3: “Hexadecimal notation for alphanumeric literals ? on page 36
// - Format 4: “Null-terminated alphanumeric literals ? on page 37

alphanumericLiteralToken: (AlphanumericLiteral | HexadecimalAlphanumericLiteral | NullTerminatedAlphanumericLiteral);


alphanumericOrNationalLiteralToken: (AlphanumericLiteral | HexadecimalAlphanumericLiteral | NullTerminatedAlphanumericLiteral |
                                     DBCSLiteral | NationalLiteral | HexadecimalNationalLiteral);

notNullTerminatedAlphanumericOrNationalLiteralToken: (AlphanumericLiteral | HexadecimalAlphanumericLiteral |
                                                      DBCSLiteral | NationalLiteral | HexadecimalNationalLiteral);

// p13: Figurative constants are reserved words that name and refer to specific constant values.
// p15: The singular and plural forms of NULL, ZERO, SPACE, HIGH-VALUE, LOW-VALUE, and QUOTE can be used interchangeably.
// When the rules of COBOL permit any one spelling of a figurative constant name, any alternative spelling of that figurative constant name can be specified.
// symbolic-character Represents one or more of the characters specified as a value of the symbolic-character in the SYMBOLIC CHARACTERS clause of the SPECIAL-NAMES paragraph.
// You can use a figurative constant wherever literal appears in a syntax diagram, except where explicitly prohibited.
// Figurative constants are not allowed as function arguments except in an arithmetic expression, where the expression is an argument to a function.
// The length of a figurative constant depends on the context of its use. 
// The following rules apply:
// - When a figurative constant is specified in a VALUE clause or associated with a data item (for example, when it is moved to or compared with another item), 
//   the length of the figurative constant character-string is equal to 1 or the number of character positions in the associated data item, whichever is greater.
// - When a figurative constant, other than the ALL literal, is not associated with another data item (for example, in a CALL, INVOKE, STOP, STRING, or UNSTRING statement), 
//   the length of the character-string is one character.

figurativeConstant: (HIGH_VALUE | HIGH_VALUES |
                     LOW_VALUE  | LOW_VALUES |
                     QUOTE | QUOTES |
                     SPACE | SPACES |
                     ZERO  | ZEROS  | ZEROES) |
                     symbolicCharacterReference;

figurativeConstantForXMLGenerate:
	HIGH_VALUE | HIGH_VALUES |
	LOW_VALUE  | LOW_VALUES |
	SPACE | SPACES |
	ZERO  | ZEROS  | ZEROES;

// ** Syntax rules for ALL literal **
// -- Definition
// p15: ALL literal - literal can be an alphanumeric literal, a DBCS literal, a national literal, or a figurative constant other than the ALL literal.
// When literal is not a figurative constant, ALL literal represents one or more occurrences of the string of characters that compose the literal. 
// When literal is a figurative constant, the word ALL has no meaning and is used only for readability. 
// p37: null-terminated literals are not supported in ALL literal figurative constants
// -- Allowed usages in data description VALUE clause
// p207: Alphanumeric items / Alphanumeric-edited items - USAGE DISPLAY - associated VALUE clause must specify an alphanumeric literal or one of the following figurative constants: ALL alphanumeric-literal
// p207: DBCS items - associated VALUE clause must contain a DBCS literal, the figurative constant SPACE, or the figurative constant ALL DBCS-literal.  
// p208: National items / National-edited items - associated VALUE clause must specify an alphanumeric literal, a national literal, or one of the following figurative constants: ALL alphanumeric-literal v ALL national-literal 
// p239: If the VALUE clause is specified at the group level for a national group, the literal can be an alphanumeric literal, a national literal, or one of the figurative constants ZERO, SPACE, QUOTES, HIGH-VALUE, LOW-VALUE, symbolic character, ALL national-literal, or ALL -literal. 
// p239: A VALUE clause associated with a DBCS item must contain a DBCS literal, the figurative constant SPACE, or the figurative constant ALL DBCS-literal.
// p240: Format 2: condition-name value - If the associated conditional variable is of class DBCS, literal-1 and literal-2 must be DBCS literals. The figurative constant SPACE or the figurative constant ALL DBCS-literal can be specified. 
// p240: Format 2: condition-name value - If the associated conditional variable is of class national, literal-1 and literal-2 must be either both national literals or both alphanumeric literals for a given condition-name. The figurative constants ZERO, SPACE, QUOTE, HIGH-VALUE, LOW-VALUE, symbolic-character, ALL national-literal, or ALL literal can be specified. 
// -- Allowed usages in logical relation conditions
// p263: Format 1: general relation condition - Comparisons involving figurative constant - ALL alphanumeric literal / ALL national literal / ALL DBCS literal
// -- Allowed usages in statements
// p371: MOVE Statement - Alphanumeric: includes the following items: The figurative constant ALL alphanumeric-literal and all other figurative constants (except NULL)
// p371: MOVE Statement - DBCS: includes data items of category DBCS, DBCS literals, and the figurative constant ALL DBCS-literal
// p371: MOVE Statement - National: includes the following items: Figurative constants ZERO, SPACE, QUOTE, and ALL national-literal when used in a context that requires a national sending item 
// p373: If the receiving item is numeric and the sending field is an alphanumeric literal, a national literal, or an ALL literal, all characters of the literal must be numeric characters. 
// -- Forbidden usages
// p238: If the VALUE clause is specified at the group level for an alphanumeric group, the literal must be an alphanumeric literal or a figurative constant as specified in “Figurative constants ? on page 13, other than ALL national-literal. 
// p15: The figurative constant ALL literal must not be used with the CALL, INSPECT, INVOKE, STOP, or STRING statements. 
// p306: CALL BY REFERENCE phrase - literal-2 Can be: A figurative constant (except ALL literal or NULL/NULLS)
// p306: CALL BY CONTENT phrase - literal-2 Can be a figurative constant (except ALL literal or NULL/NULLS) 
// p349: INSPECT statement with CONVERTING phrase - When identifier-1 is of usage DISPLAY or NATIONAL, literals can be any figurative constant that does not begin with the word ALL
// p432: STOP statement - literal can be any figurative constant except ALL literal.
// p433: STRING statement - literal-1 or literal-2can be any figurative constant that does not begin with the word ALL (except NULL). 
// p441: UNSTRING statement - literal-1 or literal-2 must not be a figurative constant that begins with the word ALL. 
// 
// ==> ALL literal figurative constant is referenced explicitely via allFigurativeConstant in all CodeElements rules where it is allowed, we don't need to include it in the generic literal rules

allFigurativeConstant: ALL (figurativeConstant | notNullTerminatedAlphanumericOrNationalLiteralToken);

// - 3. Reserved names -

// p16: Special registers are reserved words that name storage areas generated by the
// compiler. Their primary use is to store information produced through specific
// COBOL features. Each such storage area has a fixed name, and must not be
// defined within the program.
// ... p16-p17 : more details on all special registers ...
// Unless otherwise explicitly restricted, a special register can be used wherever a
// data-name or identifier that has the same definition as the implicit definition of the
// special register can be used. Implicit definitions, if applicable, are given in the
// specification of each special register.
// You can specify an alphanumeric special register in a function wherever an
// alphanumeric argument to a function is allowed, unless specifically prohibited.
// If qualification is allowed, special registers can be qualified as necessary to provide
// uniqueness. (For more information, see “Qualification ? on page 65.)
// ... p17-p33 : more details on all special registers ...

specialRegister: (DEBUG_CONTENTS |
                  DEBUG_ITEM |
                  DEBUG_LINE |
                  DEBUG_NAME |
                  DEBUG_SUB_1 |
                  DEBUG_SUB_2 |
                  DEBUG_SUB_3 |
                  JNIENVPTR |
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

// + LINAGE_COUNTER, ADDRESS OF and LENGTH OF require qualification => they are defined in CobolExpressions.g4

// p33: Special object identifiers
// COBOL provides two special object identifiers, SELF and SUPER
// SELF refers to the object instance used to invoke the currently executing method. 
// SUPER refers to the object instance used to invoke the currently executing method. 
// The resolution of the method to be invoked ignores any methods declared in the class definition of the currently executing method 
// and methods defined in any class derived from that class. 
// Thus, the method invoked is inherited from an ancestor class. 

selfObjectIdentifier: SELF;

superObjectIdentifier: SUPER;

// p 115 : STANDARD-1
// Specifies the ASCII character set.
// STANDARD-2
// Specifies the International Reference Version of ISO/IEC 646, 7-bit
// coded character set for information interchange.
// NATIVE
// Specifies the native character code set. If the ALPHABET clause is
// omitted, EBCDIC is assumed.
// EBCDIC
// Specifies the EBCDIC character set.

standardCollatingSequence: STANDARD_1 | STANDARD_2 | NATIVE | EBCDIC;


// --- Compile-time constant values used in the Cobol grammar ---

booleanValue: TRUE | FALSE;

integerValue: IntegerLiteral;

integerValue2: LevelNumber;

numericValue: numericLiteralToken | numericFigurativeConstant;

characterValue1: alphanumericLiteralToken;

characterValue2: alphanumericLiteralToken | figurativeConstant;

characterValue3: alphanumericOrNationalLiteralToken;

characterValue4: alphanumericOrNationalLiteralToken | figurativeConstant;

alphanumericValue1: alphanumericLiteralToken;

alphanumericValue2: alphanumericOrNationalLiteralToken;

alphanumericValue3: alphanumericOrNationalLiteralToken | figurativeConstant;

alphanumericValue4: UserDefinedWord;

alphanumericValue5: UserDefinedWord | alphanumericLiteralToken;

alphanumericValue6: CommentEntry;

alphanumericValue7: PictureCharacterString;

alphanumericValue8: ExecStatementText;

alphanumericValue9: specialRegister;

alphanumericValue10: standardCollatingSequence;

alphanumericValue11: SymbolicCharacter;

alphanumericValue12: SectionParagraphName;

enumeratedValue1: UserDefinedWord;

enumeratedValue2: IntrinsicFunctionName;

enumeratedValue3: ExecTranslatorName;

// Repeated character values which can only be computed 
//   . at the second stage of parsing
//   . when compared or affected to a data storage area

repeatedCharacterValue1: figurativeConstant;

repeatedCharacterValue2: figurativeConstant | allFigurativeConstant;

repeatedCharacterValue3: figurativeConstantForXMLGenerate;

// Pointers or object references

nullPointerValue: nullFigurativeConstant;

// Any type of value

value1: numericValue | alphanumericValue2 | repeatedCharacterValue2;

value2: numericValue | alphanumericValue2 | repeatedCharacterValue2 | nullPointerValue;


// --- Cobol symbol definitions and symbol references ---

// - Commonly used symbol definition or reference syntax -
// (not strictly necessary to describe the grammar, but useful to optimize the CodeElements builder code)

symbolDefinition1: alphanumericValue1;

symbolDefinition2: alphanumericValue2;

symbolDefinition4: alphanumericValue4;

symbolDefinition5: alphanumericValue5;

symbolDefinition11: alphanumericValue11;

symbolDefinition12: alphanumericValue12;

symbolReference1: alphanumericValue1;

symbolReference2: alphanumericValue2;

symbolReference4: alphanumericValue4;

symbolReference5: alphanumericValue5;

symbolReference9: alphanumericValue9; // specialRegister

symbolReference10: alphanumericValue10; // standardCollatingSequence

symbolReference11: alphanumericValue11;

// [TYPECOBOL] extension : rule modified to supportTYPE DATE (instead of TC-DATE or something)
symbolReference12: alphanumericValue4 | DATE;

ambiguousSymbolReference1: alphanumericValue1;

ambiguousSymbolReference4: alphanumericValue4;

symbolDefinitionOrReference1: alphanumericValue1;

symbolDefinitionOrReference4: alphanumericValue4;

externalName1: enumeratedValue1;

externalName2: enumeratedValue2;

externalName3: enumeratedValue3;

externalName5: alphanumericValue5;

externalNameOrSymbolReference4: alphanumericValue4;

externalNameOrSymbolReference5: alphanumericValue5;

// ** Code structure **

// p85 : program-name can be specified either as a user-defined word or in an alphanumeric literal. 
//       program-name cannot be a figurative constant. 
//       Either way, program-name must follow the rules for forming program-names. 
//       Any lowercase letters in the literal are folded to uppercase.

// p101 : program-name
// A user-defined word or alphanumeric literal, but not a figurative constant,
// that identifies your program. It must follow the following rules of
// formation, depending on the setting of the PGMNAME compiler option:
// PGMNAME(COMPAT)
// The name can be up to 30 characters in length.
// Only the hyphen, underscore, digits 0-9, and alphabetic characters
// are allowed in the name when it is specified as a user-defined
// word.
// At least one character must be alphabetic.
// The hyphen cannot be the first or last character.
// If program-name is an alphanumeric literal, the rules for the name
// are the same except that the extension characters $, #, and @ can be
// included in the name of the outermost program and the
// underscore can be the first character.
// PGMNAME (LONGUPPER)
// If program-name is a user-defined word, it can be up to 30
// characters in length.
// If program-name is an alphanumeric literal, the literal can be up to
// 160 characters in length. The literal cannot be a figurative constant.
// Only the hyphen, underscore, digits 0-9, and alphabetic characters
// are allowed in the name when the name is specified as a
// user-defined word.
// At least one character must be alphabetic.
// The hyphen cannot be the first or last character.
// If program-name is an alphanumeric literal, the underscore character
// can be the first character.
// External program-names are processed with alphabetic characters
// folded to uppercase.
// PGMNAME (LONGMIXED)
// program-name must be specified as an alphnumeric literal, which
// can be up to 160 characters in length. The literal cannot be a
// figurative constant.
// program-name can consist of any character in the range X'41' to
// X'FE'.

// p86 : The program-name of a program is specified in the PROGRAM-ID paragraph of the program's IDENTIFICATION DIVISION. 
//       A program-name can be referenced only by the CALL statement, the CANCEL statement, the SET statement, or the END PROGRAM marker.

// p86 : A separately compiled program and all of its directly and indirectly contained programs 
//       must have unique program-names within that separately compiled program.

// p86 : The following rules define the scope of a program-name:
// - If the program-name is that of a program that does not possess the COMMON
//   attribute and that program is directly contained within another program, that
//   program-name can be referenced only by statements included in that containing program.
// - If the program-name is that of a program that does possess the COMMON
//   attribute and that program is directly contained within another program, that
//   program-name can be referenced only by statements included in the containing
//   program and any programs directly or indirectly contained within that
//   containing program, except that program possessing the COMMON attribute
//   and any programs contained within it.
// - If the program-name is that of a program that is separately compiled, that
//   program-name can be referenced by statements included in any other program
//   in the run unit, except programs it directly or indirectly contains.
// The mechanism used to determine which program to call is as follows:
// - If one of two programs that have the same name as that specified in the CALL
//   statement is directly contained within the program that includes the CALL
//   statement, that program is called.
// - If one of two programs that have the same name as that specified in the CALL
//   statement possesses the COMMON attribute and is directly contained within
//   another program that directly or indirectly contains the program that includes
//   the CALL statement, that common program is called unless the calling program
//   is contained within that common program.
// - Otherwise, the separately compiled program is called.

programNameDefinition: symbolDefinition5;

programNameReference1: symbolReference1;

programNameReference2: symbolReference5;

programNameReference3: symbolReference4;

// p330: ENTRY statement
// literal-1 
// Must be an alphanumeric literal that conform to the rules for the formation of a program-name in an outermost program (see “PROGRAM-ID paragraph” on page 100). 
// Must not match the program-ID or any other ENTRY literal in this program. 
// Must not be a figurative constant.

programEntryDefinition: symbolDefinition1;

// [Type ambiguity] at this parsing stage
programNameReferenceOrProgramEntryReference: ambiguousSymbolReference1;

// p252: Section-name
// A user-defined word that identifies a section. A referenced
// section-name, because it cannot be qualified, must be unique
// within the program in which it is defined.

sectionNameDefinition: symbolDefinition12;

sectionNameReference: symbolReference4;

// p253: Paragraph-name
// A user-defined word that identifies a paragraph. A
// paragraph-name, because it can be qualified, need not be unique.
// If there are no declaratives (format 2), a paragraph-name is not
// required in the PROCEDURE DIVISION.

paragraphNameDefinition: symbolDefinition12;

paragraphNameReference: symbolReference4;

// [Type ambiguity] at this parsing stage
paragraphNameReferenceOrSectionNameReference: ambiguousSymbolReference4;

// p103 : class-name
// A user-defined word that identifies the class. class-name can optionally
// have an entry in the REPOSITORY paragraph of the configuration section
// of the class definition.

// p103 : class-name-1 and class-name-2 must conform to the normal rules of formation for a
// COBOL user-defined word, using single-byte characters.
// See “REPOSITORY paragraph” on page 121 for details on specifying a class-name
// that is part of a Java package or for using non-COBOL naming conventions for
// class-names.

classNameDefinition: symbolDefinition4;

classNameReference: symbolReference4;

// p122: external-class-name-1
// An alphanumeric literal containing a name that enables a COBOL program
// to define or access classes with class-names that are defined using Java
// rules of formation.
// The name must conform to the rules of formation for a fully qualified Java
// class-name. If the class is part of a Java package, external-class-name-1 must
// specify the fully qualified name of the package, followed by a period,
// followed by the simple name of the Java class.
// See Java Language Specification, Third Edition, by Gosling et al., for Java
// class-name formation rules.

classNameDefOrRef: symbolDefinitionOrReference4;

externalClassNameDefOrRef: symbolDefinitionOrReference1;

// p104 : method-name
// An alphanumeric literal or national literal that contains the name of the
// method. The name must conform to the rules of formation for a Java
// method name. Method names are used directly, without translation. The
// method name is processed in a case-sensitive manner.

methodNameDefinition: symbolDefinition2;

methodNameReference: symbolReference2;

// [TYPECOBOL] extension : user defined functions

functionNameDefinition: symbolDefinition4;

functionNameReference: symbolReference4;

// ** Environment vars **

// p115 : mnemonic-name-1 , mnemonic-name-2
// mnemonic-name-1 and mnemonic-name-2 follow the rules of formation for
// user-defined names. 

// p115 : Mnemonic-names and environment-names need not be unique. If you
// choose a mnemonic-name that is also an environment-name, its definition
// as a mnemonic-name will take precedence over its definition as an
// environment-name.

// mnemonic-name-1 can be used in ACCEPT, DISPLAY, and WRITE statements. 
// mnemonic-name-2 can be referenced only in the SET statement. 

mnemonicForEnvironmentNameDefinition: symbolDefinition4;

mnemonicForEnvironmentNameReference: symbolReference4;

// [Type ambiguity] at this parsing stage
mnemonicForEnvironmentNameReferenceOrEnvironmentName: externalNameOrSymbolReference4;

// p115 : mnemonic-name-1 , mnemonic-name-2
// mnemonic-name-1 and mnemonic-name-2 follow the rules of formation for
// user-defined names. 

// mnemonic-name-2 can qualify condition-1 or condition-2 names.

mnemonicForUPSISwitchNameDefinition: symbolDefinition4;

mnemonicForUPSISwitchNameReference: symbolReference4;

// p115 : upsiSwitchName
// A 1-byte user-programmable status indicator (UPSI) switch.
// - condition on UPSI switch status

conditionForUPSISwitchNameDefinition: symbolDefinition4;

// ** Character sets **

// p117 : symbolic-character-1 is a user-defined word and must contain at least one
// alphabetic character. The same symbolic-character can appear only once in
// a SYMBOLIC CHARACTERS clause. The symbolic character can be a
// DBCS user-defined word.
// The internal representation of symbolic-character-1 is the internal
// representation of the character that is represented in the specified character
// set. The following rules apply:
// - The relationship between each symbolic-character-1 and the corresponding
// integer-1 is by their position in the SYMBOLIC CHARACTERS clause.
// The first symbolic-character-1 is paired with the first integer-1; the second
// symbolic-character-1 is paired with the second integer-1; and so forth.
// - There must be a one-to-one correspondence between occurrences of
// symbolic-character-1 and occurrences of integer-1 in a SYMBOLIC
// CHARACTERS clause.
// - If the IN phrase is specified, integer-1 specifies the ordinal position of the
// character that is represented in the character set named by
// alphabet-name-2. This ordinal position must exist.
// - If the IN phrase is not specified, symbolic-character-1 represents the
// character whose ordinal position in the native character set is specified
// by integer-1.
// Ordinal positions are numbered starting from 1.

symbolicCharacterDefinition: symbolDefinition11;

symbolicCharacterReference: symbolReference10;

// p 115 : ALPHABET alphabet-name-1 IS
// alphabet-name-1 specifies a collating sequence when used in:
// - The PROGRAM COLLATING SEQUENCE clause of the object-computer
// paragraph
// - The COLLATING SEQUENCE phrase of the SORT or MERGE statement
// alphabet-name-1 specifies a character code set when used in:
// - The FD entry CODE-SET clause
// - The SYMBOLIC CHARACTERS clause

alphabetNameDefinition: symbolDefinition4;

alphabetNameReference: symbolReference4;

intrinsicAlphabetNameReference: /* standardCollatingSequence */ symbolReference10;

alphabetName: alphabetNameReference | /* standardCollatingSequence */ intrinsicAlphabetNameReference;

// p118 : CLASS class-name-1 IS
// Provides a means for relating a name to the specified set of characters
// listed in that clause. class-name-1 can be referenced only in a class
// condition. The characters specified by the values of the literals in this
// clause define the exclusive set of characters of which this class consists.
// The class-name in the CLASS clause can be a DBCS user-defined word.

characterClassNameDefinition: symbolDefinition4;

characterClassNameReference: symbolReference4;

// ** Data **

// [TYPECOBOL] extension : user defined data types

dataTypeNameDefinition: symbolDefinition4; // Not used in the grammar - merged with dataNameDefinition

dataTypeNameReference: symbolReference12;

// p187: data-name-1
// Explicitly identifies the data being described.
// data-name-1, if specified, identifies a data item used in the program.
// data-name-1 must be the first word following the level-number.
// The data item can be changed during program execution.
// data-name-1 must be specified for level-66 and level-88 items. It must also
// be specified for any entry containing the GLOBAL or EXTERNAL clause,
// and for record description entries associated with file description entries
// that have the GLOBAL or EXTERNAL clauses.

dataNameDefinition: symbolDefinition4;

dataNameReference: symbolReference4;

intrinsicDataNameReference: /* specialRegister */ symbolReference9;

// [Type ambiguity] at this parsing stage
dataNameReferenceOrFileNameReference: ambiguousSymbolReference4;

// [Type ambiguity] at this parsing stage
dataNameReferenceOrIndexNameReference: ambiguousSymbolReference4;

// [Type ambiguity] at this parsing stage
dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference: ambiguousSymbolReference4;

// [Type ambiguity] at this parsing stage
dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference: ambiguousSymbolReference4;

// [Type ambiguity] at this parsing stage
dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference: ambiguousSymbolReference4;

// [Type ambiguity] at this parsing stage
dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference: ambiguousSymbolReference4;

// [Type ambiguity] at this parsing stage
dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference: ambiguousSymbolReference4;

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

conditionNameDefinition: symbolDefinition4;

// [Type ambiguity] at this parsing stage
conditionNameReferenceOrConditionForUPSISwitchNameReference: ambiguousSymbolReference4;

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

indexNameDefinition: symbolDefinition4;

indexNameReference: symbolReference4;

// ** Files **

// p130: file-name-1
// Must be identified by an FD or SD entry in the DATA DIVISION.
// A file-name must conform to the rules for a COBOL user-defined name, must contain at least one alphabetic character, and must be unique within this program.

fileNameDefinition: symbolDefinition4;

fileNameReference: symbolReference4;

// p120: XML-SCHEMA xml-schema-name-1 IS
// xml-schema-name-1 can be referenced only in an XML PARSE statement.
// The xml-schema-name in the XML SCHEMA clause can be a DBCS
// user-defined word.

xmlSchemaNameDefinition: symbolDefinition4;

xmlSchemaNameReference: symbolReference4;


// --- Qualified names : give explicit context to resolve ambiguous name references ---

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

// p252: Procedures
// Within the PROCEDURE DIVISION, a procedure consists of a section or a group of
// sections, and a paragraph or group of paragraphs.
// A procedure-name is a user-defined name that identifies a section or a paragraph.

procedureName: 
	paragraphNameReferenceOrSectionNameReference | qualifiedParagraphNameReference;

// p66: References to PROCEDURE DIVISION names
// PROCEDURE DIVISION names that are explicitly referenced in a program must be
// unique within a section.
// A section-name is the highest and only qualifier available for a paragraph-name
// and must be unique if referenced. (Section-names are described under
// “Procedures” on page 252.)
// If explicitly referenced, a paragraph-name must not be duplicated within a section.
// When a paragraph-name is qualified by a section-name, the word SECTION must
// not appear. A paragraph-name need not be qualified when referred to within the
// section in which it appears. A paragraph-name or section-name that appears in a
// program cannot be referenced from any other program.

qualifiedParagraphNameReference: 
	paragraphNameReference (IN | OF) sectionNameReference;

// p65: Uniqueness of reference
// Every user-defined name in a COBOL program is assigned by the user to name a
// resource for solving a data processing problem. To use a resource, a statement in a
// COBOL program must contain a reference that uniquely identifies that resource.
// To ensure uniqueness of reference, a user-defined name can be qualified. A
// subscript is required for unique reference to a table element, except as specified in
// “Subscripting” on page 71. A data-name or function-name, any subscripts, and the
// specified reference-modifier uniquely reference a data item defined by reference
// modification.
// When the same name has been assigned in separate programs to two or more
// occurrences of a resource of a given type, and when qualification by itself does not
// allow the references in one of those programs to differentiate between the
// identically named resources, then certain conventions that limit the scope of names
// apply. The conventions ensure that the resource identified is that described in the
// program containing the reference. For more information about resolving
// program-names, see “Resolution of names” on page 62.
// Unless otherwise specified by the rules for a statement, any subscripts and
// reference modification are evaluated only once as the first step in executing that
// statement.

// p67: References to DATA DIVISION names
// This section discusses the following types of references.
// - “Simple data reference”
// - “Identifiers” on page 68
// Simple data reference
// The most basic method of referencing data items in a COBOL program is simple
// data reference, which is data-name-1 without qualification, subscripting, or reference
// modification. Simple data reference is used to reference a single elementary or
// group item.
// data-name-1
// Can be any data description entry.
// data-name-1 must be unique in a program.

qualifiedDataName: 
	dataNameReference | qualifiedDataName1;

qualifiedDataName1: 
	dataNameReference ((IN | OF) dataNameReferenceOrFileNameReference)+;

// [Type ambiguity] at this parsing stage
qualifiedDataNameOrIndexName: 
	dataNameReferenceOrIndexNameReference | qualifiedDataName1;

// p60: record-name 
// record-name assigns a name to a record.
// A record-name is global if the GLOBAL clause is specified in the record description that declares the record-name,
// or in the case of record description entries in the FILE SECTION, if the GLOBAL clause is specified 
// in the file description entry for the file name associated with the record description entry. 

recordName: 
	qualifiedDataName;

// p70: If qualification is used to make a condition-name unique, the associated
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
// “SPECIAL-NAMES paragraph” on page 112.

// p70: Format 1: condition-name in data division
// p70: Format 2: condition-name in SPECIAL-NAMES paragraph

// => Impossible to distinguish between the following types at this parsing stage ;
// - first token : conditionName or conditionForUPSISwitchName
// - following tokens : dataName or fileName or menmonicForUPSISwitchName

qualifiedConditionName: 
	conditionNameReferenceOrConditionForUPSISwitchNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)*;

// Ambiguous references in Cobol grammar rules

// [Type ambiguity] at this parsing stage
qualifiedDataNameOrQualifiedConditionName:
	dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference | qualifiedDataNameOrQualifiedConditionName1;

qualifiedDataNameOrQualifiedConditionName1:
	dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReference ((IN | OF) dataNameReferenceOrFileNameReferenceOrMnemonicForUPSISwitchNameReference)+;

// [Type ambiguity] at this parsing stage
qualifiedDataNameOrQualifiedConditionNameOrIndexName:
	dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrIndexNameReference | qualifiedDataNameOrQualifiedConditionName1;

// [Type ambiguity] at this parsing stage
qualifiedDataNameOrQualifiedConditionNameOrFileName:
	dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrFileNameReference | qualifiedDataNameOrQualifiedConditionName1;

// [Type ambiguity] at this parsing stage
qualifiedDataNameOrQualifiedConditionNameOrClassName:
	dataNameReferenceOrConditionNameReferenceOrConditionForUPSISwitchNameReferenceOrClassNameReference | qualifiedDataNameOrQualifiedConditionName1;

qualifiedIndexName: indexName=symbolReference4 | (symbolReference4 QualifiedNameSeparator)+ TcHeadDefiniiton=symbolReference4;

// - 4. External names -

// ** Environment vars **

// p114 : environmentName
// System devices or standard system actions taken by the compiler.
// Valid specifications for environment-name-1 are shown in the following table.
//   environmentName : Meaning : Allowed in
//   SYSIN | SYSIPT : System logical input unit : ACCEPT
//   SYSOUT | SYSLIST | SYSLST : System logical output unit : DISPLAY
//   SYSPUNCH | SYSPCH : System punch device : DISPLAY
//   CONSOLE : Console : ACCEPT and DISPLAY
//   C01 through C12 : Skip to channel 1 through channel 12, respectively : WRITE ADVANCING
//   CSP : Suppress spacing : WRITE ADVANCING
//   S01 through S05 : Pocket select 1 through 5 on punch devices : WRITE ADVANCING
//   AFP-5A : Advanced Function Printing : WRITE ADVANCING

// SYSIN | SYSIPT | SYSOUT | SYSLIST | SYSLST | SYSPUNCH | SYSPCH | CONSOLE |
// C01 | C02 | C03 | C04 | C05 | C06 | C07 | C08 | C09 | C10 | C11 | C12 |
// CSP | S01 | S02 | S03 | S04 | S05 | AFP-5A;

environmentName: externalName1;

// p115 : upsiSwitchName
// A 1-byte user-programmable status indicator (UPSI) switch.
// Valid specifications for environment-name-2 are UPSI-0 through UPSI-7.

// UPSI-0 | UPSI-1 | UPSI-2 | UPSI-3 | UPSI-4 | UPSI-5 | UPSI-6 | UPSI-7;

upsiSwitchName: externalName1;

// ** Files ***

// * text-name , library-name
// text-name identifies the copy text. library-name identifies where the copy text
// exists.
// - Can be from 1-30 characters in length
// - Can contain the following characters: Latin uppercase letters A-Z, Latin
//   lowercase letters a-z, digits 0-9, and hyphen
// - The first or last character must not be a hyphen
// - Cannot contain an underscore
// Neither text-name nor library-name need to be unique within a program.
// They can be identical to other user-defined words in the program.
// text-name need not be qualified. If text-name is not qualified, a library-name
// of SYSLIB is assumed.
// When compiling from JCL or TSO, only the first eight characters are used
// as the identifying name. When compiling with the cob2 command and
// processing COPY text residing in the z/OS UNIX file system, all characters
// are significant.
// * literal-1 , literal-2
// Must be alphanumeric literals. literal-1 identifies the copy text. literal-2
// identifies where the copy text exists.
// When compiling from JCL or TSO:
// - Literals can be from 1-30 characters in length.
// - Literals can contain characters: A-Z, a-z, 0-9, hyphen, @, #, or $.
// - The first or last character must not be a hyphen.
// - Literals cannot contain an underscore.
// - Only the first eight characters are used as the identifying name.
// When compiling with the cob2 command and processing COPY text
// residing in the z/OS UNIX file system, the literal can be from 1 to 160
// characters in length.
// The uniqueness of text-name and library-name is determined after the formation and
// conversion rules for a system-dependent name have been applied.
// For information about the mapping of characters in the text-name, library-name, and
// literals, see Compiler-directing statements in the Enterprise COBOL Programming
// Guide.

textName: externalName5;

libraryName: externalName5;

qualifiedTextName: textName ((IN | OF) libraryName)?;

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

// p132: Assignment name for environment variable
// The name component of assignment-name-1 is initially treated as a ddname. 
// If no file has been allocated using this ddname, then name is treated as an environment variable

assignmentName: externalName5;

// [Type ambiguity] at this parsing stage
assignmentNameOrFileNameReference : externalNameOrSymbolReference5;

// ** Runtime functions **

// NB : Because FunctionNames are not reserved words,
// and because the exact list of the instrinsic functions, their types and their arguments are more a library matter than a language matter,
// we do not try to check the validity of the number of arguments, the types of arguments alowed, and the referenceModifier pertinence
// at the grammar level.                      
// All these rules will be checked at a later time by looking at an independent table of instrinsic functions.

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

intrinsicFunctionName: externalName2;

// IBM Enterprise Cobol 5.1 for zOS - Programming Guide.pdf
// p423: To communicate with DB2, do these steps:
// Code any SQL statements that you need, delimiting them with EXEC SQL and END-EXEC statements.
// execStatement: (EXEC | EXECUTE) execTranslatorName  ExecStatementText* END_EXEC;
// Names of specialized Cobol preprocessors or coprocessors :
// DB2 coprocessor
//   SQL,
// IMS SQL coprocessor
//   SQLIMS,
// Integrated CICS translator
//   CICS,
// Integrated CICS translator
//   DLI

execTranslatorName : externalName3;

// ** Compiler enumerations (handled as user defined words) **

// p528: *CONTROL (*CBL) statement
// With the *CONTROL (or *CBL) statement, you can selectively display or suppress
// the listing of source code, object code, and storage maps throughout the source
// text. 
// These are not reserved words, but the only possible values are the following
// SOURCE | NOSOURCE | LIST | NOLIST | MAP | NOMAP

controlCblOption: enumeratedValue1;

// p182: Permitted values for RECORDING MODE are:
// * Recording mode F (fixed)
// All the records in a file are the same length and each is wholly contained
// within one block. Blocks can contain more than one record, and there is
// usually a fixed number of records for each block. In this mode, there are
// no record-length or block-descriptor fields.
//* Recording mode V (variable)
// The records can be either fixed-length or variable-length, and each must be
// wholly contained within one block. Blocks can contain more than one
// record. Each data record includes a record-length field and each block
// includes a block-descriptor field. These fields are not described in the
// DATA DIVISION. They are each 4 bytes long and provision is
// automatically made for them. These fields are not available to you.
// * Recording mode U (fixed or variable)
// The records can be either fixed-length or variable-length. However, there is
// only one record for each block. There are no record-length or
// block-descriptor fields.
// You cannot use RECORDING MODE U if you are using the BLOCK
// CONTAINS clause.
// * Recording mode S (spanned)
// The records can be either fixed-length or variable-length, and can be larger
// than a block. If a record is larger than the remaining space in a block, a
// segment of the record is written to fill the block. The remainder of the
// record is stored in the next block (or blocks, if required). Only complete
// records are made available to you. Each segment of a record in a block,
// even if it is the entire record, includes a segment-descriptor field, and each
// block includes a block-descriptor field. These fields are not described in the
// DATA DIVISION; provision is automatically made for them. These fields
// are not available to you.
// When recording mode S is used, the BLOCK CONTAINS CHARACTERS clause
// must be used. Recording mode S is not allowed for ASCII files.

// possible values : F | V | U | S

recordingMode: enumeratedValue1; 


// - 5. Reserved words -

// This list of reserved words is useful to parse the COPY REPLACING operands
// -> it must be updated each time a new token type is added above

literalOrUserDefinedWordOReservedWordExceptCopy: (
    // Separators - Whitespace
    // => excluded
    // Comments
    // => excluded
    // Separators - Syntax
    // => excluded
    // Special character word - Arithmetic operators
    // => excluded
    // Special character word - Relational operators
    // => excluded
    // Literals - Alphanumeric
    AlphanumericLiteral |
    HexadecimalAlphanumericLiteral |
    NullTerminatedAlphanumericLiteral |
    NationalLiteral |
    HexadecimalNationalLiteral |
    DBCSLiteral |
    // Literals - Numeric
    LevelNumber |
    IntegerLiteral |
    DecimalLiteral |
    FloatingPointLiteral |
    // Literals - Syntax tokens
    // => excluded


    // Symbols
    SectionParagraphName |
    IntrinsicFunctionName |
    ExecTranslatorName |
    PartialCobolWord |
    UserDefinedWord |
    // Keywords - Compiler directive starting tokens
    ASTERISK_CBL |
    ASTERISK_CONTROL |
    BASIS |
    CBL |
    // COPY => excluded
    DELETE_CD |
    EJECT |
    ENTER |
    EXEC_SQL_INCLUDE |
    INSERT |
    PROCESS |
    READY |
    RESET |
    REPLACE |
    SERVICE_CD |
    SKIP1 |
    SKIP2 |
    SKIP3 |
    TITLE |
    // Keywords - Code element starting tokens
    ACCEPT |
    ADD |
    ALTER |
    APPLY |
    CALL |
    CANCEL |
    CLOSE |
    COMPUTE |
    CONFIGURATION |
    CONTINUE |
    DATA |
    DECLARATIVES |
    DECLARE |
    DELETE |
    DISPLAY |
    DIVIDE |
    ELSE |
    END |
    END_ADD |
    END_CALL |
    END_COMPUTE |
    END_DECLARE |
    END_DELETE |
    END_DIVIDE |
    END_EVALUATE |
    END_EXEC |
    END_IF |
    END_INVOKE |
    END_MULTIPLY |
    END_PERFORM |
    END_READ |
    END_RETURN |
    END_REWRITE |
    END_SEARCH |
    END_START |
    END_STRING |
    END_SUBTRACT |
    END_UNSTRING |
    END_WRITE |
    END_XML |
    ENTRY |
    ENVIRONMENT |
    EVALUATE |
    EXEC |
    EXECUTE |
    EXIT |
    FD |
    FILE |
    FILE_CONTROL |
    GO |
    GOBACK |
    I_O_CONTROL |
    ID |
    IDENTIFICATION |
    IF |
    INITIALIZE |
    INPUT_OUTPUT |
    INSPECT |
    INVOKE |
    LINKAGE |
    LOCAL_STORAGE |
    MERGE |
    MOVE |
    MULTIPLE |
    MULTIPLY |
    NEXT |
    OBJECT_COMPUTER |
    OPEN |
    PERFORM |
    PROCEDURE |
    READ |
    RELEASE |
    REPOSITORY |
    RERUN |
    RETURN |
    REWRITE |
    SAME |
    SD |
    SEARCH |
    SELECT |
    SERVICE |
    SET |
    SORT |
    SOURCE_COMPUTER |
    SPECIAL_NAMES |
    START |
    STOP |
    STRING |
    SUBTRACT |
    UNSTRING |
    USE |
    WHEN |
    WORKING_STORAGE |
    WRITE |
    XML |
    // Keywords - Special registers
    ADDRESS |
    DEBUG_CONTENTS |
    DEBUG_ITEM |
    DEBUG_LINE |
    DEBUG_NAME |
    DEBUG_SUB_1 |
    DEBUG_SUB_2 |
    DEBUG_SUB_3 |
    JNIENVPTR |
    LENGTH |
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
    XML_TEXT |
    // Keywords - Figurative constants
    HIGH_VALUE |
    HIGH_VALUES |
    LOW_VALUE |
    LOW_VALUES |
    NULL |
    NULLS |
    QUOTE |
    QUOTES |
    SPACE |
    SPACES |
    ZERO |
    ZEROES |
    ZEROS |
    SymbolicCharacter |
    // Keywords - Special object identifiers
    SELF |
    SUPER |
    // Keywords - Syntax tokens
    ACCESS |
    ADVANCING |
    AFTER |
    ALL |
    ALPHABET |
    ALPHABETIC |
    ALPHABETIC_LOWER |
    ALPHABETIC_UPPER |
    ALPHANUMERIC |
    ALPHANUMERIC_EDITED |
    ALSO |
    ALTERNATE |
    AND |
    ANY |
    ARE |
    AREA |
    AREAS |
    ASCENDING |
    ASSIGN |
    AT |
    ATTRIBUTE |
    ATTRIBUTES |
    AUTHOR |
    BEFORE |
    BEGINNING |
    BINARY |
    BLANK |
    BLOCK |
    BOTTOM |
    BY |
    CHARACTER |
    CHARACTERS |
    CLASS |
    CLASS_ID |
    COBOL |
    CODE |
    CODE_SET |
    COLLATING |
    COM_REG |
    COMMA |
    COMMON |
    COMP |
    COMP_1 |
    COMP_2 |
    COMP_3 |
    COMP_4 |
    COMP_5 |
    COMPUTATIONAL |
    COMPUTATIONAL_1 |
    COMPUTATIONAL_2 |
    COMPUTATIONAL_3 |
    COMPUTATIONAL_4 |
    COMPUTATIONAL_5 |
    CONTAINS |
    CONTENT |
    CONVERTING |
    CORR |
    CORRESPONDING |
    COUNT |
    CURRENCY |
    DATE |
    DATE_COMPILED |
    DATE_WRITTEN |
    DAY |
    DAY_OF_WEEK |
    DBCS |
    DEBUGGING |
    DECIMAL_POINT |
    DELIMITED |
    DELIMITER |
    DEPENDING |
    DESCENDING |
    DISPLAY_1 |
    DIVISION |
    DOWN |
    DUPLICATES |
    DYNAMIC |
    EBCDIC |
    EGCS |
    ELEMENT |
    ENCODING |
    END_OF_PAGE |
    ENDING |
    EOP |
    EQUAL |
    ERROR |
    EVERY |
    EXCEPTION |
    EXTEND |
    EXTERNAL |
    FACTORY |
    FALSE |
    FILLER |
    FIRST |
    FOOTING |
    FOR |
    FROM |
    FUNCTION |
    FUNCTION_POINTER |
    GENERATE |
    GIVING |
    GLOBAL |
    GREATER |
    GROUP_USAGE |
    I_O |
    IN |
    INDEX |
    INDEXED |
    INHERITS |
    INITIAL |
    INPUT |
    INSTALLATION |
    INTO |
    INVALID |
    IS |
    JUST |
    JUSTIFIED |
    KANJI |
    KEY |
    LABEL |
    LEADING |
    LEFT |
    LESS |
    LINAGE |
    LINE |
    LINES |
    LOCK |
    MEMORY |
    METHOD |
    METHOD_ID |
    MODE |
    MODULES |
    MORE_LABELS |
    NAME |
    NAMESPACE |
    NAMESPACE_PREFIX |
    NATIONAL |
    NATIONAL_EDITED |
    NATIVE |
    NEGATIVE |
    NEW |
    NO |
    NONNUMERIC |
    NOT |
    NUMERIC |
    NUMERIC_EDITED |
    OBJECT |
    OCCURS |
    OF |
    OFF |
    OMITTED |
    ON |
    OPTIONAL |
    OR |
    ORDER |
    ORGANIZATION |
    OTHER |
    OUTPUT |
    OVERFLOW |
    OVERRIDE |
    PACKED_DECIMAL |
    PADDING |
    PAGE |
    PARSE |
    PASSWORD |
    PIC |
    PICTURE |
    POINTER |
    POSITION |
    POSITIVE |
    PROCEDURE_POINTER |
    PROCEDURES |
    PROCEED |
    PROCESSING |
    PROGRAM |
    PROGRAM_ID |
    RANDOM |
    RECORD |
    RECORDING |
    RECORDS |
    RECURSIVE |
    REDEFINES |
    REEL |
    REFERENCE |
    REFERENCES |
    RELATIVE |
    RELOAD |
    REMAINDER |
    REMOVAL |
    RENAMES |
    REPLACING |
    RESERVE |
    RETURNING |
    REVERSED |
    REWIND |
    RIGHT |
    ROUNDED |
    RUN |
    SECTION |
    SECURITY |
    SEGMENT_LIMIT |
    SENTENCE |
    SEPARATE |
    SEQUENCE |
    SEQUENTIAL |
    SIGN |
    SIZE |
    SORT_MERGE |
    SQL |
    SQLIMS |
    STANDARD |
    STANDARD_1 |
    STANDARD_2 |
    STATUS |
    SUPPRESS |
    SYMBOL |
    SYMBOLIC |
    SYNC |
    SYNCHRONIZED |
    TALLYING |
    TAPE |
    TEST |
    THAN |
    THEN |
    THROUGH |
    THRU |
    TIME |
    TIMES |
    TO |
    TOP |
    TRACE |
    TRAILING |
    TRUE |
    TYPE |
    UNBOUNDED |
    UNIT |
    UNTIL |
    UP |
    UPON |
    USAGE |
    USING |
    VALIDATING |
    VALUE |
    VALUES |
    VARYING |
    WITH |
    WORDS |
    WRITE_ONLY |
    XML_DECLARATION |
    XML_SCHEMA |
    YYYYDDD |
    YYYYMMDD |
    // Keywords - Cobol 2002
    TYPEDEF |
    STRONG |
    // Keywords - TypeCobol
    UNSAFE |
    PUBLIC |
    PRIVATE |
    IN_OUT | 
	STRICT  |
	QuestionMark
);




// -----------------
// COBOL TEXT FORMAT
// -----------------

// **********************
// 1. Characters encoding
// **********************

// -- 1.1 Character decoding  at compile time --

// p5: Enterprise COBOL provides the CODEPAGE compiler option for specifying a
// coded character set for use at compile time and run time  for code-page-sensitive
// elements, such as:
// - The encoding of literals in the source program
// - The default encoding for data items described with USAGE DISPLAY or
//   DISPLAY-1
// - The default encoding for XML parsing and XML generation
// If you do not specify a code page, the default is code page IBM-1140, CCSID 1140.

// p9: Source text, library text, and pseudo-text can be written in single-byte EBCDIC
// and, for some character-strings, DBCS. (The compiler cannot process source code
// written in ASCII or Unicode.)

// p3: The basic characters used in forming character-strings and separators in source
// code are shown in Table 1.
// ... Table 1. Basic COBOL character set ...

// p3: The contents of 
// - alphanumeric literals, 
// - comment lines, and comment entries 
// can include any of the characters in the computer's compile-time character set.

// p3:  For certain language elements, the basic character set is extended with the EBCDIC
// Double-Byte Character Set (DBCS).
// DBCS characters can be used in forming 
// - user-defined words,
// - alphanumeric literals, 
// - comment lines, and comment entries
// which can include both single-byte and DBCS characters.

// Programming guide / Compiler options / CODEPAGE
// DBCS code pages:
// Compile your COBOL program using the CODEPAGE option with the ccsid set to one of the EBCDIC multibyte character set (MBCS) CCSIDs shown in the table below if the program contains any of the following items: 
// - User-defined words formed with DBCS characters 
// - DBCS (USAGE DISPLAY-1) data items 
// - DBCS literals 
// All of the CCSIDs in the table below identify mixed code pages that refer to a combination of SBCS and DBCS coded character sets. 
// These are also the CCSIDs that are supported for mixed data by DB2.
// Table 1. EBCDIC multibyte coded character set identifiers 
// National language | MBCS CCSID | SBCS CCSID component | DBCS CCSID component 
// Japanese (Katakana-Kanji) 930 290 300 
// Japanese (Katakana-Kanji with euro) 1390 8482 16684 
// Japanese (Katakana-Kanji) 5026 290 4396 
// Japanese (Latin-Kanji) 939 1027 300 
// Japanese (Latin-Kanji with euro) 1399 5123 16684 
// Japanese (Latin-Kanji) 5035 1027 4396 
// Korean 933 833 834 
// Korean 1364 13121 4930 
// Simplified Chinese 935 836 837 
// Simplified Chinese 1388 13124 4933 
// Traditional Chinese 937 28709 835 
// => MBCS CCSIDS : 930,933,935,937,939,1364,1388,1390,1399,5026,5035
// => DBCS CCSIDS : 300,834,835,837,4396,4930,4933,16684

// p9: You can use single-byte and double-byte character-strings to form the following
// items:
// - COBOL words
// - Literals
// - Comment text
// You can use only single-byte characters to form PICTURE character-strings.

// p3: When the NSYMBOL (NATIONAL) compiler option is in effect, literals identified
// by the opening delimiter N" or N' are national literals and can contain any
// single-byte or double-byte characters, or both, that are valid for the compile-time
// code page in effect (either the default code page or the code page specified for the
// CODEPAGE compiler option).

// -- 1.2 Character encoding at runtime --

// p3: Runtime data can include any characters from the runtime character set of the
// computer. The runtime character set of the computer can include alphanumeric
// characters, DBCS characters, and national characters.

// p3p5: National characters are represented in UTF-16, a 16-bit encoding form of Unicode.
// The encoding of national data is not affected by the CODEPAGE compiler option.
// The encoding for national literals and data items described with usage NATIONAL
// is UTF-16BE (big endian), CCSID 1200. A reference to UTF-16 in this document is a
// reference to UTF-16BE.
// When the NSYMBOL (NATIONAL) compiler option is in effect, literals identified
// by the opening delimiter N" or N' are national literals. Characters contained in 
// national literals are represented as national characters at run time.

// p5: A character encoding unit (or encoding unit) is the unit of data that COBOL treats as
// a single character at run time. In this information, the terms character and character
// position refer to a single encoding unit.
// The size of an encoding unit for data items and literals depends on the USAGE
// clause of the data item or the category of the literal as follows:
// - For data items described with USAGE DISPLAY and for alphanumeric literals,
//   an encoding unit is 1 byte, regardless of the code page used and regardless of
//   the number of bytes used to represent a given graphic character.
// - For data items described with USAGE DISPLAY-1 (DBCS data items) and for
//   DBCS literals, an encoding unit is 2 bytes.
// - For data items described with USAGE NATIONAL and for national literals, an
//   encoding unit is 2 bytes.

// p6: The relationship between a graphic character and an encoding unit depends on the
// type of code page used for the data item or literal. See the following types of
// runtime code pages:

// p6: Single-byte EBCDIC code pages
// You can use a single-byte EBCDIC code page in data items described with USAGE
// DISPLAY and in literals of category alphanumeric. An encoding unit is 1 byte and
// each graphic character is represented in 1 byte. For these data items and literals,
// you need not be concerned with encoding units.

// p6: EBCDIC DBCS code pages
// USAGE DISPLAY
// You can use a mixture of single-byte and double-byte EBCDIC characters in data
// items described with USAGE DISPLAY and in literals of category alphanumeric.
// Double-byte characters must be delimited by shift-out and shift-in characters. An
// encoding unit is 1 byte and the size of a graphic character is 1 byte or 2 bytes.
// When alphanumeric data items or literals contain DBCS data, programmers are
// responsible for ensuring that operations do not unintentionally separate the
// multiple encoding units that form a graphic character. Care should be taken with
// reference modification, and truncation during moves should be avoided. The
// COBOL runtime system does not check for a split between the encoding units that
// form a graphic character or for the loss of shift-out or shift-in codes.
// To avoid problems, you can convert alphanumeric literals and data items described
// with usage DISPLAY to national data (UTF-16) by moving the data items or literals
// to data items described with usage NATIONAL or by using the NATIONAL-OF
// intrinsic function. You can then perform operations on the national data with less
// concern for splitting graphic characters. You can convert the data back to USAGE
// DISPLAY by using the DISPLAY-OF intrinsic function.
// USAGE DISPLAY-1
// You can use double-byte characters of an EBCDIC DBCS code page in data items
// described with USAGE DISPLAY-1 and in literals of category DBCS. An encoding
// unit is 2 bytes and each graphic character is represented in a single 2-byte
// encoding unit. For these data items and literals, you need not be concerned with
// encoding units. 

// p7: Unicode UTF-16
// You can use UTF-16 in data items described with USAGE NATIONAL. National
// literals are stored as UTF-16 characters regardless of the code page used for the
// source program. An encoding unit for data items of usage NATIONAL and
// national literals is 2 bytes.
// For most of the characters in UTF-16, a graphic character is one encoding unit.
// Characters converted to UTF-16 from an EBCDIC, ASCII, or EUC code page are
// represented in one UTF-16 encoding unit. Some of the other graphic characters in
// UTF-16 are represented by a surrogate pair or a combining character sequence. A
// surrogate pair consists of two encoding units (4 bytes). A combining character
// sequence consists of a base character and one or more combining marks or a
// sequence of one or more combining marks (4 bytes or more, in 2-byte increments).
// In data items of usage NATIONAL, each 2-byte encoding unit is treated as a
// character.
// When national data contains surrogate pairs or combining character sequences,
// programmers are responsible for ensuring that operations on national characters do
// not unintentionally separate the multiple encoding units that form a graphic
// character. Care should be taken with reference modification, and truncation during
// moves should be avoided. The COBOL runtime system does not check for a split
// between the encoding units that form a graphic character.

// *******************
// 2. Reference format
// *******************

// p51: COBOL source text must be written in COBOL reference format.

// -- 2.1 Areas --

// p51: Reference format consists of the following areas in a 72-character line.
// Sequence number area
// Columns 1 through 6
// Indicator area
// Column 7
// Area A
// Columns 8 through 11
// Area B
// Columns 12 through 72
// This figure illustrates reference format for a COBOL source line.
// 1 2 3 4 5 6             7               8 9 10 11     12 13 . . . 71 72
// Sequence Number Area    Indicator Area  Area A        Area B

// http://publib.boulder.ibm.com/infocenter/iadthelp/v7r1/index.jsp?topic=/com.ibm.etools.iseries.langref.doc/c092539545.htm
// An 80-character COBOL source line is divided into the following reference format areas: 
// Sequence Number Area : Columns 1 through 6 
// Indicator Area : Column 7 
// Area A : Columns 8 through 11 
// Area B : Columns 12 through 72 
// Comment Area : Columns 73 through 80 

// p51: Sequence number area
// The sequence number area can be used to label a source statement line. The
// content of this area can consist of any character in the character set of the
// computer.

// p51: Indicator area
// Use the indicator area to specify the continuation of words or alphanumeric literals
// from the previous line onto the current line, the treatment of text as
// documentation, and debugging lines.
// See “Continuation lines ? on page 54, “Comment lines ? on page 56, and
// “Debugging lines ? on page 57.
// The indicator area can be used for source listing formatting. A slash (/) placed in
// the indicator column causes the compiler to start a new page for the source listing,
// and the corresponding source record to be treated as a comment. The effect can be
// dependent on the LINECOUNT compiler option. For information about the
// LINECOUNT compiler option, see LINECOUNT in the Enterprise COBOL
// Programming Guide.

// p52: Area A
// Certain items must begin in Area A.
// These items are:
// - Division headers
// - Section headers
// - Paragraph headers or paragraph names
// - Level indicators or level-numbers (01 and 77)
// - DECLARATIVES and END DECLARATIVES
// - End program, end class, and end method markers
// ... p52 -> p53: more details ...
// A division header (except when a USING phrase is specified with a PROCEDURE
// DIVISION header) must be immediately followed by a separator period. Except for
// the USING phrase, no text can appear on the same line.
// In the PROCEDURE DIVISION, each of the keywords DECLARATIVES and END
// DECLARATIVES must begin in Area A and be followed immediately by a
// separator period; no other text can appear on the same line. After the keywords
// END DECLARATIVES, no text can appear before the following section header. (See
// “Declaratives ? on page 251.)

// p54: Area B
// Certain items must begin in Area B.
// These items are:
// - Entries, sentences, statements, and clauses
// - Continuation lines
// Entries, sentences, statements, clauses
// The first entry, sentence, statement, or clause begins on either the same line as the
// header or paragraph-name that it follows, or in Area B of the next nonblank line
// that is not a comment line. Successive sentences or entries either begin in Area B of
// the same line as the preceding sentence or entry, or in Area B of the next nonblank
// line that is not a comment line.
// Within an entry or sentence, successive lines in Area B can have the same format
// or can be indented to clarify program logic. The output listing is indented only if
// the input statements are indented. Indentation does not affect the meaning of the
// program. The programmer can choose the amount of indentation, subject only to
// the restrictions on the width of Area B. See also Chapter 5, “Sections and
// paragraphs, ? on page 49.

// p56: Area A or Area B
// Certain items can begin in either Area A or Area B.
// These items are:
// - Level-numbers
// - Comment lines
// - Floating comment indicators (*>)
// - Compiler-directing statements
// - Debugging lines
// - Pseudo-text
// - Blank lines
// ... p56 -> p58: more details ...
// A level-number that must begin in Area A is a one- or two-digit integer with a
// value of 01 or 77. A level-number must be followed by a space or a separator
// period. For more information, see “Level-numbers ? on page 186.
// A level-number that can begin in Area A or B is a one- or two-digit integer with a
// value of 02 through 49, 66, or 88.

// -- 2.2 Continuation lines -- 

// p54: Continuation lines
// Any sentence, entry, clause, or phrase that requires more than one line can be
// continued in Area B of the next line that is neither a comment line nor a blank line.
// The line being continued is a continued line; the succeeding lines are continuation
// lines. Area A of a continuation line must be blank.
// The following cannot be continued:
// - DBCS user-defined words
// - DBCS literals
// - Alphanumeric literals containing DBCS characters
// - National literals containing DBCS characters
// However, alphanumeric literals and national literals in hexadecimal notation can be
// continued regardless of the kind of characters expressed in hexadecimal notation.
// All characters that make up an opening literal delimiter must be on the same line.
// For example, Z", G", N", NX", or X".
// Both characters that make up the pseudo-text delimiter separator "==" must be on
// the same line.

// p54: If there is no hyphen (-) in the indicator area (column 7) of a line, the last character
// of the preceding line is assumed to be followed by a space.
// If there is a hyphen in the indicator area of a line, the first nonblank character of
// the continuation line immediately follows the last nonblank character of the
// continued line without an intervening space.

// p55: Continuation of alphanumeric and national literals
// Alphanumeric and national literals can be continued only when there are no DBCS
// characters in the content of the literal.
// The following rules apply to alphanumeric and national literals that do not contain
// DBCS characters:
// - If the continued line contains an alphanumeric or national literal without a
//   closing quotation mark, all spaces at the end of the continued line (through
//   column 72) are considered to be part of the literal. The continuation line must
//   contain a hyphen in the indicator area, and the first nonblank character must be
//   a quotation mark. The continuation of the literal begins with the character
//   immediately following the quotation mark.
// - If an alphanumeric or national literal that is to be continued on the next line has
//   as its last character a quotation mark in column 72, the continuation line must
//   start with two consecutive quotation marks. This will result in a single quotation
//   mark as part of the value of the literal.
// - If the last character on the continued line of an alphanumeric or national literal
//   is a single quotation mark in Area B, the continuation line can start with a single
//   quotation mark. This will result in two consecutive literals instead of one
//   continued literal.
// The rules are the same when an apostrophe is used instead of a quotation mark in
// delimiters.
// If you want to continue a literal such that the continued lines and the continuation
// lines are part of one literal:
// - Code a hyphen in the indicator area of each continuation line.
// - Code the literal value using all columns of each continued line, up to and
//   including column 72. (Do not terminate the continued lines with a single
//   quotation mark followed by a space.)
// - Code a quotation mark before the first character of the literal on each
//   continuation line.
// - Terminate the last continuation line with a single quotation mark followed by a
//   space.
// ... p55 -> p56: examples of continuations and expected behavior ...

// -- 2.3 Comment lines -- 

// p56: Comment lines
// A comment line is any line with an asterisk (*) or slash (/) in the indicator area
// (column 7) of the line, or with a floating comment indicator (*>) as the first
// character-string in the program text area (Area A plus Area B).
// The comment can be written anywhere in the program text area of that line, and
// can consist of any combination of characters from the character set of the
// computer.
// Comment lines can be placed anywhere in a program, method, or class definition.
// Comment lines placed before the IDENTIFICATION DIVISION header must follow
// any control cards (for example, PROCESS or CBL).
// Important: Comments intermixed with control cards could nullify some of the
// control cards and cause them to be diagnosed as errors.
// Multiple comment lines are allowed. Each must begin with an asterisk (*) or a
// slash (/) in the indicator area, or with a floating comment indicator (*>).
// For more information about floating comment indicators, see “Floating comment
// indicators (*>). ?
// An asterisk (*) comment line is printed on the next available line in the output
// listing. The effect can be dependent on the LINECOUNT compiler option. For
// information about the LINECOUNT compiler option, see LINECOUNT in the
// Enterprise COBOL Programming Guide. A slash (/) comment line is printed on the
// first line of the next page, and the current page of the output listing is ejected.
// The compiler treats a comment line as documentation, and does not check it
// syntactically.

// p57: A floating comment indicator indicates a comment line if it is the first character
// string in the program-text area (Area A plus Area B), or indicates an inline
// comment if it is after one or more character strings in the program-text area.

// -- 2.4 Debugging lines -- 

// p57: Debugging lines
// A debugging line is any line with a D (or d) in the indicator area of the line.
// Debugging lines can be written in the ENVIRONMENT DIVISION (after the
// OBJECT-COMPUTER paragraph), the DATA DIVISION, and the PROCEDURE
// DIVISION. If a debugging line contains only spaces in Area A and Area B, it is
// considered a blank line.
// See "WITH DEBUGGING MODE" in “SOURCE-COMPUTER paragraph ? on page
// 110.

// p110: WITH DEBUGGING MODE
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

// -- 2.5 Blank lines --

// p58: Blank lines
// A blank line contains nothing but spaces in column 7 through column 72. A blank
// line can be anywhere in a program.

// *************
// 3. Tokenizing
// *************

// -- 3.1 Separators --

// p9: A character-string is a character or a sequence of contiguous characters that forms a
// COBOL word, a literal, a PICTURE character-string, or a comment-entry. A
// character-string is delimited by separators.
// A separator is a string of contiguous characters used to delimit character strings.
// Separators are described in detail under Chapter 4, “Separators, ? on page 45.
// Character strings and certain separators form text words. A text word is a character
// or a sequence of contiguous characters (possibly continued across lines) between
// character positions 8 and 72 inclusive in source text, library text, or pseudo-text.
// For more information about pseudo-text, see “Pseudo-text ? on page 58.

// p45: A separator is a character or a string of two or more contiguous characters that
// delimits character-strings.
// The separators are shown in the following table. 
// ... Table 4. Separators ...

// p45: Anywhere a space is used as a separator or as part of a
// separator, more than one space can be used.
// A space can immediately precede or follow any separator except:
// - The opening pseudo-text delimiter, where the preceding space is
//   required.
// - Within quotation marks. Spaces between quotation marks are considered
//   part of the alphanumeric literal; they are not considered separators.

// p46: A separator comma is composed of a comma followed by a space. A
// separator period is composed of a period followed by a space. A separator
// semicolon is composed of a semicolon followed by a space.

// p46: The separator period must be used only to indicate the end of a sentence,
// or as shown in formats.
// - In the IDENTIFICATION DIVISION, each paragraph must end with a
//   separator period.
// - In the ENVIRONMENT DIVISION, the SOURCE-COMPUTER,
//   OBJECT-COMPUTER, SPECIAL-NAMES, and I-O-CONTROL
//   paragraphs must each end with a separator period. In the
//   FILE-CONTROL paragraph, each file-control entry must end with a
//   separator period.
// - In the DATA DIVISION, file (FD), sort/merge file (SD), and data
//   description entries must each end with a separator period.
// - In the PROCEDURE DIVISION, each sentence and each procedure must end with a
//   separator period.

// p46: The separator comma and separator semicolon can
// be used anywhere the separator space is used.
// - In the PROCEDURE DIVISION, separator commas or separator
//   semicolons can separate statements within a sentence and operands
//   within a statement. 

// p46: Except in pseudo-text, parentheses can appear only in balanced pairs of left
// and right parentheses. They delimit subscripts, a list of function
// arguments, reference-modifiers, arithmetic expressions, or conditions.

// p46: Alphanumeric Literals 
//   Quotation marks {"} ... {"}
//   Apostrophes {’} ... {’}
//   Null-terminated literal delimiters {Z"} ... {"}, {Z’} ... {’}
//   DBCS literal delimiters {G"} ... {"}, {G’} ... {’}, {N"} ... {"}, {N’} ... {’}
//   (-> N" and N’ are DBCS literal delimiters when the NSYMBOL(DBCS) compiler option is in effect)
//   National literal delimiters {N"} ... {"}, {N’} ... {’}, {NX"} ... {"}, {NX’} ... {’}
// The opening delimiter must be immediately preceded by a space or a left parenthesis.
// The closing delimiter must be immediately followed by a separator 
// space, comma, semicolon, period, right parenthesis, or pseudo-text delimiter. 
// Delimiters must appear as balanced pairs.
// They delimit alphanumeric literals, except when the literal is continued
// (see “Continuation lines ? on page 54).

// p47: Pseudo-text delimiters {b==} ... {==b}
// An opening pseudo-text delimiter must be immediately preceded by a
// space. A closing pseudo-text delimiter must be immediately followed by a
// separator space, comma, semicolon, or period. Pseudo-text delimiters must
// appear as balanced pairs. They delimit pseudo-text. (See “COPY statement ?
// on page 530.)

// p47: Any punctuation character included in a PICTURE character-string, a comment
// character-string, or an alphanumeric literal is not considered a punctuation
// character, but is part of the character-string or literal.

// -- 3.2 COBOL words --

// p9: A COBOL word is a character-string that forms a user-defined word, a
// system-name, or a reserved word.

// p9: The classification of a specific occurrence of a COBOL word is
// determined by the context of the clause or phrase in which it occurs.
// * Cobol word * (p10, p12, p13-15, p16, p34)				
//	User-defined words			
//		udw numeric		
//			Level number	
//			Priority number	
//		udw single byte		
//			Library name	
//			Object-oriented class name	
//			Program name	
//			Text name	
//		udw DBCS allowed		
//			Alphabet name	
//			Class name (of data)	
//			Condition name	
//			Data name	
//			File name	
//			Index name	
//			Mnemonic name	
//			Paragraph name	
//			Record name	
//			Section name	
//			Symbolic character	
//			XML-schema name	
//	System name			
//		sn single byte		
//			Language name	
//			Implementor name	
//				Environment name
//				External class name
//				External fileid
//				Assignment name
//		sn DBCS allowed		 
//			Computer name	
//      Function name
//	Reserved word			
//		Keyword		
//		Optional word		
//		Figurative constant		
//		Special character word		
//			Arithmetic operators	
//			Relational operators	
//		Special object identifier		
//		Special register	

// p9: Except for arithmetic operators and relation characters, each character of a COBOL
// word is selected from the following set:
// - Latin uppercase letters A through Z
// - Latin lowercase letters a through z
// - digits 0 through 9
// - - (hyphen)
// - _ (underscore)
// The hyphen cannot appear as the first or last character in such words. The
// underscore cannot appear as the first character in such words. Most user-defined
// words (all except section-names, paragraph-names, priority-numbers, and
// level-numbers) must contain at least one alphabetic character. 

// ... p10 : more rules for User-defined words with DBCS characters ...
// DBCS user-defined words begin with a shift-out character and end with a
// shift-in character. 
// DBCS user-defined words can contain only double-byte characters.
// Maximum length : 14 characters.
// DBCS-encoded uppercase and lowercase letters are not equivalent.
// Words formed with DBCS characters cannot be continued across lines.

// p11: The maximum length of a user-defined word is 30 bytes, except for level-numbers
// and priority-numbers. Level-numbers and priority numbers must each be a
// one-digit or two-digit integer.

// p12: A system-name is a character string that has a specific meaning to the system.

// p12: A function-name specifies the mechanism provided to determine the value of an
// intrinsic function.

// p12: A reserved word is a character-string with a predefined meaning in a COBOL source
// unit.

// p13: Keywords
// Keywords are reserved words that are required within a given clause,
// entry, or statement. Within each format, such words appear in uppercase
// on the main path.
// Optional words
// Optional words are reserved words that can be included in the format of a
// clause, entry, or statement in order to improve readability. They have no
// effect on the execution of the program.
// Special character words
// There are two types of special character words, which are recognized as
// special characters only when represented in single-byte characters:
// - Arithmetic operators: + - / * **
// See “Arithmetic expressions ? on page 253.
// - Relational operators: <>=<=>=
// See “Conditional expressions ? on page 256.
// Special object identifiers
// COBOL provides two special object identifiers, SELF and SUPER:
// ... p13 more details on SELF and SUPER ...

// p13: Figurative constants are reserved words that name and refer to specific constant
// values.
// ... p13-p15: more details on all the figurative constants ..
// The singular and plural forms of NULL, ZERO, SPACE, HIGH-VALUE,
// LOW-VALUE, and QUOTE can be used interchangeably. 
// When the rules of COBOL permit any one spelling of a figurative constant name,
// any alternative spelling of that figurative constant name can be specified.
// You can use a figurative constant wherever literal appears in a syntax diagram,
// except where explicitly prohibited. When a numeric literal appears in a syntax
// diagram, only the figurative constant ZERO (or ZEROS or ZEROES) can be used.
// Figurative constants are not allowed as function arguments except in an arithmetic
// expression, where the expression is an argument to a function.
// The length of a figurative constant depends on the context of its use. The following
// rules apply:
// - When a figurative constant is specified in a VALUE clause or associated with a
//   data item (for example, when it is moved to or compared with another item), the
//   length of the figurative constant character-string is equal to 1 or the number of
//   character positions in the associated data item, whichever is greater.
// - When a figurative constant, other than the ALL literal, is not associated with
//   another data item (for example, in a CALL, INVOKE, STOP, STRING, or
//   UNSTRING statement), the length of the character-string is one character.
// * ALL literal
// literal can be an alphanumeric literal, a DBCS literal, a national literal, or a
// figurative constant other than the ALL literal.
// When literal is not a figurative constant, ALL literal represents one or more
// occurrences of the string of characters that compose the literal.
// When literal is a figurative constant, the word ALL has no meaning and is
// used only for readability.
// The figurative constant ALL literal must not be used with the CALL,
// INSPECT, INVOKE, STOP, or STRING statements.
// * symbolic-character
// Represents one or more of the characters specified as a value of the
// symbolic-character in the SYMBOLIC CHARACTERS clause of the
// SPECIAL-NAMES paragraph.
// symbolic-character always represents an alphanumeric character; it can be
// used in a context that requires a national character only when implicit
// conversion of alphanumeric to national characters is defined. (It can be
// used, for example, in a MOVE statement where the receiving item is of
// class national because implicit conversion is defined when the sending
// item is alphanumeric and the receiving item is national.)

// p16: Special registers are reserved words that name storage areas generated by the
// compiler. Their primary use is to store information produced through specific
// COBOL features. Each such storage area has a fixed name, and must not be
// defined within the program.
// ... p16-p17 : more details on all special registers ...
// Unless otherwise explicitly restricted, a special register can be used wherever a
// data-name or identifier that has the same definition as the implicit definition of the
// special register can be used. Implicit definitions, if applicable, are given in the
// specification of each special register.
// You can specify an alphanumeric special register in a function wherever an
// alphanumeric argument to a function is allowed, unless specifically prohibited.
// If qualification is allowed, special registers can be qualified as necessary to provide
// uniqueness. (For more information, see “Qualification ? on page 65.)
// ... p17-p33 : more details on all special registers ...

// Scope and comparison

// p9: In COBOL words (but not in the content of alphanumeric, DBCS, and national
// literals), each lowercase single-byte alphabetic letter is considered to be equivalent
// to its corresponding single-byte uppercase alphabetic letter.

// p9: The following rules apply for all COBOL words:
// - A reserved word cannot be used as a user-defined word or as a system-name.
// - The same COBOL word, however, can be used as both a user-defined word and
//   as a system-name. 

// p9: Priority numbers and
// level numbers need not be unique; a given specification of a priority-number or
// level-number can be identical to any other priority-number or level-number.

// p11: A given user-defined word can belong to only one of these sets, except that a given
// number can be both a priority-number and a level-number. Each user-defined
// word within a set must be unique, except for priority-numbers and level-numbers
// and except as specified in Chapter 8, “Referencing data names, copy libraries, and
// PROCEDURE DIVISION names, ? on page 65.

// p11: The following types of user-defined words can be referenced by statements and
// entries in the program in which the user-defined word is declared:
// - Paragraph-name
// - Section-name
// The following types of user-defined words can be referenced by any COBOL
// program, provided that the compiling system supports the associated library or
// other system and that the entities referenced are known to that system:
// - Library-name
// - Text-name
// The following types of names, when they are declared within a configuration
// section, can be referenced by statements and entries in the program that contains
// the configuration section or in any program contained within that program:
// - Alphabet-name
// - Class-name
// - Condition-name
// - Mnemonic-name
// - Symbolic-character
// - XML-schema-name

// p12: A function-name specifies the mechanism provided to determine the value of an
// intrinsic function.
// The same word, in a different context, can appear in a program as a user-defined
// word or a system-name. For a list of function-names and their definitions, see
// Table 51 on page 485.

// -- 3.3 Literals --

// p33: A literal is a character-string whose value is specified either by the characters of
// which it is composed or by the use of a figurative constant.
// For more information about figurative constants, see “Figurative constants ? on
// page 13.

// p34: Alphanumeric literals
// Enterprise COBOL provides several formats of alphanumeric literals.
// The formats of alphanumeric literals are:
// - Format 1: “Basic alphanumeric literals ?
// - Format 2: “Alphanumeric literals with DBCS characters ? on page 35
// - Format 3: “Hexadecimal notation for alphanumeric literals ? on page 36
// - Format 4: “Null-terminated alphanumeric literals ? on page 37

// p34: Basic alphanumeric literals
// Basic alphanumeric literals can contain any character in a single-byte EBCDIC
// character set.
// The following format is for a basic alphanumeric literal:
// Format 1: Basic alphanumeric literals
// "single-byte-characters"
//’ single-byte-characters’
// The enclosing quotation marks or apostrophes are excluded from the literal when
// the program is compiled.
// An embedded quotation mark or apostrophe must be represented by a pair of
// quotation marks ("") or a pair of apostrophes (’’), respectively, when it is the
// character used as the opening delimiter. For example:
// "THIS ISN""T WRONG"
//’ THIS ISN’’T WRONG’
// The delimiter character used as the opening delimiter for a literal must be used as
// the closing delimiter for that literal. For example:
// ’THIS IS RIGHT’
// "THIS IS RIGHT"
// ’THIS IS WRONG"
// You can use apostrophes or quotation marks as the literal delimiters independent
// of the APOST/QUOTE compiler option.
// Any punctuation characters included within an alphanumeric literal are part of the
// value of the literal.
// The maximum length of an alphanumeric literal is 160 bytes. The minimum length
// is 1 byte.
// Alphanumeric literals are in the alphanumeric data class and category. (Data
// classes and categories are described in “Classes and categories of data ? on page
// 162.)

// p35: Alphanumeric literals with DBCS characters
// When the DBCS compiler option is in effect, the characters X'0E' and X'0F' in an
// alphanumeric literal will be recognized as shift codes for DBCS characters. That is,
// the characters between paired shift codes will be recognized as DBCS characters.
// Unlike an alphanumeric literal compiled under the NODBCS option, additional
// syntax rules apply to DBCS characters in an alphanumeric literal.
// Alphanumeric literals with DBCS characters have the following format:
// Format 2: Alphanumeric literals with DBCS characters
// "mixed-SBCS-and-DBCS-characters"
// ’mixed-SBCS-and-DBCS-characters’
// " or ’ The opening and closing delimiter. The closing delimiter must match the
// opening delimiter.
// mixed-SBCS-and-DBCS-characters
// Any mix of single-byte and DBCS characters.
// Shift-out and shift-in control characters are part of the literal and must be
// paired. They must contain zero or an even number of intervening bytes.
// Nested shift codes are not allowed in the DBCS portion of the literal.
// The syntax rules for single-byte characters in the literal follow the rules for
// basic alphanumeric literals. The syntax rules for DBCS characters in the
// literal follow the rules for DBCS literals.
// The move and comparison rules for alphanumeric literals with DBCS characters
// are the same as those for any alphanumeric literal.
// The length of an alphanumeric literal with DBCS characters is its byte length,
// including the shift control characters. The maximum length is limited by the
// available space on one line in Area B. An alphanumeric literal with DBCS
// characters cannot be continued.
// An alphanumeric literal with DBCS characters is of the alphanumeric category.
// Alphanumeric literals with DBCS characters cannot be used:
// ... p35-p36 : more details of the cases when a DBCS literal cannot be used ...
// Enterprise COBOL statements process alphanumeric literals with DBCS characters
// without sensitivity to the shift codes and character codes. The use of statements
// that operate on a byte-to-byte basis (for example, STRING and UNSTRING) can
// result in strings that are not valid mixtures of single-byte EBCDIC and DBCS
// characters. See Processing alphanumeric data items that contain DBCS data in the
// Enterprise COBOL Programming Guide for more information about using
// alphanumeric literals and data items with DBCS characters in statements that
// operate on a byte-by-byte basis.

// p36: Hexadecimal notation for alphanumeric literals
// Hexadecimal notation can be used for alphanumeric literals.
// Hexadecimal notation has the following format:
// Format 3: Hexadecimal notation for alphanumeric literals
// X"hexadecimal-digits"
// X’hexadecimal-digits’
// X" or X’
// The opening delimiter for the hexadecimal notation of an alphanumeric
// literal.
// " or ’ The closing delimiter for the hexadecimal notation of an alphanumeric
// literal. If a quotation mark is used in the opening delimiter, a quotation
// mark must be used as the closing delimiter. Similarly, if an apostrophe is
// used in the opening delimiter, an apostrophe must be used as the closing
// delimiter.
// Hexadecimal digits are characters in the range '0' to '9', 'a' to 'f', and 'A' to 'F',
// inclusive. Two hexadecimal digits represent one character in a single-byte character
// set (EBCDIC or ASCII). Four hexadecimal digits represent one character in a DBCS
// character set. A string of EBCDIC DBCS characters represented in hexadecimal
// notation must be preceded by the hexadecimal representation of a shift-out control
// character (X'0E') and followed by the hexadecimal representation of a shift-in
// control character (X'0F'). An even number of hexadecimal digits must be specified.
// The maximum length of a hexadecimal literal is 320 hexadecimal digits.
// The continuation rules are the same as those for any alphanumeric literal. The
// opening delimiter (X" or X’) cannot be split across lines.
// The DBCS compiler option has no effect on the processing of hexadecimal notation
// of alphanumeric literals.
// An alphanumeric literal in hexadecimal notation has data class and category
// alphanumeric. Hexadecimal notation for alphanumeric literals can be used
// anywhere alphanumeric literals can be used.
// See also “Hexadecimal notation for national literals ? on page 41.

// p37: Null-terminated alphanumeric literals
// Alphanumeric literals can be null-terminated.
// The format for null-terminated alphanumeric literals is:
// Format 4: Null-terminated alphanumeric literals
// Z"mixed-characters"
// Z’mixed-characters’
// Z" or Z’
// The opening delimiter for a null-terminated alphanumeric literal. Both
// characters of the opening delimiter (Z" or Z’) must be on the same source
// line.
// " or ’ The closing delimiter for a null-terminated alphanumeric literal.
// If a quotation mark is used in the opening delimiter, a quotation mark
// must be used as the closing delimiter. Similarly, if an apostrophe is used in
// the opening delimiter, an apostrophe must be used as the closing delimiter.
// mixed-characters
// Can be any of the following characters:
// - Solely single-byte characters
// - Mixed single-byte and DBCS characters
// - Solely DBCS characters
// However, you cannot specify the single-byte character with the value X'00'.
// X'00' is the null character automatically appended to the end of the literal.
// The content of the literal is otherwise subject to the same rules and
// restrictions as an alphanumeric literal with DBCS characters (format 2).
// The length of the string of characters in the literal content can be 0 to 159 bytes.
// The actual length of the literal includes the terminating null character, and is a
// maximum of 160 bytes.
// A null-terminated alphanumeric literal has data class and category alphanumeric.
// It can be used anywhere an alphanumeric literal can be used except that
// null-terminated literals are not supported in ALL literal figurative constants.
// The LENGTH intrinsic function, when applied to a null-terminated literal, returns
// the number of bytes in the literal prior to but not including the terminating null.
// (The LENGTH special register does not support literal operands.)

// p37: Numeric literals
// A numeric literal is a character-string whose characters are selected from the digits 0
// through 9, a sign character (+ or -), and the decimal point.
// If the literal contains no decimal point, it is an integer. (In this documentation, the
// word integer appearing in a format represents a numeric literal of nonzero value
// that contains no sign and no decimal point, except when other rules are included
// with the description of the format.) The following rules apply:
// - If the ARITH(COMPAT) compiler option is in effect, one through 18 digits are
//   allowed. If the ARITH(EXTEND) compiler option is in effect, one through 31
//   digits are allowed.
// - Only one sign character is allowed. If included, it must be the leftmost character
//   of the literal. If the literal is unsigned, it is a positive value.
// - Only one decimal point is allowed. If a decimal point is included, it is treated as
//   an assumed decimal point (that is, as not taking up a character position in the
//   literal). The decimal point can appear anywhere within the literal except as the
//   rightmost character.
// The value of a numeric literal is the algebraic quantity expressed by the characters
// in the literal. The size of a numeric literal is equal to the number of digits specified
// by the user.
// Numeric literals can be fixed-point or floating-point numbers.
// Numeric literals are in the numeric data class and category. (Data classes and
// categories are described under “Classes and categories of data ? on page 162.)

// p38: Rules for floating-point literal values
// The format and rules for floating-point literals are listed below.
// Format : ('+' | '-')? mantissa 'E' ('+' | '-')? exponent
// - The sign is optional before the mantissa and the exponent; if you omit the sign,
//   the compiler assumes a positive number.
// - The mantissa can contain between one and 16 digits. A decimal point must be
//   included in the mantissa.
// - The exponent is represented by an E followed by an optional sign and one or
//   two digits.
// - The magnitude of a floating-point literal value must fall between 0.54E-78 and
//   0.72E+76. For values outside of this range, an E-level diagnostic message is
//   produced and the value is replaced by either 0 or 0.72E+76, respectively.

// p38: DBCS literals
// The formats and rules for DBCS literals are listed in this section.
// Format for DBCS literals
// G"<DBCS-characters>"
// G’<DBCS-characters>’
// N"<DBCS-characters>"
// N’<DBCS-characters>’
// G", G’, N", or N’
// Opening delimiters.
// N" and N’ identify a DBCS literal when the NSYMBOL(DBCS) compiler
// option is in effect. They identify a national literal when the
// NSYMBOL(NATIONAL) compiler option is in effect, and the rules
// specified in “National literals ? on page 40 apply.
// The opening delimiter must be followed immediately by a shift-out control
// character.
// For literals with opening delimiter N" or N’, when embedded quotes or
// apostrophes are specified as part of DBCS characters in a DBCS literal, a
// single embedded DBCS quote or apostrophe is represented by two DBCS
// quotes or apostrophes. If a single embedded DBCS quote or apostrophe is
// found, an E-level compiler message will be issued and a second embedded
// DBCS quote or apostrophe will be assumed.
// < Represents the shift-out control character (X'0E')
// > Represents the shift-in control character (X'0F')
// " or ’ The closing delimiter. If a quotation mark is used in the opening delimiter,
// a quotation mark must be used as the closing delimiter. Similarly, if an
// apostrophe is used in the opening delimiter, an apostrophe must be used
// as the closing delimiter.
// The closing delimiter must appear immediately after the shift-in control
// character.
// DBCS-characters
// DBCS-characters can be one or more characters in the range of X'00'
// through X'FF' for either byte. Any value will be accepted in the content of
// the literal, although whether it is a valid value at run time depends on the
// CCSID in effect for the CODEPAGE compiler option.
// Maximum length
// 28 characters
// Continuation rules
// Cannot be continued across lines 

// ... p39: Where DBCS literals can be used ...

// p40: National literals
// Basic national literals
// The format and rules for basic national literals are listed in this section.
// Format 1: Basic national literals
// N"character-data"
// N’character-data’
// When the NSYMBOL(NATIONAL) compiler option is in effect, the opening
// delimiter N" or N’ identifies a national literal. A national literal is of the class and
// category national.
// When the NSYMBOL(DBCS) compiler option is in effect, the opening delimiter N"
// or N’ identifies a DBCS literal, and the rules specified in “DBCS literals ? on page
// 38 apply.
// N" or N’
// Opening delimiters. The opening delimiter must be coded as single-byte
// characters. It cannot be split across lines.
// " or ’ The closing delimiter. The closing delimiter must be coded as a single-byte
// character. If a quotation mark is used in the opening delimiter, it must be
// used as the closing delimiter. Similarly, if an apostrophe is used in the
// opening delimiter, it must be used as the closing delimiter.
// To include the quotation mark or apostrophe used in the opening delimiter
// in the content of the literal, specify a pair of quotation marks or
// apostrophes, respectively. Examples:
// N’This literal’’s content includes an apostrophe’
// N’This literal includes ", which is not used in the opening delimiter’
// N"This literal includes "", which is used in the opening delimiter"
// character-data
// The source text representation of the content of the national literal.
// character-data can include any combination of EBCDIC single-byte
// characters and double-byte characters encoded in the Coded Character Set
// ID (CCSID) specified by the CODEPAGE compiler option.
// DBCS characters in the content of the literal must be delimited by shift-out
// and shift-in control characters.
// Maximum length
// The maximum length of a national literal is 80 character positions,
// excluding the opening and closing delimiters. If the source content of the
// literal contains one or more DBCS characters, the maximum length is
// limited by the available space in Area B of a single source line.
// The literal must contain at least one character. Each single-byte character in
// the literal counts as one character position and each DBCS character in the
// literal counts as one character position. Shift-in and shift-out delimiters for
// DBCS characters are not counted.
// Continuation rules
// When the content of the literal includes DBCS characters, the literal cannot
// be continued. When the content of the literal does not include DBCS
// characters, normal continuation rules apply.
// The source text representation of character-data is automatically converted to
// UTF-16 for use at run time (for example, when the literal is moved to or compared
// with a data item of category national).

// p41: Hexadecimal notation for national literals
// The format and rules for the hexadecimal notation format of national literals are
// listed in this section.
// Format 2: Hexadecimal notation for national literals
// NX"hexadecimal-digits"
// NX’hexadecimal-digits’
// The hexadecimal notation format of national literals is not affected by the
// NSYMBOL compiler option.
// NX" or NX’
// Opening delimiters. The opening delimiter must be represented in
// single-byte characters. It must not be split across lines.
// " or ’ The closing delimiter. The closing delimiter must be represented as a
// single-byte character.
// If a quotation mark is used in the opening delimiter, a quotation mark
// must be used as the closing delimiter. Similarly, if an apostrophe is used in
// the opening delimiter, an apostrophe must be used as the closing delimiter.
// hexadecimal-digits
// Hexadecimal digits in the range '0' to '9', 'a' - f', and 'A' to 'F', inclusive.
// Each group of four hexadecimal digits represents a single national
// character and must represent a valid code point in UTF-16. The number of
// hexadecimal digits must be a multiple of four.
// Maximum length
// The length of a national literal in hexadecimal notation must be from four
// to 320 hexadecimal digits, excluding the opening and closing delimiters.
// The length must be a multiple of four.
// Continuation rules
// Normal continuation rules apply.
// The content of a national literal in hexadecimal notation is stored as national
// characters. The resulting content has the same meaning as a basic national literal
// that specifies the same national characters.
// A national literal in hexadecimal notation has data class and category national and
// can be used anywhere that a basic national literal can be used. 

// ... p42: Where national literals can be used ...

// -- 3.4 Picture strings --

// p42: A PICTURE character-string is composed of the currency symbol and certain
// combinations of characters in the COBOL character set. PICTURE character-strings
// are delimited only by the separator space, separator comma, separator semicolon,
// or separator period.
// A chart of PICTURE clause symbols appears in Table 12 on page 199.

// LP : the following separators are not delimiters for picture strings :
// Left parenthesis, Right parenthesis, Colon

// LP : context to detect picture strings :
// pictureClause:
//                 (PICTURE | PIC) IS? pictureCharacterString;
// currencySignClause :
//                       CURRENCY SIGN? IS? literal (WITH? PICTURE SYMBOL literal)?;
// -> PICTURE or PIC keyword changes lexer mode
// -> match the following token delimited only by separator space, separator comma, separator semicolon, or separator period
// -> if the token is SYMBOL => exit special mode
// -> if the token is IS => match one more token in special mode and then exit

// p199: character-string
// character-string is made up of certain COBOL characters used as picture
// symbols. The allowable combinations determine the category of the
// elementary data item.
// character-string can contain a maximum of 50 characters.
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

// ... p202 -> p204 : more rules to check that a picture string is valid ...

// -- 3.5 Comment text --

// p42: Comments
// A comment is a character-string that can contain any combination of characters from
// the character set of the computer.
// It has no effect on the execution of the program. There are three forms of
// comments:
// Comment entry (IDENTIFICATION DIVISION)
// This form is described under “Optional paragraphs ? on page 105.
// Comment line (any division)
// This form is described under “Comment lines ? on page 56.
// Inline comments (any division)
// Character-strings that form comments can contain DBCS characters or a
// combination of DBCS and single-byte EBCDIC characters.
// Multiple comment lines that contain DBCS strings are allowed. The embedding of
// DBCS characters in a comment line must be done on a line-by-line basis. Words
// containing those characters cannot be continued to a following line. No syntax
// checking for valid strings is provided in comment lines.

// p56: Comment lines
// A comment line is any line with an asterisk (*) or slash (/) in the indicator area
// (column 7) of the line, or with a floating comment indicator (*>) as the first
// character-string in the program text area (Area A plus Area B).
// The comment can be written anywhere in the program text area of that line, and
// can consist of any combination of characters from the character set of the
// computer.
// Comment lines can be placed anywhere in a program, method, or class definition.
// Comment lines placed before the IDENTIFICATION DIVISION header must follow
// any control cards (for example, PROCESS or CBL).
// Important: Comments intermixed with control cards could nullify some of the
// control cards and cause them to be diagnosed as errors.
// Multiple comment lines are allowed. Each must begin with an asterisk (*) or a
// slash (/) in the indicator area, or with a floating comment indicator (*>).
// For more information about floating comment indicators, see “Floating comment
// indicators (*>). ?
// An asterisk (*) comment line is printed on the next available line in the output
// listing. The effect can be dependent on the LINECOUNT compiler option. For
// information about the LINECOUNT compiler option, see LINECOUNT in the
// Enterprise COBOL Programming Guide. A slash (/) comment line is printed on the
// first line of the next page, and the current page of the output listing is ejected.
// The compiler treats a comment line as documentation, and does not check it
// syntactically.

// p43: An inline comment is identified by a floating comment indicator (*>)
// preceded by one or more character-strings in the program-text area, and
// can be written on any line of a compilation group. All characters that
// follow the floating comment indicator up to the end of area B are comment
// text.
// p57: Floating comment indicators (*>)
// In addition to the fixed indicators that can only be specified in the indicator area of
// the source reference format, a floating comment indicator (*>) can be specified
// anywhere in the program-text area to indicate a comment line or an inline
// comment.
// A floating comment indicator indicates a comment line if it is the first character
// string in the program-text area (Area A plus Area B), or indicates an inline
// comment if it is after one or more character strings in the program-text area.
// These are the rules for floating comment indicators:
// - Both characters (* and >) that form the multiple-character floating indicator must
//   be contiguous and on the same line.
// - The floating comment indicator for an inline comment must be preceded by a
//   separator space, and can be specified wherever a separator space can be
//   specified.
// - All characters following the floating comment indicator up to the end of Area B
//   are comment text.

// p105: Optional paragraphs
// Some optional paragraphs in the IDENTIFICATION DIVISION can be omitted.
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
// The comment-entry in any of the optional paragraphs can be any combination of
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

// LP -> a comment entry is delimited only by characters in area A on the next line

// -- 3.6 Pseudo-text --

// p47: Pseudo-text delimiters {b==} ... {==b}
// An opening pseudo-text delimiter must be immediately preceded by a
// space. A closing pseudo-text delimiter must be immediately followed by a
// separator space, comma, semicolon, or period. Pseudo-text delimiters must
// appear as balanced pairs. They delimit pseudo-text. (See “COPY statement ?
// on page 530.)

// p54 : Both characters that make up the pseudo-text delimiter separator "==" must be on
// the same line.

// p58: Pseudo-text
// The character-strings and separators that comprise pseudo-text can start in either
// Area A or Area B.
// If, however, there is a hyphen in the indicator area (column 7) of a line that follows
// the opening pseudo-text delimiter, Area A of the line must be blank, and the rules
// for continuation lines apply to the formation of text words. See “Continuation
// lines ? on page 54 for details.

// p532: Library text and pseudo-text can consist of or include any words (except
// COPY), identifiers, or literals that can be written in the source text. This
// includes DBCS user-defined words, DBCS literals, and national literals.
// DBCS user-defined words must be wholly formed; that is, there is no
// partial-word replacement for DBCS words.
// Words or literals containing DBCS characters cannot be continued across
// lines.

// -- 3.7 Additional keywords --

// => Reserved words found in the grammar rules but not declared as keywords 
//    in the IBM Cobol language reference v5.1

// - CobolCodeElements -

// ATTRIBUTE : 'ATTRIBUTE'; // found in xmlGenerateStatement
// ATTRIBUTES : 'ATTRIBUTES'; // found in xmlGenerateStatement
// EBCDIC : 'EBCDIC'; // found in alphabet clause
// ELEMENT : 'ELEMENT'; // found in xmlGenerateStatement
// ENCODING : 'ENCODING'; // found in xmlGenerateStatement
// NAME : 'NAME'; // found in xmlGenerateStatement
// NAMESPACE : 'NAMESPACE'; // found in xmlGenerateStatement
// NAMESPACE_PREFIX  : 'NAMESPACE-PREFIX'; // found in xmlGenerateStatement
// NEW : 'NEW'; // found in invokeStatement
// NONNUMERIC : 'NONNUMERIC'; // found in xmlGenerateStatement
// PARSE : 'PARSE'; // found in xmlParseStatement
// SYMBOL : 'SYMBOL'; // found in currencySignClause
// UNBOUNDED : 'UNBOUNDED'; // found in occursClause
// VALIDATING : 'VALIDATING'; // found in xmlParseStatement
// XML_DECLARATION : 'XML-DECLARATION'; // found in xmlGenerateStatement
// YYYYMMDD : 'DATE YYYYMMDD'; // found in acceptStatement
// YYYYDDD : 'DAY YYYYDDD'; // found in acceptStatement

// - CobolCompilerDirectives -

// ASTERISK_CBL : '*CBL'; // found in controlCblStatement
// ASTERISK_CONTROL : '*CONTROL'; // found in controlCblStatement 
// EXEC_SQL_INCLUDE : 'EXEC SQL INCLUDE'; // found in execSqlIncludeStatement
// PROCESS : 'PROCESS'; // found in cblProcessStatement

// - TO DO if necessary : no token SQL-INIT-FLAG has been defined in the scanner yet -

// p433: Precompiler: With the DB2 precompiler, if you pass host variables that might be
// located at different addresses when the program is called more than once, the
// called program must reset SQL-INIT-FLAG. Resetting this flag indicates to DB2 that
// storage must be initialized when the next SQL statement runs. To reset the flag,
// insert the statement MOVE ZERO TO SQL-INIT-FLAG in the PROCEDURE DIVISION of the
// called program ahead of any executable SQL statements that use those host
// variables.
// Coprocessor: With the DB2 coprocessor, the called program does not need to reset
// SQL-INIT-FLAG. An SQL-INIT-FLAG is automatically defined in the program to aid
// program portability. However, statements that modify SQL-INIT-FLAG, such as MOVE
// ZERO TO SQL-INIT-FLAG, have no effect on the SQL processing in the program.

// DB2 11 for zOs - Application Programming and SQL Guide p330:
// If your program uses the DB2 precompiler and uses parameters that are
// defined in LINKAGE SECTION as host variables to DB2 and the address of the
// input parameter might change on subsequent invocations of your program, your
// program must reset the variable SQL-INIT-FLAG. This flag is generated by the
// DB2 precompiler. Resetting this flag indicates that the storage must initialize when
// the next SQL statement executes. To reset the flag, insert the statement MOVE
// ZERO TO SQL-INIT-FLAG in the called program's PROCEDURE DIVISION, ahead
// of any executable SQL statements that use the host variables. If you use the
// COBOL DB2 coprocessor, the called program does not need to reset
// SQL-INIT-FLAG.

// SQL_INIT_FLAG : 'SQL-INIT-FLAG';
