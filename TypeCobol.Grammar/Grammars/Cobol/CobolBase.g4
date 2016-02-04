// IBM Enterprise Cobol 5.1 for zOS

// -----------------------------------------------------------------------
// Common parts of the Cobol grammar used by the Cobol compiler directives
// and by the Cobol code elements (imported by CobolCompilerDirectives.g4 
// and by CobolCodeElements.g4) : tokens, token families and literals.
// -----------------------------------------------------------------------

grammar CobolBase;

// Note : the complete specifications of the COBOL TEXT FORMAT :
//   1. Encoding, 2. Reference format, 3. Tokenizing
// are reproduced in the comments at the end of this grammar file.

// --- COBOL TOKEN TYPES ---

// The token types are recognized by the Scanner before the parsing step.
// All token types names start with a capital letter.

// The grammar rules defined in the Parser match sequences of theses token types.
// All grammar rule names start with a lower case letter.

// IMPORTANT : HOW TO INSERT, REMOVE, or REORDER a TOKEN TYPE in the list of tokens ?

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

tokens 
{ 
    // Separators - Whitespace
    SpaceSeparator,
    CommaSeparator,
    SemicolonSeparator,
    EndOfFile, // <- do not use in this grammar, use constant EOF instead
    // Comments
    FloatingComment,
    CommentLine,
    // Separators - Syntax
    PeriodSeparator,
    ColonSeparator,
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
    IntegerLiteral,
    DecimalLiteral,
    FloatingPointLiteral,
    // Literals - Syntax tokens
    PictureCharacterString,
    CommentEntry,
    ExecStatementText,
    // Symbols    
    FunctionName,
    ExecTranslatorName,
    PartialCobolWord ,
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
    SERVICE,
    SKIP1,
    SKIP2,
    SKIP3,
    TITLE,
    // Keywords - Code element starting tokens
    APPLY,
    CONFIGURATION,
    ELSE,
    ENVIRONMENT,
    FD,
    FILE_CONTROL,
    I_O_CONTROL,
    ID,
    IDENTIFICATION,
    INPUT_OUTPUT,
    LINKAGE,
    LOCAL_STORAGE,
    MULTIPLE,
    OBJECT_COMPUTER,
    REPOSITORY,
    RERUN,
    SAME,
    SD,
    SELECT,
    SOURCE_COMPUTER,
    SPECIAL_NAMES,
    USE,
    WORKING_STORAGE,
    // Keywords - Statement starting tokens
    ACCEPT,
    ADD,
    ALTER,
    CALL,
    CANCEL,
    CLOSE,
    COMPUTE,
    CONTINUE,
    DELETE,
    DISPLAY,
    DIVIDE,
    ENTRY,
    EVALUATE,
    EXEC,
    EXECUTE,
    EXIT,
    GOBACK,
    GO,
    IF,
    INITIALIZE,
    INSPECT,
    INVOKE,
    MERGE,
    MOVE,
    MULTIPLY,
    OPEN,
    PERFORM,
    READ,
    RELEASE,
    RETURN,
    REWRITE,
    SEARCH,
    SET,
    SORT,
    START,
    STOP,
    STRING,
    SUBTRACT,
    UNSTRING,
    WRITE,
    XML,
    // Keywords - Statement ending tokens
    END_ADD,
    END_CALL,
    END_COMPUTE,
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
    DATA,
    DATE,
    DATE_COMPILED,
    DATE_WRITTEN,
    DAY,
    DAY_OF_WEEK,
    DBCS,
    DEBUGGING,
    DECIMAL_POINT,
    DECLARATIVES,
    DELIMITED,
    DELIMITER,
    DEPENDING,
    DESCENDING,
    DISPLAY_ARG,
    DISPLAY_1,
    DIVISION,
    DOWN,
    DUPLICATES,
    DYNAMIC,
    EBCDIC,
    EGCS,
    ELEMENT,
    ENCODING,
    END,
    END_OF_PAGE,
    ENDING,
    ENTRY_ARG,
    EOP,
    EQUAL,
    ERROR,
    EVERY,
    EXCEPTION,
    EXTEND,
    EXTERNAL,
    FACTORY,
    FALSE,
    FILE,
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
    NEXT,
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
    PROCEDURE,
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
    SORT_ARG,
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
    WHEN,
    WITH,
    WORDS,
    WRITE_ONLY,
    XML_DECLARATION,
    XML_SCHEMA,
    YYYYDDD,
    YYYYMMDD
}

// --- COBOL TOKEN FAMILIES ---

// - 1. Numeric literals -

// p37: In this documentation, the word integer appearing in a format represents a numeric literal of nonzero value
// that contains no sign and no decimal point, except when other rules are included
// with the description of the format.

//integer : IntegerLiteral;

// p37: Numeric literals can be fixed-point or floating-point numbers.
// p13: When a numeric literal appears in a syntax
// diagram, only the figurative constant ZERO (or ZEROS or ZEROES) can be used.

numericLiteral: 
                  IntegerLiteral | DecimalLiteral | FloatingPointLiteral | (ZERO | ZEROS | ZEROES);

// - 2. Alphanumeric literals -

// p33: A literal is a character-string whose value is specified either by the characters of
// which it is composed or by the use of a figurative constant.
// p34: The formats of alphanumeric literals are:
// - Format 1: �Basic alphanumeric literals�
// - Format 2: �Alphanumeric literals with DBCS characters� on page 35
// - Format 3: �Hexadecimal notation for alphanumeric literals� on page 36
// - Format 4: �Null-terminated alphanumeric literals� on page 37

alphanumOrHexadecimalLiteral:
                                AlphanumericLiteral |
                                HexadecimalAlphanumericLiteral;

alphanumericLiteralBase:
                           AlphanumericLiteral |
                           HexadecimalAlphanumericLiteral |                      
                           figurativeConstant;

alphanumOrNationalLiteralBase:
                                 (AlphanumericLiteral |
                                  HexadecimalAlphanumericLiteral |                      
                                  NationalLiteral |
                                  HexadecimalNationalLiteral |
                                  DBCSLiteral) |
                                 figurativeConstant;

// p13: You can use a figurative constant wherever literal appears in a syntax diagram,
// except where explicitly prohibited.

figurativeConstant:
	HIGH_VALUE | HIGH_VALUES |
	LOW_VALUE  | LOW_VALUES |
	NULL  | NULLS |
	QUOTE | QUOTES |
	SPACE | SPACES |
	ZERO  | ZEROS  | ZEROES |
	SymbolicCharacter;

// p13: ALL literal
// literal can be an alphanumeric literal, a DBCS literal, a national literal, or a
// figurative constant other than the ALL literal.
// p37: null-terminated literals are not supported in ALL literal figurative constants.

alphanumericLiteral:
           alphanumericLiteralBase |
           NullTerminatedAlphanumericLiteral |
           (ALL alphanumericLiteralBase);

alphanumOrNationalLiteral:
           alphanumOrNationalLiteralBase |
           NullTerminatedAlphanumericLiteral |
           (ALL alphanumOrNationalLiteralBase);

// p534: literal
// Can be numeric, alphanumeric, DBCS, or national.

literal:
           alphanumOrNationalLiteral | numericLiteral;

// - 3. Special registers -

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
// uniqueness. (For more information, see �Qualification� on page 65.)
// ... p17-p33 : more details on all special registers ...

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

// - 4. Reserved words -

// This list of reserved word is useful to parse the COPY REPLACING operands
// -> it should be updated each time a new keyword token is added

reservedWord:
                 keyword | figurativeConstant | specialRegister;

keyword:
// Keywords - Compiler directive starting tokens
    (ASTERISK_CBL | ASTERISK_CONTROL | BASIS | CBL | COPY | DELETE_CD | EJECT |
    ENTER | EXEC_SQL_INCLUDE | INSERT | PROCESS | READY | RESET | REPLACE |
    SERVICE | SKIP1 | SKIP2 | SKIP3 | TITLE |
    // Keywords - Statement starting tokens
    ACCEPT | ADD | ALTER | CALL | CANCEL | CLOSE | COMPUTE | CONTINUE |
    DELETE | DISPLAY | DIVIDE | ENTRY | EVALUATE | EXEC | EXECUTE | EXIT |
    GOBACK | GO | IF | INITIALIZE | INSPECT | INVOKE | MERGE | MOVE |
    MULTIPLY | OPEN | PERFORM | READ | RELEASE | RETURN | REWRITE | SEARCH |
    SET | SORT | START | STOP | STRING | SUBTRACT | UNSTRING | WRITE | XML |
    // Keywords - Statement ending tokens
    END_ADD | END_CALL | END_COMPUTE | END_DELETE | END_DIVIDE | END_EVALUATE |
    END_EXEC | END_IF | END_INVOKE | END_MULTIPLY | END_PERFORM | END_READ |
    END_RETURN | END_REWRITE | END_SEARCH | END_START | END_STRING |
    END_SUBTRACT | END_UNSTRING | END_WRITE | END_XML |
    // Keywords - Special object identifiers
    SELF | SUPER |
    // Keywords - Syntax tokens
    ACCESS | ADVANCING | AFTER | ALL | ALPHABET | ALPHABETIC |
    ALPHABETIC_LOWER | ALPHABETIC_UPPER | ALPHANUMERIC | ALPHANUMERIC_EDITED |
    ALSO | ALTERNATE | AND | ANY | APPLY | ARE | AREA | AREAS | ASCENDING |
    ASSIGN | AT | ATTRIBUTE | ATTRIBUTES | AUTHOR | BEFORE | BEGINNING |
    BINARY | BLANK | BLOCK | BOTTOM | BY | CHARACTER | CHARACTERS | CLASS |
    CLASS_ID | COBOL | CODE | CODE_SET | COLLATING | COM_REG | COMMA | COMMON |
    COMP | COMP_1 | COMP_2 | COMP_3 | COMP_4 | COMP_5 | COMPUTATIONAL |
    COMPUTATIONAL_1 | COMPUTATIONAL_2 | COMPUTATIONAL_3 | COMPUTATIONAL_4 |
    COMPUTATIONAL_5 | CONFIGURATION | CONTAINS | CONTENT | CONVERTING |
    CORR | CORRESPONDING | COUNT | CURRENCY | DATA | DATE | DATE_COMPILED |
    DATE_WRITTEN | DAY | DAY_OF_WEEK | DBCS | DEBUGGING | DECIMAL_POINT |
    DECLARATIVES | DELIMITED | DELIMITER | DEPENDING | DESCENDING |
    DISPLAY_ARG | DISPLAY_1 | DIVISION | DOWN | DUPLICATES | DYNAMIC | EBCDIC |
    EGCS | ELEMENT | ELSE | ENCODING | END | END_OF_PAGE | ENDING | ENTRY_ARG |
    ENVIRONMENT | EOP | EQUAL | ERROR | EVERY | EXCEPTION | EXTEND | EXTERNAL |
    FACTORY | FALSE | FD | FILE | FILE_CONTROL | FILLER | FIRST | FOOTING |
    FOR | FROM | FUNCTION | FUNCTION_POINTER | GENERATE | GIVING | GLOBAL |
    GREATER | GROUP_USAGE | I_O | I_O_CONTROL | ID | IDENTIFICATION | IN |
    INDEX | INDEXED | INHERITS | INITIAL | INPUT | INPUT_OUTPUT | INSTALLATION |
    INTO | INVALID | IS | JUST | JUSTIFIED | KANJI | KEY | LABEL | LEADING |
    LEFT | LESS | LINAGE | LINE | LINES | LINKAGE | LOCAL_STORAGE | LOCK |
    MEMORY | METHOD | METHOD_ID | MODE | MODULES | MORE_LABELS | MULTIPLE |
    NAME | NAMESPACE | NAMESPACE_PREFIX | NATIONAL | NATIONAL_EDITED | NATIVE |
    NEGATIVE | NEW | NEXT | NO | NONNUMERIC | NOT | NUMERIC | NUMERIC_EDITED |
    OBJECT | OBJECT_COMPUTER | OCCURS | OF | OFF | OMITTED | ON | OPTIONAL |
    OR | ORDER | ORGANIZATION | OTHER | OUTPUT | OVERFLOW | OVERRIDE |
    PACKED_DECIMAL | PADDING | PAGE | PARSE | PASSWORD | PIC | PICTURE |
    POINTER | POSITION | POSITIVE | PROCEDURE | PROCEDURE_POINTER |
    PROCEDURES | PROCEED | PROCESSING | PROGRAM | PROGRAM_ID | RANDOM |
    RECORD | RECORDING | RECORDS | RECURSIVE | REDEFINES | REEL | REFERENCE |
    REFERENCES | RELATIVE | RELOAD | REMAINDER | REMOVAL | RENAMES | REPLACING |
    REPOSITORY | RERUN | RESERVE | RETURNING | REVERSED | REWIND | RIGHT |
    ROUNDED | RUN | SAME | SD | SECTION | SECURITY | SEGMENT_LIMIT | SELECT |
    SENTENCE | SEPARATE | SEQUENCE | SEQUENTIAL | SIGN | SIZE | SORT_ARG |
    SORT_MERGE | SOURCE_COMPUTER | SPECIAL_NAMES | SQL | SQLIMS | STANDARD |
    STANDARD_1 | STANDARD_2 | STATUS | SUPPRESS | SYMBOL | SYMBOLIC | SYNC |
    SYNCHRONIZED | TALLYING | TAPE | TEST | THAN | THEN | THROUGH | THRU |
    TIME | TIMES | TO | TOP | TRACE | TRAILING | TRUE | TYPE | UNBOUNDED |
    UNIT | UNTIL | UP | UPON | USAGE | USE | USING | VALIDATING | VALUE |
    VALUES | VARYING | WHEN | WITH | WORDS | WORKING_STORAGE | WRITE_ONLY |
    XML_DECLARATION | XML_SCHEMA | YYYYDDD | YYYYMMDD |
	// Keywords - Parts of special registers
	LENGTH | ADDRESS);

// - 5. Text names -

// For rules on referencing COPY libraries, see �COPY statement� on page 530.
 
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

textName : UserDefinedWord | AlphanumericLiteral;
libraryName : UserDefinedWord | AlphanumericLiteral;


// -----------------
// COBOL TEXT FORMAT
// -----------------

// **********************
// 1. Characters encoding
// **********************

// -- 1.1 Character decoding  at compile time --

// p5: Enterprise COBOL provides the CODEPAGE compiler option for specifying a
// coded character set for use at compile time and run time for code-page-sensitive
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
// See �Continuation lines� on page 54, �Comment lines� on page 56, and
// �Debugging lines� on page 57.
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
// �Declaratives� on page 251.)

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
// the restrictions on the width of Area B. See also Chapter 5, �Sections and
// paragraphs,� on page 49.

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
// period. For more information, see �Level-numbers� on page 186.
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
// For more information about floating comment indicators, see �Floating comment
// indicators (*>).�
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
// See "WITH DEBUGGING MODE" in �SOURCE-COMPUTER paragraph� on page
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
// Separators are described in detail under Chapter 4, �Separators,� on page 45.
// Character strings and certain separators form text words. A text word is a character
// or a sequence of contiguous characters (possibly continued across lines) between
// character positions 8 and 72 inclusive in source text, library text, or pseudo-text.
// For more information about pseudo-text, see �Pseudo-text� on page 58.

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
//   Apostrophes {�} ... {�}
//   Null-terminated literal delimiters {Z"} ... {"}, {Z�} ... {�}
//   DBCS literal delimiters {G"} ... {"}, {G�} ... {�}, {N"} ... {"}, {N�} ... {�}
//   (-> N" and N� are DBCS literal delimiters when the NSYMBOL(DBCS) compiler option is in effect)
//   National literal delimiters {N"} ... {"}, {N�} ... {�}, {NX"} ... {"}, {NX�} ... {�}
// The opening delimiter must be immediately preceded by a space or a left parenthesis.
// The closing delimiter must be immediately followed by a separator 
// space, comma, semicolon, period, right parenthesis, or pseudo-text delimiter. 
// Delimiters must appear as balanced pairs.
// They delimit alphanumeric literals, except when the literal is continued
// (see �Continuation lines� on page 54).

// p47: Pseudo-text delimiters {b==} ... {==b}
// An opening pseudo-text delimiter must be immediately preceded by a
// space. A closing pseudo-text delimiter must be immediately followed by a
// separator space, comma, semicolon, or period. Pseudo-text delimiters must
// appear as balanced pairs. They delimit pseudo-text. (See �COPY statement�
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
// See �Arithmetic expressions� on page 253.
// - Relational operators: <>=<=>=
// See �Conditional expressions� on page 256.
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
// uniqueness. (For more information, see �Qualification� on page 65.)
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
// and except as specified in Chapter 8, �Referencing data names, copy libraries, and
// PROCEDURE DIVISION names,� on page 65.

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
// For more information about figurative constants, see �Figurative constants� on
// page 13.

// p34: Alphanumeric literals
// Enterprise COBOL provides several formats of alphanumeric literals.
// The formats of alphanumeric literals are:
// - Format 1: �Basic alphanumeric literals�
// - Format 2: �Alphanumeric literals with DBCS characters� on page 35
// - Format 3: �Hexadecimal notation for alphanumeric literals� on page 36
// - Format 4: �Null-terminated alphanumeric literals� on page 37

// p34: Basic alphanumeric literals
// Basic alphanumeric literals can contain any character in a single-byte EBCDIC
// character set.
// The following format is for a basic alphanumeric literal:
// Format 1: Basic alphanumeric literals
// "single-byte-characters"
//� single-byte-characters�
// The enclosing quotation marks or apostrophes are excluded from the literal when
// the program is compiled.
// An embedded quotation mark or apostrophe must be represented by a pair of
// quotation marks ("") or a pair of apostrophes (��), respectively, when it is the
// character used as the opening delimiter. For example:
// "THIS ISN""T WRONG"
//� THIS ISN��T WRONG�
// The delimiter character used as the opening delimiter for a literal must be used as
// the closing delimiter for that literal. For example:
// �THIS IS RIGHT�
// "THIS IS RIGHT"
// �THIS IS WRONG"
// You can use apostrophes or quotation marks as the literal delimiters independent
// of the APOST/QUOTE compiler option.
// Any punctuation characters included within an alphanumeric literal are part of the
// value of the literal.
// The maximum length of an alphanumeric literal is 160 bytes. The minimum length
// is 1 byte.
// Alphanumeric literals are in the alphanumeric data class and category. (Data
// classes and categories are described in �Classes and categories of data� on page
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
// �mixed-SBCS-and-DBCS-characters�
// " or � The opening and closing delimiter. The closing delimiter must match the
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
// X�hexadecimal-digits�
// X" or X�
// The opening delimiter for the hexadecimal notation of an alphanumeric
// literal.
// " or � The closing delimiter for the hexadecimal notation of an alphanumeric
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
// opening delimiter (X" or X�) cannot be split across lines.
// The DBCS compiler option has no effect on the processing of hexadecimal notation
// of alphanumeric literals.
// An alphanumeric literal in hexadecimal notation has data class and category
// alphanumeric. Hexadecimal notation for alphanumeric literals can be used
// anywhere alphanumeric literals can be used.
// See also �Hexadecimal notation for national literals� on page 41.

// p37: Null-terminated alphanumeric literals
// Alphanumeric literals can be null-terminated.
// The format for null-terminated alphanumeric literals is:
// Format 4: Null-terminated alphanumeric literals
// Z"mixed-characters"
// Z�mixed-characters�
// Z" or Z�
// The opening delimiter for a null-terminated alphanumeric literal. Both
// characters of the opening delimiter (Z" or Z�) must be on the same source
// line.
// " or � The closing delimiter for a null-terminated alphanumeric literal.
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
// categories are described under �Classes and categories of data� on page 162.)

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
// G�<DBCS-characters>�
// N"<DBCS-characters>"
// N�<DBCS-characters>�
// G", G�, N", or N�
// Opening delimiters.
// N" and N� identify a DBCS literal when the NSYMBOL(DBCS) compiler
// option is in effect. They identify a national literal when the
// NSYMBOL(NATIONAL) compiler option is in effect, and the rules
// specified in �National literals� on page 40 apply.
// The opening delimiter must be followed immediately by a shift-out control
// character.
// For literals with opening delimiter N" or N�, when embedded quotes or
// apostrophes are specified as part of DBCS characters in a DBCS literal, a
// single embedded DBCS quote or apostrophe is represented by two DBCS
// quotes or apostrophes. If a single embedded DBCS quote or apostrophe is
// found, an E-level compiler message will be issued and a second embedded
// DBCS quote or apostrophe will be assumed.
// < Represents the shift-out control character (X'0E')
// > Represents the shift-in control character (X'0F')
// " or � The closing delimiter. If a quotation mark is used in the opening delimiter,
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
// N�character-data�
// When the NSYMBOL(NATIONAL) compiler option is in effect, the opening
// delimiter N" or N� identifies a national literal. A national literal is of the class and
// category national.
// When the NSYMBOL(DBCS) compiler option is in effect, the opening delimiter N"
// or N� identifies a DBCS literal, and the rules specified in �DBCS literals� on page
// 38 apply.
// N" or N�
// Opening delimiters. The opening delimiter must be coded as single-byte
// characters. It cannot be split across lines.
// " or � The closing delimiter. The closing delimiter must be coded as a single-byte
// character. If a quotation mark is used in the opening delimiter, it must be
// used as the closing delimiter. Similarly, if an apostrophe is used in the
// opening delimiter, it must be used as the closing delimiter.
// To include the quotation mark or apostrophe used in the opening delimiter
// in the content of the literal, specify a pair of quotation marks or
// apostrophes, respectively. Examples:
// N�This literal��s content includes an apostrophe�
// N�This literal includes ", which is not used in the opening delimiter�
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
// NX�hexadecimal-digits�
// The hexadecimal notation format of national literals is not affected by the
// NSYMBOL compiler option.
// NX" or NX�
// Opening delimiters. The opening delimiter must be represented in
// single-byte characters. It must not be split across lines.
// " or � The closing delimiter. The closing delimiter must be represented as a
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
// This form is described under �Optional paragraphs� on page 105.
// Comment line (any division)
// This form is described under �Comment lines� on page 56.
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
// For more information about floating comment indicators, see �Floating comment
// indicators (*>).�
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
// appear as balanced pairs. They delimit pseudo-text. (See �COPY statement�
// on page 530.)

// p54 : Both characters that make up the pseudo-text delimiter separator "==" must be on
// the same line.

// p58: Pseudo-text
// The character-strings and separators that comprise pseudo-text can start in either
// Area A or Area B.
// If, however, there is a hyphen in the indicator area (column 7) of a line that follows
// the opening pseudo-text delimiter, Area A of the line must be blank, and the rules
// for continuation lines apply to the formation of text words. See �Continuation
// lines� on page 54 for details.

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

// - !! TO DO if necessary : no token SQL-INIT-FLAG has been defined in the scanner yet -

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