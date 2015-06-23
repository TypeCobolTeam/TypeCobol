using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
    internal static class TokenUtils
    {
        /// <summary>
        /// Total number of token types
        /// </summary>
        public static int MAX_TOKEN_TYPE = 454;

        static TokenUtils()
        {
            // Map token types to token families

            tokenFamilyFromTokenType = new TokenFamily[MAX_TOKEN_TYPE + 1];
            int tokenType = 0;
            for (; tokenType < (int)TokenFamily.Whitespace; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.Invalid;
            }
            for (; tokenType < (int)TokenFamily.Comments; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.Whitespace;
            }
            for (; tokenType < (int)TokenFamily.SyntaxSeparator; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.Comments;
            }
            for (; tokenType < (int)TokenFamily.ArithmeticOperator; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.SyntaxSeparator;
            }
            for (; tokenType < (int)TokenFamily.RelationalOperator; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.ArithmeticOperator;
            }
            for (; tokenType < (int)TokenFamily.AlphanumericLiteral; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.RelationalOperator;
            }
            for (; tokenType < (int)TokenFamily.NumericLiteral; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.AlphanumericLiteral;
            }
            for (; tokenType < (int)TokenFamily.SyntaxLiteral; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.NumericLiteral;
            }
            for (; tokenType < (int)TokenFamily.Symbol; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.SyntaxLiteral;
            }
            for (; tokenType < (int)TokenFamily.CompilerDirectiveStartingKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.Symbol;
            }
            for (; tokenType < (int)TokenFamily.CodeElementStartingKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.CompilerDirectiveStartingKeyword;
            }
            for (; tokenType < (int)TokenFamily.StatementStartingKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.CodeElementStartingKeyword;
            }
            for (; tokenType < (int)TokenFamily.StatementEndingKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.StatementStartingKeyword;
            }
            for (; tokenType < (int)TokenFamily.SpecialRegisterKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.StatementEndingKeyword;
            }
            for (; tokenType < (int)TokenFamily.FigurativeConstantKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.SpecialRegisterKeyword;
            }
            for (; tokenType < (int)TokenFamily.SpecialObjetIdentifierKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.FigurativeConstantKeyword;
            }
            for (; tokenType < (int)TokenFamily.SyntaxKeyword; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.SpecialObjetIdentifierKeyword;
            }
            for (; tokenType < (int)TokenFamily.CompilerDirective; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.SyntaxKeyword;
            }
            for (; tokenType < (int)TokenFamily.InternalTokenGroup; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.CompilerDirective;
            }
            for (; tokenType < (int)MAX_TOKEN_TYPE; tokenType++)
            {
                tokenFamilyFromTokenType[tokenType] = TokenFamily.InternalTokenGroup;
            }

            // Register the token strings corresponding to each token type (for keywords only)

            tokenStringFromTokenType = new string[MAX_TOKEN_TYPE + 1];
            tokenStringFromTokenType[(int)TokenType.ASTERISK_CBL] = "*CBL";
            tokenStringFromTokenType[(int)TokenType.ASTERISK_CONTROL] = "*CONTROL";
            tokenStringFromTokenType[(int)TokenType.BASIS] = "BASIS";
            tokenStringFromTokenType[(int)TokenType.CBL] = "CBL";
            tokenStringFromTokenType[(int)TokenType.COPY] = "COPY";
            tokenStringFromTokenType[(int)TokenType.DELETE_CD] = "DELETE";
            tokenStringFromTokenType[(int)TokenType.EJECT] = "EJECT";
            tokenStringFromTokenType[(int)TokenType.ENTER] = "ENTER";
            tokenStringFromTokenType[(int)TokenType.EXEC_SQL_INCLUDE] = null;
            tokenStringFromTokenType[(int)TokenType.INSERT] = "INSERT";
            tokenStringFromTokenType[(int)TokenType.PROCESS] = "PROCESS";
            tokenStringFromTokenType[(int)TokenType.READY] = "READY";
            tokenStringFromTokenType[(int)TokenType.RESET] = "RESET";
            tokenStringFromTokenType[(int)TokenType.REPLACE] = "REPLACE";
            tokenStringFromTokenType[(int)TokenType.SERVICE] = "SERVICE";
            tokenStringFromTokenType[(int)TokenType.SKIP1] = "SKIP1";
            tokenStringFromTokenType[(int)TokenType.SKIP2] = "SKIP2";
            tokenStringFromTokenType[(int)TokenType.SKIP3] = "SKIP3";
            tokenStringFromTokenType[(int)TokenType.TITLE] = "TITLE";
            tokenStringFromTokenType[(int)TokenType.ACCEPT] = "ACCEPT";
            tokenStringFromTokenType[(int)TokenType.ADD] = "ADD";
            tokenStringFromTokenType[(int)TokenType.ALTER] = "ALTER";
            tokenStringFromTokenType[(int)TokenType.CALL] = "CALL";
            tokenStringFromTokenType[(int)TokenType.CANCEL] = "CANCEL";
            tokenStringFromTokenType[(int)TokenType.CLOSE] = "CLOSE";
            tokenStringFromTokenType[(int)TokenType.COMPUTE] = "COMPUTE";
            tokenStringFromTokenType[(int)TokenType.CONTINUE] = "CONTINUE";
            tokenStringFromTokenType[(int)TokenType.DELETE] = "DELETE";
            tokenStringFromTokenType[(int)TokenType.DISPLAY] = "DISPLAY";
            tokenStringFromTokenType[(int)TokenType.DIVIDE] = "DIVIDE";
            tokenStringFromTokenType[(int)TokenType.ENTRY] = "ENTRY";
            tokenStringFromTokenType[(int)TokenType.EVALUATE] = "EVALUATE";
            tokenStringFromTokenType[(int)TokenType.EXEC] = "EXEC";
            tokenStringFromTokenType[(int)TokenType.EXECUTE] = "EXECUTE";
            tokenStringFromTokenType[(int)TokenType.EXIT] = "EXIT";
            tokenStringFromTokenType[(int)TokenType.GOBACK] = "GOBACK";
            tokenStringFromTokenType[(int)TokenType.GO] = "GO";
            tokenStringFromTokenType[(int)TokenType.IF] = "IF";
            tokenStringFromTokenType[(int)TokenType.INITIALIZE] = "INITIALIZE";
            tokenStringFromTokenType[(int)TokenType.INSPECT] = "INSPECT";
            tokenStringFromTokenType[(int)TokenType.INVOKE] = "INVOKE";
            tokenStringFromTokenType[(int)TokenType.MERGE] = "MERGE";
            tokenStringFromTokenType[(int)TokenType.MOVE] = "MOVE";
            tokenStringFromTokenType[(int)TokenType.MULTIPLY] = "MULTIPLY";
            tokenStringFromTokenType[(int)TokenType.OPEN] = "OPEN";
            tokenStringFromTokenType[(int)TokenType.PERFORM] = "PERFORM";
            tokenStringFromTokenType[(int)TokenType.READ] = "READ";
            tokenStringFromTokenType[(int)TokenType.RELEASE] = "RELEASE";
            tokenStringFromTokenType[(int)TokenType.RETURN] = "RETURN";
            tokenStringFromTokenType[(int)TokenType.REWRITE] = "REWRITE";
            tokenStringFromTokenType[(int)TokenType.SEARCH] = "SEARCH";
            tokenStringFromTokenType[(int)TokenType.SET] = "SET";
            tokenStringFromTokenType[(int)TokenType.SORT] = "SORT";
            tokenStringFromTokenType[(int)TokenType.START] = "START";
            tokenStringFromTokenType[(int)TokenType.STOP] = "STOP";
            tokenStringFromTokenType[(int)TokenType.STRING] = "STRING";
            tokenStringFromTokenType[(int)TokenType.SUBTRACT] = "SUBTRACT";
            tokenStringFromTokenType[(int)TokenType.UNSTRING] = "UNSTRING";
            tokenStringFromTokenType[(int)TokenType.WRITE] = "WRITE";
            tokenStringFromTokenType[(int)TokenType.XML] = "XML";
            tokenStringFromTokenType[(int)TokenType.END_ADD] = "END-ADD";
            tokenStringFromTokenType[(int)TokenType.END_CALL] = "END-CALL";
            tokenStringFromTokenType[(int)TokenType.END_COMPUTE] = "END-COMPUTE";
            tokenStringFromTokenType[(int)TokenType.END_DELETE] = "END-DELETE";
            tokenStringFromTokenType[(int)TokenType.END_DIVIDE] = "END-DIVIDE";
            tokenStringFromTokenType[(int)TokenType.END_EVALUATE] = "END-EVALUATE";
            tokenStringFromTokenType[(int)TokenType.END_EXEC] = "END-EXEC";
            tokenStringFromTokenType[(int)TokenType.END_IF] = "END-IF";
            tokenStringFromTokenType[(int)TokenType.END_INVOKE] = "END-INVOKE";
            tokenStringFromTokenType[(int)TokenType.END_MULTIPLY] = "END-MULTIPLY";
            tokenStringFromTokenType[(int)TokenType.END_PERFORM] = "END-PERFORM";
            tokenStringFromTokenType[(int)TokenType.END_READ] = "END-READ";
            tokenStringFromTokenType[(int)TokenType.END_RETURN] = "END-RETURN";
            tokenStringFromTokenType[(int)TokenType.END_REWRITE] = "END-REWRITE";
            tokenStringFromTokenType[(int)TokenType.END_SEARCH] = "END-SEARCH";
            tokenStringFromTokenType[(int)TokenType.END_START] = "END-START";
            tokenStringFromTokenType[(int)TokenType.END_STRING] = "END-STRING";
            tokenStringFromTokenType[(int)TokenType.END_SUBTRACT] = "END-SUBTRACT";
            tokenStringFromTokenType[(int)TokenType.END_UNSTRING] = "END-UNSTRING";
            tokenStringFromTokenType[(int)TokenType.END_WRITE] = "END-WRITE";
            tokenStringFromTokenType[(int)TokenType.END_XML] = "END-XML";
            tokenStringFromTokenType[(int)TokenType.ADDRESS] = "ADDRESS";
            tokenStringFromTokenType[(int)TokenType.DEBUG_CONTENTS] = "DEBUG-CONTENTS";
            tokenStringFromTokenType[(int)TokenType.DEBUG_ITEM] = "DEBUG-ITEM";
            tokenStringFromTokenType[(int)TokenType.DEBUG_LINE] = "DEBUG-LINE";
            tokenStringFromTokenType[(int)TokenType.DEBUG_NAME] = "DEBUG-NAME";
            tokenStringFromTokenType[(int)TokenType.DEBUG_SUB_1] = "DEBUG-SUB-1";
            tokenStringFromTokenType[(int)TokenType.DEBUG_SUB_2] = "DEBUG-SUB-2";
            tokenStringFromTokenType[(int)TokenType.DEBUG_SUB_3] = "DEBUG-SUB-3";
            tokenStringFromTokenType[(int)TokenType.JNIENVPTR] = "JNIENVPTR";
            tokenStringFromTokenType[(int)TokenType.LENGTH] = "LENGTH";
            tokenStringFromTokenType[(int)TokenType.LINAGE_COUNTER] = "LINAGE-COUNTER";
            tokenStringFromTokenType[(int)TokenType.RETURN_CODE] = "RETURN-CODE";
            tokenStringFromTokenType[(int)TokenType.SHIFT_IN] = "SHIFT-IN";
            tokenStringFromTokenType[(int)TokenType.SHIFT_OUT] = "SHIFT-OUT";
            tokenStringFromTokenType[(int)TokenType.SORT_CONTROL] = "SORT-CONTROL";
            tokenStringFromTokenType[(int)TokenType.SORT_CORE_SIZE] = "SORT-CORE-SIZE";
            tokenStringFromTokenType[(int)TokenType.SORT_FILE_SIZE] = "SORT-FILE-SIZE";
            tokenStringFromTokenType[(int)TokenType.SORT_MESSAGE] = "SORT-MESSAGE";
            tokenStringFromTokenType[(int)TokenType.SORT_MODE_SIZE] = "SORT-MODE-SIZE";
            tokenStringFromTokenType[(int)TokenType.SORT_RETURN] = "SORT-RETURN";
            tokenStringFromTokenType[(int)TokenType.TALLY] = "TALLY";
            tokenStringFromTokenType[(int)TokenType.WHEN_COMPILED] = "WHEN-COMPILED";
            tokenStringFromTokenType[(int)TokenType.XML_CODE] = "XML-CODE";
            tokenStringFromTokenType[(int)TokenType.XML_EVENT] = "XML-EVENT";
            tokenStringFromTokenType[(int)TokenType.XML_INFORMATION] = "XML-INFORMATION";
            tokenStringFromTokenType[(int)TokenType.XML_NAMESPACE] = "XML-NAMESPACE";
            tokenStringFromTokenType[(int)TokenType.XML_NAMESPACE_PREFIX] = "XML-NAMESPACE-PREFIX";
            tokenStringFromTokenType[(int)TokenType.XML_NNAMESPACE] = "XML-NNAMESPACE";
            tokenStringFromTokenType[(int)TokenType.XML_NNAMESPACE_PREFIX] = "XML-NNAMESPACE-PREFIX";
            tokenStringFromTokenType[(int)TokenType.XML_NTEXT] = "XML-NTEXT";
            tokenStringFromTokenType[(int)TokenType.XML_TEXT] = "XML-TEXT";
            tokenStringFromTokenType[(int)TokenType.HIGH_VALUE] = "HIGH-VALUE";
            tokenStringFromTokenType[(int)TokenType.HIGH_VALUES] = "HIGH-VALUES";
            tokenStringFromTokenType[(int)TokenType.LOW_VALUE] = "LOW-VALUE";
            tokenStringFromTokenType[(int)TokenType.LOW_VALUES] = "LOW-VALUES";
            tokenStringFromTokenType[(int)TokenType.NULL] = "NULL";
            tokenStringFromTokenType[(int)TokenType.NULLS] = "NULLS";
            tokenStringFromTokenType[(int)TokenType.QUOTE] = "QUOTE";
            tokenStringFromTokenType[(int)TokenType.QUOTES] = "QUOTES";
            tokenStringFromTokenType[(int)TokenType.SPACE] = "SPACE";
            tokenStringFromTokenType[(int)TokenType.SPACES] = "SPACES";
            tokenStringFromTokenType[(int)TokenType.ZERO] = "ZERO";
            tokenStringFromTokenType[(int)TokenType.ZEROES] = "ZEROES";
            tokenStringFromTokenType[(int)TokenType.ZEROS] = "ZEROS";
            tokenStringFromTokenType[(int)TokenType.SymbolicCharacter] = "SymbolicCharacter";
            tokenStringFromTokenType[(int)TokenType.SELF] = "SELF";
            tokenStringFromTokenType[(int)TokenType.SUPER] = "SUPER";
            tokenStringFromTokenType[(int)TokenType.ACCESS] = "ACCESS";
            tokenStringFromTokenType[(int)TokenType.ADVANCING] = "ADVANCING";
            tokenStringFromTokenType[(int)TokenType.AFTER] = "AFTER";
            tokenStringFromTokenType[(int)TokenType.ALL] = "ALL";
            tokenStringFromTokenType[(int)TokenType.ALPHABET] = "ALPHABET";
            tokenStringFromTokenType[(int)TokenType.ALPHABETIC] = "ALPHABETIC";
            tokenStringFromTokenType[(int)TokenType.ALPHABETIC_LOWER] = "ALPHABETIC-LOWER";
            tokenStringFromTokenType[(int)TokenType.ALPHABETIC_UPPER] = "ALPHABETIC-UPPER";
            tokenStringFromTokenType[(int)TokenType.ALPHANUMERIC] = "ALPHANUMERIC";
            tokenStringFromTokenType[(int)TokenType.ALPHANUMERIC_EDITED] = "ALPHANUMERIC-EDITED";
            tokenStringFromTokenType[(int)TokenType.ALSO] = "ALSO";
            tokenStringFromTokenType[(int)TokenType.ALTERNATE] = "ALTERNATE";
            tokenStringFromTokenType[(int)TokenType.AND] = "AND";
            tokenStringFromTokenType[(int)TokenType.ANY] = "ANY";
            tokenStringFromTokenType[(int)TokenType.APPLY] = "APPLY";
            tokenStringFromTokenType[(int)TokenType.ARE] = "ARE";
            tokenStringFromTokenType[(int)TokenType.AREA] = "AREA";
            tokenStringFromTokenType[(int)TokenType.AREAS] = "AREAS";
            tokenStringFromTokenType[(int)TokenType.ASCENDING] = "ASCENDING";
            tokenStringFromTokenType[(int)TokenType.ASSIGN] = "ASSIGN";
            tokenStringFromTokenType[(int)TokenType.AT] = "AT";
            tokenStringFromTokenType[(int)TokenType.ATTRIBUTE] = "ATTRIBUTE";
            tokenStringFromTokenType[(int)TokenType.ATTRIBUTES] = "ATTRIBUTES";
            tokenStringFromTokenType[(int)TokenType.AUTHOR] = "AUTHOR";
            tokenStringFromTokenType[(int)TokenType.BEFORE] = "BEFORE";
            tokenStringFromTokenType[(int)TokenType.BEGINNING] = "BEGINNING";
            tokenStringFromTokenType[(int)TokenType.BINARY] = "BINARY";
            tokenStringFromTokenType[(int)TokenType.BLANK] = "BLANK";
            tokenStringFromTokenType[(int)TokenType.BLOCK] = "BLOCK";
            tokenStringFromTokenType[(int)TokenType.BOTTOM] = "BOTTOM";
            tokenStringFromTokenType[(int)TokenType.BY] = "BY";
            tokenStringFromTokenType[(int)TokenType.CHARACTER] = "CHARACTER";
            tokenStringFromTokenType[(int)TokenType.CHARACTERS] = "CHARACTERS";
            tokenStringFromTokenType[(int)TokenType.CLASS] = "CLASS";
            tokenStringFromTokenType[(int)TokenType.CLASS_ID] = "CLASS-ID";
            tokenStringFromTokenType[(int)TokenType.COBOL] = "COBOL";
            tokenStringFromTokenType[(int)TokenType.CODE] = "CODE";
            tokenStringFromTokenType[(int)TokenType.CODE_SET] = "CODE-SET";
            tokenStringFromTokenType[(int)TokenType.COLLATING] = "COLLATING";
            tokenStringFromTokenType[(int)TokenType.COM_REG] = "COM-REG";
            tokenStringFromTokenType[(int)TokenType.COMMA] = "COMMA";
            tokenStringFromTokenType[(int)TokenType.COMMON] = "COMMON";
            tokenStringFromTokenType[(int)TokenType.COMP] = "COMP";
            tokenStringFromTokenType[(int)TokenType.COMP_1] = "COMP-1";
            tokenStringFromTokenType[(int)TokenType.COMP_2] = "COMP-2";
            tokenStringFromTokenType[(int)TokenType.COMP_3] = "COMP-3";
            tokenStringFromTokenType[(int)TokenType.COMP_4] = "COMP-4";
            tokenStringFromTokenType[(int)TokenType.COMP_5] = "COMP-5";
            tokenStringFromTokenType[(int)TokenType.COMPUTATIONAL] = "COMPUTATIONAL";
            tokenStringFromTokenType[(int)TokenType.COMPUTATIONAL_1] = "COMPUTATIONAL-1";
            tokenStringFromTokenType[(int)TokenType.COMPUTATIONAL_2] = "COMPUTATIONAL-2";
            tokenStringFromTokenType[(int)TokenType.COMPUTATIONAL_3] = "COMPUTATIONAL-3";
            tokenStringFromTokenType[(int)TokenType.COMPUTATIONAL_4] = "COMPUTATIONAL-4";
            tokenStringFromTokenType[(int)TokenType.COMPUTATIONAL_5] = "COMPUTATIONAL-5";
            tokenStringFromTokenType[(int)TokenType.CONFIGURATION] = "CONFIGURATION";
            tokenStringFromTokenType[(int)TokenType.CONTAINS] = "CONTAINS";
            tokenStringFromTokenType[(int)TokenType.CONTENT] = "CONTENT";
            tokenStringFromTokenType[(int)TokenType.CONVERTING] = "CONVERTING";
            tokenStringFromTokenType[(int)TokenType.CORR] = "CORR";
            tokenStringFromTokenType[(int)TokenType.CORRESPONDING] = "CORRESPONDING";
            tokenStringFromTokenType[(int)TokenType.COUNT] = "COUNT";
            tokenStringFromTokenType[(int)TokenType.CURRENCY] = "CURRENCY";
            tokenStringFromTokenType[(int)TokenType.DATA] = "DATA";
            tokenStringFromTokenType[(int)TokenType.DATE] = "DATE";
            tokenStringFromTokenType[(int)TokenType.DATE_COMPILED] = "DATE-COMPILED";
            tokenStringFromTokenType[(int)TokenType.DATE_WRITTEN] = "DATE-WRITTEN";
            tokenStringFromTokenType[(int)TokenType.DAY] = "DAY";
            tokenStringFromTokenType[(int)TokenType.DAY_OF_WEEK] = "DAY-OF-WEEK";
            tokenStringFromTokenType[(int)TokenType.DBCS] = "DBCS";
            tokenStringFromTokenType[(int)TokenType.DEBUGGING] = "DEBUGGING";
            tokenStringFromTokenType[(int)TokenType.DECIMAL_POINT] = "DECIMAL-POINT";
            tokenStringFromTokenType[(int)TokenType.DECLARATIVES] = "DECLARATIVES";
            tokenStringFromTokenType[(int)TokenType.DELIMITED] = "DELIMITED";
            tokenStringFromTokenType[(int)TokenType.DELIMITER] = "DELIMITER";
            tokenStringFromTokenType[(int)TokenType.DEPENDING] = "DEPENDING";
            tokenStringFromTokenType[(int)TokenType.DESCENDING] = "DESCENDING";
            tokenStringFromTokenType[(int)TokenType.DISPLAY_ARG] = "DISPLAY";
            tokenStringFromTokenType[(int)TokenType.DISPLAY_1] = "DISPLAY-1";
            tokenStringFromTokenType[(int)TokenType.DIVISION] = "DIVISION";
            tokenStringFromTokenType[(int)TokenType.DOWN] = "DOWN";
            tokenStringFromTokenType[(int)TokenType.DUPLICATES] = "DUPLICATES";
            tokenStringFromTokenType[(int)TokenType.DYNAMIC] = "DYNAMIC";
            tokenStringFromTokenType[(int)TokenType.EBCDIC] = "EBCDIC";
            tokenStringFromTokenType[(int)TokenType.EGCS] = "EGCS";
            tokenStringFromTokenType[(int)TokenType.ELEMENT] = "ELEMENT";
            tokenStringFromTokenType[(int)TokenType.ELSE] = "ELSE";
            tokenStringFromTokenType[(int)TokenType.ENCODING] = "ENCODING";
            tokenStringFromTokenType[(int)TokenType.END] = "END";
            tokenStringFromTokenType[(int)TokenType.END_OF_PAGE] = "END-OF-PAGE";
            tokenStringFromTokenType[(int)TokenType.ENDING] = "ENDING";
            tokenStringFromTokenType[(int)TokenType.ENTRY_ARG] = "ENTRY";
            tokenStringFromTokenType[(int)TokenType.ENVIRONMENT] = "ENVIRONMENT";
            tokenStringFromTokenType[(int)TokenType.EOP] = "EOP";
            tokenStringFromTokenType[(int)TokenType.EQUAL] = "EQUAL";
            tokenStringFromTokenType[(int)TokenType.ERROR] = "ERROR";
            tokenStringFromTokenType[(int)TokenType.EVERY] = "EVERY";
            tokenStringFromTokenType[(int)TokenType.EXCEPTION] = "EXCEPTION";
            tokenStringFromTokenType[(int)TokenType.EXTEND] = "EXTEND";
            tokenStringFromTokenType[(int)TokenType.EXTERNAL] = "EXTERNAL";
            tokenStringFromTokenType[(int)TokenType.FACTORY] = "FACTORY";
            tokenStringFromTokenType[(int)TokenType.FALSE] = "FALSE";
            tokenStringFromTokenType[(int)TokenType.FD] = "FD";
            tokenStringFromTokenType[(int)TokenType.FILE] = "FILE";
            tokenStringFromTokenType[(int)TokenType.FILE_CONTROL] = "FILE-CONTROL";
            tokenStringFromTokenType[(int)TokenType.FILLER] = "FILLER";
            tokenStringFromTokenType[(int)TokenType.FIRST] = "FIRST";
            tokenStringFromTokenType[(int)TokenType.FOOTING] = "FOOTING";
            tokenStringFromTokenType[(int)TokenType.FOR] = "FOR";
            tokenStringFromTokenType[(int)TokenType.FROM] = "FROM";
            tokenStringFromTokenType[(int)TokenType.FUNCTION] = "FUNCTION";
            tokenStringFromTokenType[(int)TokenType.FUNCTION_POINTER] = "FUNCTION-POINTER";
            tokenStringFromTokenType[(int)TokenType.GENERATE] = "GENERATE";
            tokenStringFromTokenType[(int)TokenType.GIVING] = "GIVING";
            tokenStringFromTokenType[(int)TokenType.GLOBAL] = "GLOBAL";
            tokenStringFromTokenType[(int)TokenType.GREATER] = "GREATER";
            tokenStringFromTokenType[(int)TokenType.GROUP_USAGE] = "GROUP-USAGE";
            tokenStringFromTokenType[(int)TokenType.I_O] = "I-O";
            tokenStringFromTokenType[(int)TokenType.I_O_CONTROL] = "I-O-CONTROL";
            tokenStringFromTokenType[(int)TokenType.ID] = "ID";
            tokenStringFromTokenType[(int)TokenType.IDENTIFICATION] = "IDENTIFICATION";
            tokenStringFromTokenType[(int)TokenType.IN] = "IN";
            tokenStringFromTokenType[(int)TokenType.INDEX] = "INDEX";
            tokenStringFromTokenType[(int)TokenType.INDEXED] = "INDEXED";
            tokenStringFromTokenType[(int)TokenType.INHERITS] = "INHERITS";
            tokenStringFromTokenType[(int)TokenType.INITIAL] = "INITIAL";
            tokenStringFromTokenType[(int)TokenType.INPUT] = "INPUT";
            tokenStringFromTokenType[(int)TokenType.INPUT_OUTPUT] = "INPUT-OUTPUT";
            tokenStringFromTokenType[(int)TokenType.INSTALLATION] = "INSTALLATION";
            tokenStringFromTokenType[(int)TokenType.INTO] = "INTO";
            tokenStringFromTokenType[(int)TokenType.INVALID] = "INVALID";
            tokenStringFromTokenType[(int)TokenType.IS] = "IS";
            tokenStringFromTokenType[(int)TokenType.JUST] = "JUST";
            tokenStringFromTokenType[(int)TokenType.JUSTIFIED] = "JUSTIFIED";
            tokenStringFromTokenType[(int)TokenType.KANJI] = "KANJI";
            tokenStringFromTokenType[(int)TokenType.KEY] = "KEY";
            tokenStringFromTokenType[(int)TokenType.LABEL] = "LABEL";
            tokenStringFromTokenType[(int)TokenType.LEADING] = "LEADING";
            tokenStringFromTokenType[(int)TokenType.LEFT] = "LEFT";
            tokenStringFromTokenType[(int)TokenType.LESS] = "LESS";
            tokenStringFromTokenType[(int)TokenType.LINAGE] = "LINAGE";
            tokenStringFromTokenType[(int)TokenType.LINE] = "LINE";
            tokenStringFromTokenType[(int)TokenType.LINES] = "LINES";
            tokenStringFromTokenType[(int)TokenType.LINKAGE] = "LINKAGE";
            tokenStringFromTokenType[(int)TokenType.LOCAL_STORAGE] = "LOCAL-STORAGE";
            tokenStringFromTokenType[(int)TokenType.LOCK] = "LOCK";
            tokenStringFromTokenType[(int)TokenType.MEMORY] = "MEMORY";
            tokenStringFromTokenType[(int)TokenType.METHOD] = "METHOD";
            tokenStringFromTokenType[(int)TokenType.METHOD_ID] = "METHOD-ID";
            tokenStringFromTokenType[(int)TokenType.MODE] = "MODE";
            tokenStringFromTokenType[(int)TokenType.MODULES] = "MODULES";
            tokenStringFromTokenType[(int)TokenType.MORE_LABELS] = "MORE-LABELS";
            tokenStringFromTokenType[(int)TokenType.MULTIPLE] = "MULTIPLE";
            tokenStringFromTokenType[(int)TokenType.NAME] = "NAME";
            tokenStringFromTokenType[(int)TokenType.NAMESPACE] = "NAMESPACE";
            tokenStringFromTokenType[(int)TokenType.NAMESPACE_PREFIX] = "NAMESPACE-PREFIX";
            tokenStringFromTokenType[(int)TokenType.NATIONAL] = "NATIONAL";
            tokenStringFromTokenType[(int)TokenType.NATIONAL_EDITED] = "NATIONAL-EDITED";
            tokenStringFromTokenType[(int)TokenType.NATIVE] = "NATIVE";
            tokenStringFromTokenType[(int)TokenType.NEGATIVE] = "NEGATIVE";
            tokenStringFromTokenType[(int)TokenType.NEW] = "NEW";
            tokenStringFromTokenType[(int)TokenType.NEXT] = "NEXT";
            tokenStringFromTokenType[(int)TokenType.NO] = "NO";
            tokenStringFromTokenType[(int)TokenType.NONNUMERIC] = "NONNUMERIC";
            tokenStringFromTokenType[(int)TokenType.NOT] = "NOT";
            tokenStringFromTokenType[(int)TokenType.NUMERIC] = "NUMERIC";
            tokenStringFromTokenType[(int)TokenType.NUMERIC_EDITED] = "NUMERIC-EDITED";
            tokenStringFromTokenType[(int)TokenType.OBJECT] = "OBJECT";
            tokenStringFromTokenType[(int)TokenType.OBJECT_COMPUTER] = "OBJECT-COMPUTER";
            tokenStringFromTokenType[(int)TokenType.OCCURS] = "OCCURS";
            tokenStringFromTokenType[(int)TokenType.OF] = "OF";
            tokenStringFromTokenType[(int)TokenType.OFF] = "OFF";
            tokenStringFromTokenType[(int)TokenType.OMITTED] = "OMITTED";
            tokenStringFromTokenType[(int)TokenType.ON] = "ON";
            tokenStringFromTokenType[(int)TokenType.OPTIONAL] = "OPTIONAL";
            tokenStringFromTokenType[(int)TokenType.OR] = "OR";
            tokenStringFromTokenType[(int)TokenType.ORDER] = "ORDER";
            tokenStringFromTokenType[(int)TokenType.ORGANIZATION] = "ORGANIZATION";
            tokenStringFromTokenType[(int)TokenType.OTHER] = "OTHER";
            tokenStringFromTokenType[(int)TokenType.OUTPUT] = "OUTPUT";
            tokenStringFromTokenType[(int)TokenType.OVERFLOW] = "OVERFLOW";
            tokenStringFromTokenType[(int)TokenType.OVERRIDE] = "OVERRIDE";
            tokenStringFromTokenType[(int)TokenType.PACKED_DECIMAL] = "PACKED-DECIMAL";
            tokenStringFromTokenType[(int)TokenType.PADDING] = "PADDING";
            tokenStringFromTokenType[(int)TokenType.PAGE] = "PAGE";
            tokenStringFromTokenType[(int)TokenType.PARSE] = "PARSE";
            tokenStringFromTokenType[(int)TokenType.PASSWORD] = "PASSWORD";
            tokenStringFromTokenType[(int)TokenType.PIC] = "PIC";
            tokenStringFromTokenType[(int)TokenType.PICTURE] = "PICTURE";
            tokenStringFromTokenType[(int)TokenType.POINTER] = "POINTER";
            tokenStringFromTokenType[(int)TokenType.POSITION] = "POSITION";
            tokenStringFromTokenType[(int)TokenType.POSITIVE] = "POSITIVE";
            tokenStringFromTokenType[(int)TokenType.PROCEDURE] = "PROCEDURE";
            tokenStringFromTokenType[(int)TokenType.PROCEDURE_POINTER] = "PROCEDURE-POINTER";
            tokenStringFromTokenType[(int)TokenType.PROCEDURES] = "PROCEDURES";
            tokenStringFromTokenType[(int)TokenType.PROCEED] = "PROCEED";
            tokenStringFromTokenType[(int)TokenType.PROCESSING] = "PROCESSING";
            tokenStringFromTokenType[(int)TokenType.PROGRAM] = "PROGRAM";
            tokenStringFromTokenType[(int)TokenType.PROGRAM_ID] = "PROGRAM-ID";
            tokenStringFromTokenType[(int)TokenType.RANDOM] = "RANDOM";
            tokenStringFromTokenType[(int)TokenType.RECORD] = "RECORD";
            tokenStringFromTokenType[(int)TokenType.RECORDING] = "RECORDING";
            tokenStringFromTokenType[(int)TokenType.RECORDS] = "RECORDS";
            tokenStringFromTokenType[(int)TokenType.RECURSIVE] = "RECURSIVE";
            tokenStringFromTokenType[(int)TokenType.REDEFINES] = "REDEFINES";
            tokenStringFromTokenType[(int)TokenType.REEL] = "REEL";
            tokenStringFromTokenType[(int)TokenType.REFERENCE] = "REFERENCE";
            tokenStringFromTokenType[(int)TokenType.REFERENCES] = "REFERENCES";
            tokenStringFromTokenType[(int)TokenType.RELATIVE] = "RELATIVE";
            tokenStringFromTokenType[(int)TokenType.RELOAD] = "RELOAD";
            tokenStringFromTokenType[(int)TokenType.REMAINDER] = "REMAINDER";
            tokenStringFromTokenType[(int)TokenType.REMOVAL] = "REMOVAL";
            tokenStringFromTokenType[(int)TokenType.RENAMES] = "RENAMES";
            tokenStringFromTokenType[(int)TokenType.REPLACING] = "REPLACING";
            tokenStringFromTokenType[(int)TokenType.REPOSITORY] = "REPOSITORY";
            tokenStringFromTokenType[(int)TokenType.RERUN] = "RERUN";
            tokenStringFromTokenType[(int)TokenType.RESERVE] = "RESERVE";
            tokenStringFromTokenType[(int)TokenType.RETURNING] = "RETURNING";
            tokenStringFromTokenType[(int)TokenType.REVERSED] = "REVERSED";
            tokenStringFromTokenType[(int)TokenType.REWIND] = "REWIND";
            tokenStringFromTokenType[(int)TokenType.RIGHT] = "RIGHT";
            tokenStringFromTokenType[(int)TokenType.ROUNDED] = "ROUNDED";
            tokenStringFromTokenType[(int)TokenType.RUN] = "RUN";
            tokenStringFromTokenType[(int)TokenType.SAME] = "SAME";
            tokenStringFromTokenType[(int)TokenType.SD] = "SD";
            tokenStringFromTokenType[(int)TokenType.SECTION] = "SECTION";
            tokenStringFromTokenType[(int)TokenType.SECURITY] = "SECURITY";
            tokenStringFromTokenType[(int)TokenType.SEGMENT_LIMIT] = "SEGMENT-LIMIT";
            tokenStringFromTokenType[(int)TokenType.SELECT] = "SELECT";
            tokenStringFromTokenType[(int)TokenType.SENTENCE] = "SENTENCE";
            tokenStringFromTokenType[(int)TokenType.SEPARATE] = "SEPARATE";
            tokenStringFromTokenType[(int)TokenType.SEQUENCE] = "SEQUENCE";
            tokenStringFromTokenType[(int)TokenType.SEQUENTIAL] = "SEQUENTIAL";
            tokenStringFromTokenType[(int)TokenType.SIGN] = "SIGN";
            tokenStringFromTokenType[(int)TokenType.SIZE] = "SIZE";
            tokenStringFromTokenType[(int)TokenType.SORT_ARG] = "SORT";
            tokenStringFromTokenType[(int)TokenType.SORT_MERGE] = "SORT-MERGE";
            tokenStringFromTokenType[(int)TokenType.SOURCE_COMPUTER] = "SOURCE-COMPUTER";
            tokenStringFromTokenType[(int)TokenType.SPECIAL_NAMES] = "SPECIAL-NAMES";
            tokenStringFromTokenType[(int)TokenType.SQL] = "SQL";
            tokenStringFromTokenType[(int)TokenType.SQLIMS] = "SQLIMS";
            tokenStringFromTokenType[(int)TokenType.STANDARD] = "STANDARD";
            tokenStringFromTokenType[(int)TokenType.STANDARD_1] = "STANDARD-1";
            tokenStringFromTokenType[(int)TokenType.STANDARD_2] = "STANDARD-2";
            tokenStringFromTokenType[(int)TokenType.STATUS] = "STATUS";
            tokenStringFromTokenType[(int)TokenType.SUPPRESS] = "SUPPRESS";
            tokenStringFromTokenType[(int)TokenType.SYMBOL] = "SYMBOL";
            tokenStringFromTokenType[(int)TokenType.SYMBOLIC] = "SYMBOLIC";
            tokenStringFromTokenType[(int)TokenType.SYNC] = "SYNC";
            tokenStringFromTokenType[(int)TokenType.SYNCHRONIZED] = "SYNCHRONIZED";
            tokenStringFromTokenType[(int)TokenType.TALLYING] = "TALLYING";
            tokenStringFromTokenType[(int)TokenType.TAPE] = "TAPE";
            tokenStringFromTokenType[(int)TokenType.TEST] = "TEST";
            tokenStringFromTokenType[(int)TokenType.THAN] = "THAN";
            tokenStringFromTokenType[(int)TokenType.THEN] = "THEN";
            tokenStringFromTokenType[(int)TokenType.THROUGH] = "THROUGH";
            tokenStringFromTokenType[(int)TokenType.THRU] = "THRU";
            tokenStringFromTokenType[(int)TokenType.TIME] = "TIME";
            tokenStringFromTokenType[(int)TokenType.TIMES] = "TIMES";
            tokenStringFromTokenType[(int)TokenType.TO] = "TO";
            tokenStringFromTokenType[(int)TokenType.TOP] = "TOP";
            tokenStringFromTokenType[(int)TokenType.TRACE] = "TRACE";
            tokenStringFromTokenType[(int)TokenType.TRAILING] = "TRAILING";
            tokenStringFromTokenType[(int)TokenType.TRUE] = "TRUE";
            tokenStringFromTokenType[(int)TokenType.TYPE] = "TYPE";
            tokenStringFromTokenType[(int)TokenType.UNBOUNDED] = "UNBOUNDED";
            tokenStringFromTokenType[(int)TokenType.UNIT] = "UNIT";
            tokenStringFromTokenType[(int)TokenType.UNTIL] = "UNTIL";
            tokenStringFromTokenType[(int)TokenType.UP] = "UP";
            tokenStringFromTokenType[(int)TokenType.UPON] = "UPON";
            tokenStringFromTokenType[(int)TokenType.USAGE] = "USAGE";
            tokenStringFromTokenType[(int)TokenType.USE] = "USE";
            tokenStringFromTokenType[(int)TokenType.USING] = "USING";
            tokenStringFromTokenType[(int)TokenType.VALIDATING] = "VALIDATING";
            tokenStringFromTokenType[(int)TokenType.VALUE] = "VALUE";
            tokenStringFromTokenType[(int)TokenType.VALUES] = "VALUES";
            tokenStringFromTokenType[(int)TokenType.VARYING] = "VARYING";
            tokenStringFromTokenType[(int)TokenType.WHEN] = "WHEN";
            tokenStringFromTokenType[(int)TokenType.WITH] = "WITH";
            tokenStringFromTokenType[(int)TokenType.WORDS] = "WORDS";
            tokenStringFromTokenType[(int)TokenType.WORKING_STORAGE] = "WORKING-STORAGE";
            tokenStringFromTokenType[(int)TokenType.WRITE_ONLY] = "WRITE-ONLY";
            tokenStringFromTokenType[(int)TokenType.XML_DECLARATION] = "XML-DECLARATION";
            tokenStringFromTokenType[(int)TokenType.XML_SCHEMA] = "XML-SCHEMA";
            tokenStringFromTokenType[(int)TokenType.YYYYDDD] = "YYYYDDD";
            tokenStringFromTokenType[(int)TokenType.YYYYMMDD] = "YYYYMMDD";

            // Map token string to token type

            tokenTypeFromTokenString = new Dictionary<string, TokenType>(MAX_TOKEN_TYPE, StringComparer.OrdinalIgnoreCase);
            for (tokenType = 0; tokenType <= MAX_TOKEN_TYPE; tokenType++)
            {
                string tokenString = tokenStringFromTokenType[tokenType];
                if (!String.IsNullOrEmpty(tokenString) && !tokenTypeFromTokenString.ContainsKey(tokenString))
                {
                    tokenTypeFromTokenString[tokenString] = (TokenType)tokenType;
                }
            }
            // Token type DELETE is much more frequent than DELETE_CD, it should have priority
            tokenTypeFromTokenString["DELETE"] = TokenType.DELETE;
        }

        private static TokenFamily[] tokenFamilyFromTokenType; 
        
        public static TokenFamily GetTokenFamilyFromTokenType(TokenType tokenType)
        {
            if (tokenType == TokenType.EndOfFile) return TokenFamily.SyntaxSeparator;
            return tokenFamilyFromTokenType[(int)tokenType];
        }

        private static string[] tokenStringFromTokenType;

        public static string GetTokenStringFromTokenType(TokenType tokenType)
        {
            return tokenStringFromTokenType[(int)tokenType];
        }

        private static IDictionary<string,TokenType> tokenTypeFromTokenString;

        internal static TokenType GetTokenTypeFromTokenString(string tokenString)
        {
            TokenType tokenType; 
            if(tokenTypeFromTokenString.TryGetValue(tokenString, out tokenType))
            {
                return tokenType;
            }
            else
            {
                return TokenType.UserDefinedWord;
            }
        }
    }
}
