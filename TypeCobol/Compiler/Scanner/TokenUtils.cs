using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
    internal static class TokenUtils
    {

        static TokenUtils()
        {
            // Map token types to token families
            var types = (TokenType[])Enum.GetValues(typeof(TokenType));
            var families = (TokenFamily[])Enum.GetValues(typeof(TokenFamily));

            tokenFamilyFromTokenType = new TokenFamily[types.Length];
            int family = 0;
            for (int tokenType = 0; tokenType < types.Length - 1; tokenType++)
            {
                if (family < (families.Length - 1) && tokenType == (int)families[family + 1]) family++;
                tokenFamilyFromTokenType[tokenType] = families[family];
            }
            // Register the token strings corresponding to each token type (for keywords only)
            int keywordBegin = (int)TokenType.UserDefinedWord + 1;
            int keywordEnd = (int)TokenType.QuestionMark - 1;
            tokenStringFromTokenType = new string[types.Length];
            for (int c = keywordBegin; c < types.Length; c++)
            {
                var current = types[c];
                if ((int)current > keywordEnd) break;
                tokenStringFromTokenType[(int)current] = current.ToString().Replace('_', '-');
            }
            tokenStringFromTokenType[(int)TokenType.ASTERISK_CBL] = "*CBL";
            tokenStringFromTokenType[(int)TokenType.ASTERISK_CONTROL] = "*CONTROL";
            tokenStringFromTokenType[(int)TokenType.DELETE_CD] = "DELETE";
            tokenStringFromTokenType[(int)TokenType.SERVICE_CD] = "SERVICE";
            tokenStringFromTokenType[(int)TokenType.EXEC_SQL_INCLUDE] = "INCLUDE";

            // Map token string to token type
            tokenTypeFromTokenString = new Dictionary<string, TokenType>(types.Length - 1, StringComparer.OrdinalIgnoreCase);
            for (int tokenType = 0; tokenType < types.Length; tokenType++)
            {
                string tokenString = tokenStringFromTokenType[tokenType];
                if (!String.IsNullOrEmpty(tokenString) && !tokenTypeFromTokenString.ContainsKey(tokenString))
                {
                    tokenTypeFromTokenString[tokenString] = (TokenType)tokenType;
                }
            }
            // Token type DELETE is much more frequent than DELETE_CD, it should have priority
            tokenTypeFromTokenString["DELETE"] = TokenType.DELETE;
            // Token type SERVICE is much more frequent than SERVICE_CD, it should have priority
            tokenTypeFromTokenString["SERVICE"] = TokenType.SERVICE;
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

        private static IDictionary<string, TokenType> tokenTypeFromTokenString;

        internal static TokenType GetTokenTypeFromTokenString(string tokenString)
        {
            TokenType tokenType;
            if (tokenTypeFromTokenString.TryGetValue(tokenString, out tokenType))
            {
                return tokenType;
            }
            else
            {
                return TokenType.UserDefinedWord;
            }
        }

        public static Regex COBOL_INTRINSIC_FUNCTIONS = new Regex("^(ACOS|ANNUITY|ASIN|ATAN|CHAR|COS|CURRENT-DATE|DATE-OF-INTEGER|DATE-TO-YYYYMMDD|DAY-OF-INTEGER|DAY-TO-YYYYDDD|DISPLAY-OF|FACTORIAL|INTEGER|INTEGER-OF-DATE|INTEGER-OF-DAY|INTEGER-PART|LENGTH|LOG|LOG10|LOWER-CASE|MAX|MEAN|MEDIAN|MIDRANGE|MIN|MOD|NATIONAL-OF|NUMVAL|NUMVAL-C|ORD|ORD-MAX|ORD-MIN|PRESENT-VALUE|RANDOM|RANGE|REM|REVERSE|SIN|SQRT|STANDARD-DEVIATION|SUM|TAN|ULENGTH|UPOS|UPPER-CASE|USUBSTR|USUPPLEMENTARY|UVALID|UWIDTH|VARIANCE|WHEN-COMPILED|YEAR-TO-YYYY)$", RegexOptions.IgnoreCase | RegexOptions.Compiled);

        public static string GetDisplayNameForTokenFamily(TokenFamily tokenFamily)
        {
            switch (tokenFamily)
            {
                case TokenFamily.Whitespace:
                    return "whitespace";
                case TokenFamily.Comments:
                    return "comments";
                case TokenFamily.SyntaxSeparator:
                    return "separator";
                case TokenFamily.ArithmeticOperator:
                    return "arithmetic operator";
                case TokenFamily.RelationalOperator:
                    return "relational operator";
                case TokenFamily.AlphanumericLiteral:
                    return "alphanumeric literal";
                case TokenFamily.NumericLiteral:
                    return "numeric literal";
                case TokenFamily.SyntaxLiteral:
                    return "character string";
                case TokenFamily.Symbol:
                    return "symbol";
                case TokenFamily.CompilerDirectiveStartingKeyword:
                    return "compiler directive starting keyword";
                case TokenFamily.CodeElementStartingKeyword:
                    return "statement starting keyword";
                case TokenFamily.SpecialRegisterKeyword:
                    return "special register";
                case TokenFamily.FigurativeConstantKeyword:
                    return "figurative constant";
                case TokenFamily.SpecialObjetIdentifierKeyword:
                    return "special object identifier";
                case TokenFamily.SyntaxKeyword:
                    return "keyword";
                case TokenFamily.Cobol2002Keyword:
                    return "cobol 2002 keyword";
                case TokenFamily.TypeCobolKeyword:
                    return "TypeCobol keyword";
                case TokenFamily.TypeCobolOperators:
                    return "TypeCobol Operators";
                default:
                    return "...";
            }
        }

        public static string GetDisplayNameForTokenType(TokenType tokenType)
        {
            if ((int)TokenFamily.TypeCobolOperators > (int)tokenType && (int)tokenType >= (int)TokenFamily.CompilerDirectiveStartingKeyword && tokenType != TokenType.SymbolicCharacter )
            {
                return tokenType.ToString().Replace('_', '-');
            }
            else
            {
                switch (tokenType)
                {
                    case TokenType.EndOfFile:
                        return "end of file";
                    case TokenType.SpaceSeparator:
                        return "space";
                    case TokenType.CommaSeparator:
                        return "','";
                    case TokenType.SemicolonSeparator:
                        return "';'";
                    case TokenType.PeriodSeparator:
                        return "'.'";
                    case TokenType.ColonSeparator:
                        return "':'";
                    case TokenType.QualifiedNameSeparator:
                        return "'::'";
                    case TokenType.LeftParenthesisSeparator:
                        return "'('";
                    case TokenType.RightParenthesisSeparator:
                        return "')'";
                    case TokenType.PseudoTextDelimiter:
                        return "'=='";
                    case TokenType.PlusOperator:
                        return "'+'";
                    case TokenType.MinusOperator:
                        return "'-'";
                    case TokenType.DivideOperator:
                        return "'/'";
                    case TokenType.MultiplyOperator:
                        return "'*'";
                    case TokenType.PowerOperator:
                        return "'**'";
                    case TokenType.LessThanOperator:
                        return "'<'";
                    case TokenType.GreaterThanOperator:
                        return "'>'";
                    case TokenType.LessThanOrEqualOperator:
                        return "'<='";
                    case TokenType.GreaterThanOrEqualOperator:
                        return "'>='";
                    case TokenType.EqualOperator:
                        return "'='";
                    case TokenType.AlphanumericLiteral:
                        return "alphanumeric literal";
                    case TokenType.HexadecimalAlphanumericLiteral:
                        return "hexadecimal alphanumeric literal";
                    case TokenType.NullTerminatedAlphanumericLiteral:
                        return "null terminated alphanumeric literal";
                    case TokenType.NationalLiteral:
                        return "national literal";
                    case TokenType.HexadecimalNationalLiteral:
                        return "hexadecimal national literal";
                    case TokenType.DBCSLiteral:
                        return "dbcs literal";
                    case TokenType.LevelNumber:
                        return "level number";
                    case TokenType.IntegerLiteral:
                        return "integer literal";
                    case TokenType.DecimalLiteral:
                        return "decimal literal";
                    case TokenType.FloatingPointLiteral:
                        return "floating point literal";
                    case TokenType.PictureCharacterString:
                        return "picture character string";
                    case TokenType.CommentEntry:
                        return "comment entry";
                    case TokenType.ExecStatementText:
                        return "exec statement text";
                    case TokenType.SectionParagraphName:
                        return "section or pargraph name";
                    case TokenType.IntrinsicFunctionName:
                        return "intrinsic function name";
                    case TokenType.ExecTranslatorName:
                        return "exec translator name";
                    case TokenType.PartialCobolWord:
                        return "partial cobol word";
                    case TokenType.UserDefinedWord:
                        return "user defined word";
                    case TokenType.SymbolicCharacter:
                        return "symbolic character";
                    case TokenType.QuestionMark:
                        return "?";
                    default:
                        return "...";
                }
            }
        }
    }
}
