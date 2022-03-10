using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace TypeCobol.Compiler.Scanner
{
    internal static class TokenUtils
    {
        public static readonly Regex CobolIntrinsicFunctions = new Regex("^(ACOS|ANNUITY|ASIN|ATAN|CHAR|COS|CURRENT-DATE|DATE-OF-INTEGER|DATE-TO-YYYYMMDD|DAY-OF-INTEGER|DAY-TO-YYYYDDD|DISPLAY-OF|FACTORIAL|INTEGER|INTEGER-OF-DATE|INTEGER-OF-DAY|INTEGER-PART|LENGTH|LOG|LOG10|LOWER-CASE|MAX|MEAN|MEDIAN|MIDRANGE|MIN|MOD|NATIONAL-OF|NUMVAL|NUMVAL-C|ORD|ORD-MAX|ORD-MIN|PRESENT-VALUE|RANDOM|RANGE|REM|REVERSE|SIN|SQRT|STANDARD-DEVIATION|SUM|TAN|ULENGTH|UPOS|UPPER-CASE|USUBSTR|USUPPLEMENTARY|UVALID|UWIDTH|VARIANCE|WHEN-COMPILED|YEAR-TO-YYYY)$", RegexOptions.IgnoreCase | RegexOptions.Compiled);

        private static readonly TokenFamily[] _TokenFamilyFromTokenType;
        private static readonly string[] _TokenStringFromTokenType;
        private static readonly IDictionary<string, TokenType> _TokenTypeFromTokenString;
        private static readonly IDictionary<string, TokenType> _SqlTokenTypeFromTokenString;

        static TokenUtils()
        {
            // Map token types to token families
            var types = (TokenType[])Enum.GetValues(typeof(TokenType));
            var families = (TokenFamily[])Enum.GetValues(typeof(TokenFamily));
            _TokenFamilyFromTokenType = new TokenFamily[types.Length];
            int family = 0;
            for (int tokenType = 0; tokenType < types.Length - 1; tokenType++)
            {
                if (family < (families.Length - 1) && tokenType == (int)families[family + 1]) family++;
                _TokenFamilyFromTokenType[tokenType] = families[family];
            }

            // Register the token strings corresponding to each token type (for keywords only)
            int keywordBegin = (int)TokenType.UserDefinedWord + 1;
            int keywordEnd = (int)TokenType.QUESTION_MARK - 1;
            _TokenStringFromTokenType = new string[types.Length];
            for (int c = keywordBegin; c <= keywordEnd; c++)
            {
                var current = types[c];
                _TokenStringFromTokenType[(int)current] = current.ToString().Replace('_', '-');
            }
            _TokenStringFromTokenType[(int)TokenType.ASTERISK_CBL] = "*CBL";
            _TokenStringFromTokenType[(int)TokenType.ASTERISK_CONTROL] = "*CONTROL";
            _TokenStringFromTokenType[(int)TokenType.DELETE_CD] = "DELETE";
            _TokenStringFromTokenType[(int)TokenType.SERVICE_CD] = "SERVICE";
            _TokenStringFromTokenType[(int)TokenType.EXEC_SQL] = "EXEC-SQL";

            // Same thing for SQL keywords
            keywordBegin = (int)TokenFamily.SqlFamily;
            keywordEnd = types.Length - 2; //EndOfFile is the last TokenType
            for (int c = keywordBegin; c <= keywordEnd; c++)
            {
                var current = types[c];
                string name = current.ToString();
                _TokenStringFromTokenType[(int)current] = name.Substring(4, name.Length - 4); //Remove 'SQL_' prefix
            }

            // Map token string to token type
            _TokenTypeFromTokenString = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);
            _SqlTokenTypeFromTokenString = new Dictionary<string, TokenType>(StringComparer.OrdinalIgnoreCase);
            for (int tokenType = 0; tokenType < _TokenStringFromTokenType.Length; tokenType++)
            {
                string tokenString = _TokenStringFromTokenType[tokenType];
                if (tokenString == null)
                {
                    continue;
                }

                if (_TokenFamilyFromTokenType[tokenType] == TokenFamily.SqlFamily)
                {
                    //SQL token
                    _SqlTokenTypeFromTokenString.Add(tokenString, (TokenType)tokenType);
                }
                else
                {
                    //Cobol token
                    _TokenTypeFromTokenString[tokenString] = (TokenType)tokenType;
                    //Token type DELETE is much more frequent than DELETE_CD, it should have priority
                    //Token type SERVICE is much more frequent than SERVICE_CD, it should have priority
                    //As DELETE and SERVICE appear after DELETE_CD and SERVICE_CD they overwrite previous values and are prioritized
                }
            }
        }

        public static TokenFamily GetTokenFamilyFromTokenType(TokenType tokenType)
        {
            if (tokenType == TokenType.EndOfFile) return TokenFamily.SyntaxSeparator;
            return _TokenFamilyFromTokenType[(int)tokenType];
        }

        public static string GetTokenStringFromTokenType(TokenType tokenType)
        {
            return _TokenStringFromTokenType[(int)tokenType];
        }

        internal static TokenType GetCobolKeywordTokenTypeFromTokenString(string tokenString, CobolLanguageLevel targetLanguageLevel)
        {
            if (_TokenTypeFromTokenString.TryGetValue(tokenString, out var tokenType))
            {
                if (targetLanguageLevel == CobolLanguageLevel.TypeCobol) return tokenType;

                var family = GetTokenFamilyFromTokenType(tokenType);
                if (targetLanguageLevel > CobolLanguageLevel.Cobol85)
                {
                    //Cobol2002 or Cobol2014 -> exclude TC keywords
                    return family == TokenFamily.TypeCobolKeyword ? TokenType.UserDefinedWord : tokenType;
                }

                //Cobol85 -> exclude TC and 2002 keywords
                return family == TokenFamily.TypeCobolKeyword || family == TokenFamily.Cobol2002Keyword ? TokenType.UserDefinedWord : tokenType;
            }

            return TokenType.UserDefinedWord;
        }

        internal static TokenType GetSqlKeywordTokenTypeFromTokenString(string tokenString)
        {
            if (_SqlTokenTypeFromTokenString.TryGetValue(tokenString, out var tokenType))
            {
                return tokenType;
            }

            return TokenType.UserDefinedWord;
        }

        // Formalized Comments only to avoid Formalized Comments tokens detection in Cobol and Cobol tokens in Formalized Comments
        internal static TokenType GetFormalComTokenTypeFromTokenString(string tokenString)
        {
            // The usual token detection method can not be applied because of the two possible keywords per tokenTypes
            TokenType tokenType;

            switch (tokenString.ToUpper())
            {
                case "DESCRIPTION":
                case "DESC":
                    tokenType = TokenType.FORMALIZED_COMMENTS_DESCRIPTION;
                    break;
                case "PARAMETERS":
                case "PARAMS":
                    tokenType = TokenType.FORMALIZED_COMMENTS_PARAMETERS;
                    break;
                case "DEPRECATED":
                case "DEPREC":
                    tokenType = TokenType.FORMALIZED_COMMENTS_DEPRECATED;
                    break;
                case "REPLACEDBY":
                case "REPLBY":
                    tokenType = TokenType.FORMALIZED_COMMENTS_REPLACED_BY;
                    break;
                case "RESTRICTION":
                case "RSTRIC":
                    tokenType = TokenType.FORMALIZED_COMMENTS_RESTRICTION;
                    break;
                case "NEED":
                    tokenType = TokenType.FORMALIZED_COMMENTS_NEED;
                    break;
                case "SEE":
                    tokenType = TokenType.FORMALIZED_COMMENTS_SEE;
                    break;
                case "TODO":
                    tokenType = TokenType.FORMALIZED_COMMENTS_TODO;
                    break;
                default:
                    tokenType = TokenType.UserDefinedWord;
                    break;
            }

            return tokenType;
        }

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
                case TokenFamily.SqlFamily:
                    return "Sql statement starting keyword";
                case TokenFamily.SpecialRegisterKeyword:
                    return "special register";
                case TokenFamily.FigurativeConstantKeyword:
                    return "figurative constant";
                case TokenFamily.SpecialObjectIdentifierKeyword:
                    return "special object identifier";
                case TokenFamily.SyntaxKeyword:
                case TokenFamily.CobolV6Keyword:
                case TokenFamily.Cobol2002Keyword:
                    return "keyword";
                case TokenFamily.TypeCobolKeyword:
                    return "TypeCobol keyword";
                case TokenFamily.TypeCobolOperators:
                    return "TypeCobol Operators";
                case TokenFamily.FormalizedCommentsFamily:
                    return "Formalized Comments elements";  // Can be keyword or open/close markup
                default:
                    return "...";
            }
        }

        public static string GetDisplayNameForTokenType(TokenType tokenType)
        {
            if ((int)TokenFamily.TypeCobolOperators > (int)tokenType && (int)tokenType >= (int)TokenFamily.CompilerDirectiveStartingKeyword && tokenType != TokenType.SymbolicCharacter)
            {
                return tokenType.ToString().Replace('_', '-');
            }
            
            if (_TokenFamilyFromTokenType[(int)tokenType] == TokenFamily.SqlFamily)
            {
                string name = tokenType.ToString();
                return name.Substring(4, name.Length - 4);
            }

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
                    return "DBCS literal";
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
                    return "section or paragraph name";
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
                case TokenType.QUESTION_MARK:
                    return "?";
                default:
                    return "...";
            }
        }
    }
}
