using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
	internal static class TokenUtils {

		static TokenUtils() {
			// Map token types to token families
			var types = (TokenType[])Enum.GetValues(typeof(TokenType));
			var families = (TokenFamily[])Enum.GetValues(typeof(TokenFamily));

			tokenFamilyFromTokenType = new TokenFamily[types.Length];
			int family = 0;
			for (int tokenType = 0; tokenType < types.Length-1; tokenType++) {
                if (family < (families.Length-1) && tokenType == (int)families[family + 1]) family++;
                tokenFamilyFromTokenType[tokenType] = families[family];				
			}
			// Register the token strings corresponding to each token type (for keywords only)
			int keywordBegin = (int)TokenType.UserDefinedWord+1;
			int keywordEnd = (int)TokenType.CompilerDirective-1;
			tokenStringFromTokenType = new string[types.Length];
			for(int c=keywordBegin; c<types.Length; c++) {
				var current = types[c];
				if ((int)current > keywordEnd) break;
				tokenStringFromTokenType[(int)current] = current.ToString().Replace('_','-');
			}
			tokenStringFromTokenType[(int)TokenType.ASTERISK_CBL] = "*CBL";
			tokenStringFromTokenType[(int)TokenType.ASTERISK_CONTROL] = "*CONTROL";
			tokenStringFromTokenType[(int)TokenType.DELETE_CD] = "DELETE";
			tokenStringFromTokenType[(int)TokenType.EXEC_SQL_INCLUDE] = null;
			tokenStringFromTokenType[(int)TokenType.DISPLAY_ARG] = "DISPLAY";
			tokenStringFromTokenType[(int)TokenType.ENTRY_ARG] = "ENTRY";
			tokenStringFromTokenType[(int)TokenType.SORT_ARG] = "SORT";

			// Map token string to token type
			tokenTypeFromTokenString = new Dictionary<string, TokenType>(types.Length-1, StringComparer.OrdinalIgnoreCase);
			for (int tokenType = 0; tokenType < types.Length; tokenType++) {
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

        public static Regex COBOL_INTRINSIC_FUNCTIONS = new Regex("^(ACOS|ANNUITY|ASIN|ATAN|CHAR|COS|CURRENT-DATE|DATE-OF-INTEGER|DATE-TO-YYYYMMDD|DAY-OF-INTEGER|DAY-TO-YYYYDDD|DISPLAY-OF|FACTORIAL|INTEGER|INTEGER-OF-DATE|INTEGER-OF-DAY|INTEGER-PART|LENGTH|LOG|LOG10|LOWER-CASE|MAX|MEAN|MEDIAN|MIDRANGE|MIN|MOD|NATIONAL-OF|NUMVAL|NUMVAL-C|ORD|ORD-MAX|ORD-MIN|PRESENT-VALUE|RANDOM|RANGE|REM|REVERSE|SIN|SQRT|STANDARD-DEVIATION|SUM|TAN|ULENGTH|UPOS|UPPER-CASE|USUBSTR|USUPPLEMENTARY|UVALID|UWIDTH|VARIANCE|WHEN-COMPILED|YEAR-TO-YYYY)$", RegexOptions.IgnoreCase | RegexOptions.Compiled);
    }
}
