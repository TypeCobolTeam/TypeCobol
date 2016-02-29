using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Scanner
{
	internal static class TokenUtils {

		static TokenUtils() {
			// Map token types to token families
			var types = (TokenType[])Enum.GetValues(typeof(TokenType));
			var families = (TokenFamily[])Enum.GetValues(typeof(TokenFamily));

			tokenFamilyFromTokenType = new TokenFamily[types.Length];
			int tokenType = 0;
			int family = 0;
			while (tokenType<types.Length-1) {
				tokenFamilyFromTokenType[tokenType] = families[family];
				tokenType ++;
				if (tokenType == (int)families[family+1]) family++;
			}
			// Register the token strings corresponding to each token type (for keywords only)
			int max = (int)TokenType.CompilerDirective-1;
			tokenStringFromTokenType = new string[types.Length];
			for(int c=38; c<types.Length; c++) {
				var current = types[c];
				if ((int)current > max) break;
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
			for (tokenType = 0; tokenType < types.Length; tokenType++) {
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
