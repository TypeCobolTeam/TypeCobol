using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.LanguageServer
{
    public static class CompletionElligibleTokens
    {
        /// <summary>
        /// Completion Tokens is an array of Tuple<TokenType, AllowLastPos> for each token that can target a completion.
        /// The Flag AllowLastPos says that if the cursor is at the last position of a token that the token can
        /// be conidered a completion token. For instance the for PERFORM token the last position is not allowed:
        /// PERFORM
        ///        ^
        /// that is to say if the cursor is just after the M that no completion should occurs.
        /// </summary>
        private static Tuple<TokenType, bool>[] elligibleCompletionTokens = {
            new Tuple<TokenType, bool>(TokenType.PERFORM, false),
            new Tuple<TokenType, bool>(TokenType.CALL, false),
            new Tuple<TokenType, bool>(TokenType.TYPE, false),
            new Tuple<TokenType, bool>(TokenType.QualifiedNameSeparator, true),
            new Tuple<TokenType, bool>(TokenType.INPUT, false),
            new Tuple<TokenType, bool>(TokenType.OUTPUT, false),
            new Tuple<TokenType, bool>(TokenType.IN_OUT, false),
            new Tuple<TokenType, bool>(TokenType.MOVE, false),
            new Tuple<TokenType, bool>(TokenType.TO, false),
            new Tuple<TokenType, bool>(TokenType.IF, false),
            new Tuple<TokenType, bool>(TokenType.DISPLAY, false),
            new Tuple<TokenType, bool>(TokenType.SET, false),
            new Tuple<TokenType, bool>(TokenType.OF, false),
        };

        /// <summary>
        /// Determines if the given token is elligible for a completion
        /// </summary>
        /// <param name="token"></param>
        /// <param name="bAllowLastPos"></param>
        /// <returns></returns>
        public static bool IsCompletionElligibleToken(Token token)
        {
            for (int i = 0; i < elligibleCompletionTokens.Length; i++)
            {
                if (elligibleCompletionTokens[i].Item1 == token.TokenType)
                    return true;
            }
            return false;
        }

        public static bool DoesTokenAllowLastPos(Token token)
        {
            var potentialToken = elligibleCompletionTokens.FirstOrDefault(t => t.Item1 == token.TokenType);
            if (potentialToken != null)
                return potentialToken.Item2;
            else
                return false;
        }
    }
}
