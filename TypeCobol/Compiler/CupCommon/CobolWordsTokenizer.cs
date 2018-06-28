using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CupCommon
{
    /// <summary>
    /// Cobol Words Tokenizer to be used with CUP parser.
    /// </summary>
    public class CobolWordsTokenizer : TokensLinesIterator, TUVienna.CS_CUP.Runtime.Scanner, IEnumerable<TUVienna.CS_CUP.Runtime.Symbol>, IEnumerator<TUVienna.CS_CUP.Runtime.Symbol>
    {
        /// <summary>
        /// With CS CUP real toke start at 0, 0 is for EOF and 1 for error
        /// and we have introduced the CUP_ANY_TOKEN token.
        /// </summary>
        public const int CsCupStartToken = 3;
        
        /// <summary>
        /// The ID od the Any Token
        /// </summary>
        public const int ANY_TOKEN = 2;

        /// <summary>
        /// The EOF symbol
        /// </summary>
        public static TUVienna.CS_CUP.Runtime.Symbol EOF => new TUVienna.CS_CUP.Runtime.Symbol(0, null);

        /// <summary>
        /// Internal Symbol Yielder
        /// </summary>
        private IEnumerator<TUVienna.CS_CUP.Runtime.Symbol> _symbolYielder;

        /// <summary>
        /// Constructor à la TokensLinesIterator
        /// </summary>
        /// <param name="textName"></param>
        /// <param name="tokensLines"></param>
        /// <param name="startToken"></param>
        /// <param name="channelFilter"></param>
        public CobolWordsTokenizer(string textName, ISearchableReadOnlyList<ITokensLine> tokensLines, Token startToken, int channelFilter) : base(textName, tokensLines, startToken, channelFilter)
        {
            Reset();
        }

        /// <summary>
        /// Next Symbol for Cup Scanner
        /// </summary>
        /// <returns></returns>
        public Symbol next_token()
        {
            if (_symbolYielder.MoveNext())
                return _symbolYielder.Current;
            return EOF;
        }

        /// <summary>
        /// Is the Tokenizer in the Any Token Nmode ?
        /// </summary>
        public bool IsAnyTokenMode { get; private set; }
        /// <summary>
        /// Excluded Tokens from the Current Any Token Mode.
        /// </summary>
        private TokenType[] ExcludedTokens { get; set; }

        /// <summary>
        /// Call this method to enter in the any token mode
        /// </summary>
        /// <param name="excluded">Excludes Tokens from the Any Toke Mode</param>
        public void EnterAnyTokenMode(params TokenType[] excluded)
        {
            ExcludedTokens = excluded;
            if (excluded != null && excluded.Length != 0)
                IsAnyTokenMode = true;
        }

        /// <summary>
        /// Consume the given token type if it is the next token type at the same line than the current Token.
        /// </summary>
        /// <param name="nextTokenType">The next token type to check</param>
        public void ConsumeNextTokenOnTheSameLine(TokenType nextTokenType)
        {
            Token currentToken = base.CurrentToken;
            if (currentToken == Token.END_OF_FILE)
                return;//Ignore if end of file
            Token nextToken = base.NextToken();
            if (nextToken != null && currentToken != null &&
                nextToken.TokensLine == currentToken.TokensLine && nextToken.TokenType == nextTokenType)
            {
                return;//Consume it
            }
            else if (nextToken != null && currentToken != null)
            {//Rollback
                base.PreviousToken();
            }
        }

        /// <summary>
        /// Symbol Enumerator over Scanner.Token
        /// </summary>
        /// <returns></returns>
        public IEnumerator<Symbol> GetEnumerator()
        {
            Token token = null;
            while ((token = base.NextToken()) != Token.END_OF_FILE)
            {
                TUVienna.CS_CUP.Runtime.Symbol symbol = new TUVienna.CS_CUP.Runtime.Symbol(((int)token.TokenType) + CsCupStartToken - 1, token);
                if (IsAnyTokenMode)
                {
                    if (ExcludedTokens.Contains(token.TokenType))
                    {   //Leave the mode, the token is an excluded token.
                        IsAnyTokenMode = false;
                    }
                    else
                    {   //Change the symbol to the any token symbol.
                        symbol.sym = ANY_TOKEN;
                    }
                }
                yield return symbol;
            }
            yield return EOF;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public void Dispose()
        {
            _symbolYielder = null;
        }

        /// <summary>
        /// Move to the Next Symbol
        /// </summary>
        /// <returns>true if there is Symbol, false otherwise.</returns>
        public bool MoveNext()
        {
            return _symbolYielder != null && _symbolYielder.MoveNext();
        }

        /// <summary>
        /// Reset the Tokenizer
        /// </summary>
        public new void Reset()
        {
            base.Reset();
            _symbolYielder = GetEnumerator();
        }

        /// <summary>
        /// The Current Symbol
        /// </summary>
        public TUVienna.CS_CUP.Runtime.Symbol Current => _symbolYielder.Current;

        object IEnumerator.Current
        {
            get { return Current; }
        }

        /// <summary>
        /// Get the string representation of a CodeElementType
        /// </summary>
        /// <param name="ceType"></param>
        /// <returns></returns>
        public static string ToString(TokenType ceType)
        {
            return TokenUtils.GetDisplayNameForTokenType(ceType);
        }

        /// <summary>
        /// Get the string representation of the CodeElementType correponding to a Cup Token.
        /// </summary>
        /// <param name="token"></param>
        /// <returns></returns>
        public static string CupTokenToString(int token)
        {
            return ToString((TokenType)(token - CsCupStartToken + 1));
        }
    }
}
