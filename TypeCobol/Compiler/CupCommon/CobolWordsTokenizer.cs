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
        /// With CS CUP real toke start at 0, 0 is for EOF and 1 for error.
        /// </summary>
        public const int CsCupStartToken = 2;

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
        /// Symbol Enumerator over Scanner.Token
        /// </summary>
        /// <returns></returns>
        public IEnumerator<Symbol> GetEnumerator()
        {
            Token token = null;
            while ((token = base.NextToken()) != Token.END_OF_FILE)
            {
                TUVienna.CS_CUP.Runtime.Symbol symbol = new TUVienna.CS_CUP.Runtime.Symbol(((int)token.TokenType) + CsCupStartToken - 1, token);
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
    }
}
