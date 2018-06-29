using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.CupParser
{
    /// <summary>
    /// The Code Element Tokenizer for CS Cup Symbol.
    /// </summary>
    public class CodeElementTokenizer : TUVienna.CS_CUP.Runtime.Scanner, IEnumerable<TUVienna.CS_CUP.Runtime.Symbol>, IEnumerator<TUVienna.CS_CUP.Runtime.Symbol>
    {
        private const int NSTARTS = 1;
        /// <summary>
        /// With CS CUP real toke start at 3, 0 is for EOF and 1 for error.
        /// 2 is for the StatementStart terminal symbol
        /// </summary>
        public const int CS_CUP_START_TOKEN = 2 + NSTARTS;
        /// <summary>
        /// The EOF symbol
        /// </summary>
        public static TUVienna.CS_CUP.Runtime.Symbol EOF => new TUVienna.CS_CUP.Runtime.Symbol(0, null);

        /// <summary>
        /// Current code element line index
        /// </summary>
        private int m_CodeElementsLineIndex;
        /// <summary>
        /// Current Code Element Index inside a Code Element Line index
        /// </summary>
        private int m_CodeElementIndex;
        /// <summary>
        /// The list of Code Elements
        /// </summary>
        public ISearchableReadOnlyList<CodeElementsLine> CodeElementsLines
        {
            get;
            internal set;
        }

        /// <summary>
        /// Any Start Token
        /// </summary>
        private int StartToken
        {
            get; set;
        }

        /// <summary>
        /// Any first Code Eelement
        /// </summary>
        private CodeElement[] FirstCodeElements
        {
            get; set;
        }

        /// <summary>
        /// Internal Symbol Yielder
        /// </summary>
        private IEnumerator<TUVienna.CS_CUP.Runtime.Symbol> symbol_yielder;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="codeElementsLines">The List of Code Elements</param>
        public CodeElementTokenizer(ISearchableReadOnlyList<CodeElementsLine> codeElementsLines)
        {
            StartToken = -1;
            this.CodeElementsLines = codeElementsLines;
            Reset();
        }

        /// <summary>
        /// Copy constructor.
        /// </summary>
        /// <param name="from">From which to copy the Tokenizer in the same state.</param>
        public CodeElementTokenizer(CodeElementTokenizer from) : this(-1, from)
        {
        }

        /// <summary>
        /// Copy constructor with a start symbol.
        /// </summary>
        /// <param name="start">From which to copy the Tokenizer in the same state.</param>
        /// <param name="from">From which to copy the Tokenizer in the same state.</param>
        public CodeElementTokenizer(int start, CodeElementTokenizer from) : this(start, from, null)
        {
        }

        /// <summary>
        /// Copy constructor with a start symbol and a first CodeElement.
        /// </summary>
        /// <param name="start">From which to copy the Tokenizer in the same state.</param>
        /// <param name="firstCE">The first code elemnt</param>
        /// <param name="from">From which to copy the Tokenizer in the same state.</param>
        public CodeElementTokenizer(int start, CodeElementTokenizer from, params CodeElement[] firstCE)
        {
            StartToken = start;
            FirstCodeElements = firstCE;
            this.CodeElementsLines = from.CodeElementsLines;
            m_CodeElementsLineIndex = from.m_CodeElementsLineIndex;
            m_CodeElementIndex = from.m_CodeElementIndex;
            Reset();
        }

        /// <summary>
        /// Singleton Constructor
        /// </summary>
        /// <param name="codeElementsLines">The List of Code Elements</param>
        public CodeElementTokenizer(int start, params CodeElement[] firstCE)
        {
            StartToken = start;
            FirstCodeElements = firstCE;
            this.CodeElementsLines = null;
            Reset();
        }

        public TUVienna.CS_CUP.Runtime.Symbol next_token()
        {
            if (symbol_yielder.MoveNext())
                return symbol_yielder.Current;
            return EOF;
        }

        /// <summary>
        /// The Last Symbol return by the Tokenizer. This one can be used for reporting errors.
        /// </summary>
        public TUVienna.CS_CUP.Runtime.Symbol LastSymbol
        {
            get;
            private set;
        }

        /// <summary>
        /// Enumerator all Symbol from the CodeElementLines
        /// </summary>
        /// <returns>An Enumerator on Symbols</returns>
        public IEnumerator<TUVienna.CS_CUP.Runtime.Symbol> GetEnumerator()
        {
            if (StartToken >= 0)
            {
                TUVienna.CS_CUP.Runtime.Symbol start_symbol = new TUVienna.CS_CUP.Runtime.Symbol(StartToken, null);
                yield return start_symbol;
            }
            if (FirstCodeElements != null)
            {
                foreach (CodeElement ce in FirstCodeElements)
                {
                    if (ce != null)
                    {
                        TUVienna.CS_CUP.Runtime.Symbol symbol = new TUVienna.CS_CUP.Runtime.Symbol(((int)ce.Type) + CS_CUP_START_TOKEN - 1, ce);
                        LastSymbol = symbol;
                        yield return symbol;
                    }

                }
            }
            if (CodeElementsLines != null)
            {
                int celCount = CodeElementsLines.Count;
                for (; m_CodeElementsLineIndex < celCount; m_CodeElementsLineIndex++)
                {
                    CodeElementsLine cel = CodeElementsLines[m_CodeElementsLineIndex];
                    if (cel.CodeElements != null)
                    {
                        int ceCount = cel.CodeElements.Count;
                        for (; m_CodeElementIndex < ceCount; m_CodeElementIndex++)
                        {
                            CodeElement ce = cel.CodeElements[m_CodeElementIndex];
                            TUVienna.CS_CUP.Runtime.Symbol symbol =
                                new TUVienna.CS_CUP.Runtime.Symbol(((int)ce.Type) + CS_CUP_START_TOKEN - 1, ce);
                            LastSymbol = symbol;
                            yield return symbol;
                        }
                        m_CodeElementIndex = 0;
                    }
                }
            }
            m_CodeElementsLineIndex = 0;
            yield return EOF;
        }

        /// <summary>
        /// Get the string representation of a CodeElementType
        /// </summary>
        /// <param name="ceType"></param>
        /// <returns></returns>
        public static string ToString(CodeElementType ceType)
        {
            string name = System.Enum.GetName(typeof(CodeElementType), ceType);
            return name;
        }

        /// <summary>
        /// Get the string representation of the CodeElementType correponding to a Cup Token.
        /// </summary>
        /// <param name="token"></param>
        /// <returns></returns>
        public static string CupTokenToString(int token)
        {
            return ToString((CodeElementType)(token - CS_CUP_START_TOKEN + 1));
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }

        public TUVienna.CS_CUP.Runtime.Symbol Current => symbol_yielder.Current;

        public void Dispose()
        {
            symbol_yielder = null;
        }

        object System.Collections.IEnumerator.Current => symbol_yielder.Current;

        public bool MoveNext()
        {
            return symbol_yielder != null && symbol_yielder.MoveNext();
        }

        public void Reset()
        {
            symbol_yielder = GetEnumerator();
        }
    }
}
