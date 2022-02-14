using System;
using System.Collections.Generic;
using System.Linq;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Parser;

namespace TypeCobol.Compiler.CupParser
{
    /// <summary>
    /// Return CodeElement from a set of lines in sequence as Cup symbols.
    /// Optionally add fake CodeElements at the beginning and at the end.
    /// </summary>
    public class CodeElementTokenizer : TUVienna.CS_CUP.Runtime.Scanner
    {
        /// <summary>
        /// With CS CUP real tokens start at 3, 0 is for EOF, 1 is for error,
        /// and 2 is for the StatementStart terminal symbol.
        /// </summary>
        private const int CUP_TOKEN_OFFSET = 2;

        private static Symbol ToSymbol(CodeElement codeElement) => new Symbol((int)codeElement.Type + CUP_TOKEN_OFFSET, codeElement);

        public static string CupTokenToString(int token) => Enum.GetName(typeof(CodeElementType), token - CUP_TOKEN_OFFSET);

        private readonly IEnumerator<Symbol> _enumerator;

        /// <summary>
        /// Create a new CodeElementTokenizer.
        /// </summary>
        /// <param name="lines">CodeElement lines to enumerate as symbols.</param>
        /// <param name="before">Optional fake CodeElements to enumerate before real ones.</param>
        /// <param name="after">Optional fake CodeElements to enumerate after real ones.</param>
        public CodeElementTokenizer(IEnumerable<CodeElementsLine> lines, IEnumerable<CodeElement> before = null, IEnumerable<CodeElement> after = null)
            : this(before?.Select(ToSymbol), lines, after?.Select(ToSymbol))
        {

        }

        /// <summary>
        /// Special constructor used for error recovery.
        /// Will enumerate the supplied start symbol followed by the supplied CodeElement.
        /// </summary>
        /// <param name="start">Start symbol id.</param>
        /// <param name="codeElement">CodeElement to return in second position.</param>
        public CodeElementTokenizer(int start, CodeElement codeElement)
            : this(new []
                   {
                       new Symbol(start, null),
                       ToSymbol(codeElement)
                   }, null, null)
        {

        }

        private CodeElementTokenizer(IEnumerable<Symbol> before, IEnumerable<CodeElementsLine> lines, IEnumerable<Symbol> after)
        {
            _enumerator = Enumerator(before, lines, after);
        }

        public Symbol next_token()
        {
            if (_enumerator.MoveNext())
            {
                return _enumerator.Current;
            }

            return null;
        }

        private IEnumerator<Symbol> Enumerator(IEnumerable<Symbol> before, IEnumerable<CodeElementsLine> lines, IEnumerable<Symbol> after)
        {
            //symbols before
            if (before != null)
            {
                foreach (var symbol in before)
                {
                    yield return symbol;
                }
            }

            //symbols from lines
            if (lines != null)
            {
                foreach (var line in lines)
                {
                    if (line.HasCodeElements)
                    {
                        foreach (var codeElement in line.CodeElements)
                        {
                            yield return ToSymbol(codeElement);
                        }
                    }
                }
            }

            //symbols after
            if (after != null)
            {
                foreach (var symbol in after)
                {
                    yield return symbol;
                }
            }
        }
    }
}
