using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Special TokensLinesIterator that add fake tokens at the beginning of the stream
    /// and at the end. Used in direct copy parsing to wrap a Copy into a fake Program.
    /// </summary>
    public class WrapperTokensLinesIterator : ITokensLinesIterator
    {
        private readonly IEnumerator<Token> _before;
        private readonly ITokensLinesIterator _sourceIterator;
        private readonly IEnumerator<Token> _after;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="beforeTokens">A non-null enumerable of fake tokens to be returned before real tokens from source.</param>
        /// <param name="sourceIterator">Wrapped source iterator, must be non-null.</param>
        /// <param name="afterTokens">A non-null enumerable of fake tokens to be returned after real tokens from source.</param>
        /// <remarks>Use <see cref="Enumerable.Empty{Token}" /> on before or after to add only on one side of the enumeration.</remarks>
        public WrapperTokensLinesIterator([NotNull] IEnumerable<Token> beforeTokens, [NotNull] ITokensLinesIterator sourceIterator, [NotNull] IEnumerable<Token> afterTokens)
        {
            System.Diagnostics.Debug.Assert(beforeTokens != null);
            System.Diagnostics.Debug.Assert(sourceIterator != null);
            System.Diagnostics.Debug.Assert(afterTokens != null);

            _before = beforeTokens.GetEnumerator();
            _sourceIterator = sourceIterator;
            _after = afterTokens.GetEnumerator();
        }

        /// <summary>
        /// Return tokens from the source iterator preceded by fake tokens before
        /// and followed by fake tokens after. The EOF token always come last.
        /// </summary>
        /// <returns>Next token in sequence.</returns>
        public Token NextToken()
        {
            //return fake tokens before
            if (_before.MoveNext())
            {
                return _before.Current;
            }

            var nextToken = _sourceIterator.NextToken();

            //if we've reached the end of source stream, return fake tokens after
            if (Token.END_OF_FILE.Equals(nextToken) && _after.MoveNext())
            {
                return _after.Current;
            }

            return nextToken;
        }

        // Delegate the rest of the implementation to _sourceIterator

        public string DocumentPath => _sourceIterator.DocumentPath;

        public int LineIndexInMainDocument => _sourceIterator.LineIndexInMainDocument;

        public int ColumnIndex => _sourceIterator.ColumnIndex;

        public int LineIndex => _sourceIterator.LineIndex;

        public ITokensLine FirstLine => _sourceIterator.FirstLine;

        public ITokensLine CurrentLine => _sourceIterator.CurrentLine;

        public ITokensLine LastLine => _sourceIterator.LastLine;

        public Token CurrentToken => _sourceIterator.CurrentToken;

        public object GetCurrentPosition()
        {
            return _sourceIterator.GetCurrentPosition();
        }

        public void SeekToPosition(object iteratorPosition)
        {
            _sourceIterator.SeekToPosition(iteratorPosition);
        }

        public void SaveCurrentPositionSnapshot()
        {
            _sourceIterator.SaveCurrentPositionSnapshot();
        }

        public void ReturnToLastPositionSnapshot()
        {
            _sourceIterator.ReturnToLastPositionSnapshot();
        }
    }
}
