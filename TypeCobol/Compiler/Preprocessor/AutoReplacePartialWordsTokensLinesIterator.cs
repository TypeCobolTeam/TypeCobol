using System;
using JetBrains.Annotations;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Special TokensLinesIterator that auto-replace PartialCobolWords tokens based on an external replacing strategy.
    /// Used in direct copy parsing to transform remaining partial words into valid tokens.
    /// </summary>
    public class AutoReplacePartialWordsTokensLinesIterator : ITokensLinesIterator
    {
        private readonly ITokensLinesIterator _sourceIterator;
        private readonly Func<Token, Token> _generateReplacement;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="sourceIterator">Wrapped source iterator, must be non-null.</param>
        /// <param name="generateReplacement">A non-null delegate to a token replacement method.
        /// Supplied token is guaranteed to be PartialCobolWord, returned token is the generated replacement for it.
        /// Returned token must be non-null, no token skipping allowed !.</param>
        /// <remarks>Generated token is wrapped into a <see cref="ReplacedPartialCobolWord"/> in resulting stream.</remarks>
        public AutoReplacePartialWordsTokensLinesIterator([NotNull] ITokensLinesIterator sourceIterator, [NotNull] Func<Token, Token> generateReplacement)
        {
            System.Diagnostics.Debug.Assert(sourceIterator != null);
            System.Diagnostics.Debug.Assert(sourceIterator != null);

            _sourceIterator = sourceIterator;
            _generateReplacement = generateReplacement;
        }

        public Token NextToken()
        {
            var nextToken = _sourceIterator.NextToken();
            if (nextToken.TokenType == TokenType.PartialCobolWord)
            {
                //Perform automatic token replacement
                var originalToken = nextToken;
                var generatedReplacementToken = _generateReplacement(originalToken);
                nextToken = new ReplacedPartialCobolWord(generatedReplacementToken, null, originalToken);
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
