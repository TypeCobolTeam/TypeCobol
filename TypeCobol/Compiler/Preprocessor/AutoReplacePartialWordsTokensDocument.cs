using JetBrains.Annotations;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Special ProcessedTokensDocument that auto replaces all PartialCobolWords.
    /// Used in direct copy parsing to transform remaining partial words into valid tokens.
    /// </summary>
    public class AutoReplacePartialWordsTokensDocument : ProcessedTokensDocument
    {
        private class TokensLinesIterator : ITokensLinesIterator
        {
            private readonly ITokensLinesIterator _sourceIterator;
            private readonly TypeCobolOptions _compilerOptions;

            public TokensLinesIterator([NotNull] ITokensLinesIterator sourceIterator, [NotNull] TypeCobolOptions compilerOptions)
            {
                System.Diagnostics.Debug.Assert(sourceIterator != null);
                System.Diagnostics.Debug.Assert(compilerOptions != null);
                _sourceIterator = sourceIterator;
                _compilerOptions = compilerOptions;
            }

            public Token NextToken()
            {
                var nextToken = _sourceIterator.NextToken();
                if (nextToken.TokenType == TokenType.PartialCobolWord)
                {
                    //basic replacement mechanic, remove the ':' from the tag.
                    //NOTE: PartialCobolWord have their ScanStateSnapshot embedded so we can use it to replace with number literal after OCCURS if need be...
                    //NOTE: Does not handle '::-item' or 'item-::' partial names as '::' will turn into empty string and will produce invalid data names.
                    var originalToken = nextToken;
                    string replacedTokenText = originalToken.NormalizedText.Replace(":", string.Empty);
                    var generatedReplacementToken = ReplaceTokensLinesIterator.GenerateReplacementToken(originalToken, replacedTokenText, _compilerOptions);

                    nextToken = new ReplacedPartialCobolWord(generatedReplacementToken, null, originalToken);
                }

                return nextToken;
            }

            // Delegate the rest of the implementation to _sourceIterator

            public string DocumentPath => _sourceIterator.DocumentPath;

            public int LineIndexInMainDocument => _sourceIterator.LineIndexInMainDocument;

            public int ColumnIndex => _sourceIterator.ColumnIndex;

            public int LineIndex => _sourceIterator.LineIndex;

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

            public void SeekToLineInMainDocument(int line)
            {
                _sourceIterator.SeekToLineInMainDocument(line);
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

        private readonly TypeCobolOptions _compilerOptions;

        public AutoReplacePartialWordsTokensDocument(TokensDocument previousStepSnapshot, DocumentVersion<IProcessedTokensLine> processedTokensLinesVersion, ISearchableReadOnlyList<CodeElementsLine> processedTokensLines, TypeCobolOptions compilerOptions)
            : base(previousStepSnapshot, processedTokensLinesVersion, processedTokensLines, compilerOptions)
        {
            _compilerOptions = compilerOptions;
        }

        public override ITokensLinesIterator GetProcessedTokensIterator() => new TokensLinesIterator(base.GetProcessedTokensIterator(), _compilerOptions);
    }
}
