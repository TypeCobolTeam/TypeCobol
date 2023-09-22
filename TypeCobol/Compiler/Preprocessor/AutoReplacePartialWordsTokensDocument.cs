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
        private class TokensLinesIterator : AbstractReplaceTokensLinesIterator
        {
            public TokensLinesIterator([NotNull] ITokensLinesIterator sourceIterator, [NotNull] TypeCobolOptions compilerOptions) : base(sourceIterator, compilerOptions)
            {
            }

            protected override CheckTokenStatus CheckNextTokenBeforeReplace(IReadOnlyList<ReplaceOperation> currentReplaceOperations)
            {
                var nextToken = SourceIteratorNextToken();

                // Reset previous replace operations
                List<ReplaceOperation> updatedReplaceOperations = null;
                if (nextToken.TokenType == TokenType.PartialCobolWord)
                {
                    //TODO Don't reset replace but first try to check if it's the same than before
                    updatedReplaceOperations = new List<ReplaceOperation>();
                    var replacementText =
                        new string(' ', nextToken.StartIndex) //Keep original spaces before first token
                        + nextToken.NormalizedText.Replace(":", string.Empty);
                    TokensLine tempTokensLine = TokensLine.CreateVirtualLineForInsertedToken(nextToken.TokensLine.LineIndex, replacementText, nextToken.TokensLine.ColumnsLayout);
                    var replacementToken = new Token(TokenType.UserDefinedWord, nextToken.StartIndex, tempTokensLine.Length - 1, tempTokensLine);

                    updatedReplaceOperations.Add(new SingleTokenReplaceOperation(nextToken, replacementToken, null, null));
                }

                return new CheckTokenStatus()
                {
                    NextToken = nextToken,
                    UpdatedReplaceOperations = updatedReplaceOperations
                };
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
