#nullable enable

using TypeCobol.Compiler.CupCommon;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CupPreprocessor
{
    /// <summary>
    /// Build Replace operations for both COPY REPLACING and REPLACE statements
    /// using a CupReplaceOperation instance received from CUP parser.
    /// </summary>
    internal class ReplaceOperationBuilder
    {
        private const string EMPTY_COMPARISON = "\"REPLACE\" Empty Comparison Pseudo Text.";
        private const string INVALID_LEADING_TRAILING = "\"LEADING\" and \"TRAILING\" can only be used to replace one single text word. This REPLACE operation cannot be applied and is discarded.";

        private readonly CompilerDirectiveBuilder _directiveBuilder;

        public ReplaceOperationBuilder(CompilerDirectiveBuilder directiveBuilder)
        {
            _directiveBuilder = directiveBuilder;
        }

        public void BuildFromCupReplaceOperation(IList<ReplaceOperation> operations, CupReplaceOperation cupReplaceOperation, Token copyOrReplace)
        {
            Token? comparisonToken = null;
            Token[]? followingComparisonTokens = null;
            Token? replacementToken = null;
            Token[]? replacementTokens = null;

            // Capture comparison tokens
            var comparisonTokens = cupReplaceOperation.From;
            if (comparisonTokens != null && comparisonTokens.Count > 0)
            {
                comparisonToken = comparisonTokens[0];
                if (comparisonTokens.Count > 1)
                {
                    followingComparisonTokens = new Token[comparisonTokens.Count - 1];
                    for (int i = 1; i < comparisonTokens.Count; i++)
                    {
                        followingComparisonTokens[i - 1] = comparisonTokens[i];
                    }
                }
            }
            else
            {
                // It cannot be empty, add error
                AddError(copyOrReplace, EMPTY_COMPARISON);
            }

            // Capture replacementToken(s)
            var operandTokens = cupReplaceOperation.By;
            if (operandTokens != null && operandTokens.Count > 0)
            {
                if (followingComparisonTokens == null && operandTokens.Count == 1)
                {
                    replacementToken = operandTokens[0];
                }
                else
                {
                    replacementTokens = operandTokens.Where(t => t.TokenFamily != TokenFamily.Comments).ToArray();
                }
            }

            // Build replace operation
            ReplaceOperation? replaceOperation = null;
            var leading = cupReplaceOperation.Leading;
            var trailing = cupReplaceOperation.Trailing;
            if (followingComparisonTokens == null)
            {
                if (replacementTokens == null)
                {
                    if (comparisonToken == null || comparisonToken.TokenType != TokenType.PartialCobolWord)
                    {
                        replaceOperation = new SingleTokenReplaceOperation(comparisonToken, replacementToken, leading, trailing);
                    }
                    else if (leading == null && trailing == null)
                    {
                        replaceOperation = new PartialWordReplaceOperation(comparisonToken, replacementToken);
                    }
                    else
                    {
                        // LEADING and TRAILING forbidden here
                        AddError(comparisonToken, INVALID_LEADING_TRAILING);
                    }
                }
                else if (leading == null && trailing == null)
                {
                    replaceOperation = new SingleToMultipleTokensReplaceOperation(comparisonToken, replacementTokens);
                }
                else
                {
                    // LEADING and TRAILING forbidden here
                    AddError(replacementTokens[0], INVALID_LEADING_TRAILING);
                }
            }
            else if (leading == null && trailing == null)
            {
                replaceOperation = new MultipleTokensReplaceOperation(comparisonToken, followingComparisonTokens, replacementTokens);
            }
            else
            {
                // LEADING and TRAILING forbidden here
                var errorToken = comparisonToken ?? leading ?? trailing;
                AddError(errorToken, INVALID_LEADING_TRAILING);
            }

            // Add to given container
            if (replaceOperation != null)
            {
                operations.Add(replaceOperation);
            }

            void AddError(Token token, string message)
            {
                Diagnostic error = new Diagnostic(MessageCode.SyntaxErrorInParser, token.Position(), message);
                _directiveBuilder.CompilerDirective.AddParsingDiagnostic(error);
            }
        }
    }
}
