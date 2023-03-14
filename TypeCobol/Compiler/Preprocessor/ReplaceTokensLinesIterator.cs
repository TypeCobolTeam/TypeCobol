using System.Collections.Generic;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Implements the REPLACE directives on top of an underlying tokens iterator 
    /// </summary>
    public class ReplaceTokensLinesIterator : AbstractReplaceTokensLinesIterator
    {
        /// <summary>
        /// Implement REPLACE directives on top of a CopyTokensLinesIterator
        /// </summary>
        public ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, TypeCobolOptions compilerOptions)
            : base(sourceIterator, compilerOptions)
        {

        }

        protected override CheckTokenStatus CheckNextTokenBeforeReplace(IReadOnlyList<ReplaceOperation> currentReplaceOperations)
        {
            var nextToken = SourceIteratorNextToken();
            var updatedReplaceOperations = currentReplaceOperations;

            // If the next token is a REPLACE directive, update the current replace operations in effect
            while (nextToken.TokenType == TokenType.REPLACE_DIRECTIVE)
            {
                // Reset previous replace operations
                updatedReplaceOperations = null;

                // Token may come from main document or a copy...
                CompilerDirectiveToken compilerDirectiveToken = nextToken is ImportedToken importedToken
                    ? (CompilerDirectiveToken)importedToken.OriginalToken
                    : (CompilerDirectiveToken)nextToken;

                // A REPLACE OFF directive simply cancels the previous directive 
                if (compilerDirectiveToken.CompilerDirective.Type == CompilerDirectiveType.REPLACE)
                {
                    var replaceDirective = (ReplaceDirective)compilerDirectiveToken.CompilerDirective;
                    updatedReplaceOperations = (IReadOnlyList<ReplaceOperation>)replaceDirective.ReplaceOperations;
                }
                // else: it is a REPLACE OFF, update replace operations is null

                nextToken = SourceIteratorNextToken();
            }

            return new CheckTokenStatus()
                   {
                       ApplyReplace = updatedReplaceOperations != null,
                       NextToken = nextToken,
                       UpdatedReplaceOperations = updatedReplaceOperations
                   };
        }
    }
}
