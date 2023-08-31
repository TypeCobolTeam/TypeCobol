using System.Collections.Generic;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Implements the REPLACE directives on top of an underlying tokens iterator 
    /// </summary>
    public class ReplaceTokensLinesIterator : AbstractReplaceTokensLinesIterator
    {
        private ReplaceDirective _currentReplaceDirective;

        /// <summary>
        /// Implement REPLACE directives on top of a CopyTokensLinesIterator
        /// </summary>
        public ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, TypeCobolOptions compilerOptions)
            : base(sourceIterator, compilerOptions)
        {
            _currentReplaceDirective = null;
        }

        //TODO ReplaceAndReplacing constructor only useful to detect replace affected by replacing
        internal ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, IReadOnlyList<ReplaceOperation> replaceOperations, TypeCobolOptions compilerOptions)
            : base(sourceIterator, replaceOperations, compilerOptions)
        {
            _currentReplaceDirective = null;//TODO ReplaceAndReplacing this is wrong as replaceOperations are set
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

                // Update current REPLACE
                _currentReplaceDirective = (ReplaceDirective)compilerDirectiveToken.CompilerDirective;

                if (compilerDirectiveToken.CompilerDirective.Type == CompilerDirectiveType.REPLACE)
                {
                    // Update replace operation for base iterator
                    updatedReplaceOperations = (IReadOnlyList<ReplaceOperation>)_currentReplaceDirective.ReplaceOperations;
                }
                else
                {
                    // A REPLACE OFF directive simply cancels the previous directive
                    _currentReplaceDirective = null;
                }

                nextToken = SourceIteratorNextToken();
            }

            // Set active REPLACE for line
            if (nextToken.TokenType != TokenType.EndOfFile)
            {
                ((CodeElementsLine)nextToken.TokensLine).ActiveReplaceDirective = _currentReplaceDirective;
            }

            return new CheckTokenStatus()
                   {
                       NextToken = nextToken,
                       UpdatedReplaceOperations = updatedReplaceOperations
                   };
        }

        public override void SeekToLineInMainDocument(int line)
        {
            base.SeekToLineInMainDocument(line);
            _currentReplaceDirective = ((CodeElementsLine)CurrentLine).ActiveReplaceDirective;
        }
    }
}
