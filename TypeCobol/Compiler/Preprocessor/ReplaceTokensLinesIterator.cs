using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

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
        public ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, TypeCobolOptions compilerOptions) : base(sourceIterator, compilerOptions)
        {
        }

        protected override CheckTokenStatus CheckTokenBeforeReplace(Func<Token> getNextToken, IReadOnlyList<ReplaceOperation> currentReplaceOperations)
        {
            var nextToken = getNextToken();
            var updatedReplaceOperations = currentReplaceOperations;

            // If the next token is a REPLACE directive, update the current replace operations in effect
            while (nextToken.TokenType == TokenType.REPLACE_DIRECTIVE)
            {
                // Reset previous replace operations
                updatedReplaceOperations = null;

                // A REPLACE OFF directive simply cancels the previous directive 
                if (((CompilerDirectiveToken)nextToken).CompilerDirective.Type == CompilerDirectiveType.REPLACE)
                {
                    var replaceDirective = (ReplaceDirective)((CompilerDirectiveToken)nextToken).CompilerDirective;
                    updatedReplaceOperations = (IReadOnlyList<ReplaceOperation>)replaceDirective.ReplaceOperations;
                }
                // else: it is a REPLACE OFF, update replace operations is null

                nextToken = getNextToken();
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
