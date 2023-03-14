using System;
using System.Linq;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Preprocessor
{
    public class ReplaceTokensLinesIterator2 : AbstractReplaceTokensLinesIterator
    {
        protected override CheckTokenStatus CheckTokenBeforeReplace(Func<Token> getNextToken, ReplaceOperation[] currentReplaceOperations)
        {
            var nextToken = getNextToken();
            ReplaceOperation[] updatedReplaceOperations = currentReplaceOperations;

            // If the next token is a REPLACE directive, update the current replace operations in effect
            while (nextToken.TokenType == TokenType.REPLACE_DIRECTIVE)
            {
                // Reset previous replace operations
                updatedReplaceOperations = null;

                // A REPLACE OFF directive simply cancels the previous directive 
                if (((CompilerDirectiveToken)nextToken).CompilerDirective.Type == CompilerDirectiveType.REPLACE)
                {
                    var replaceDirective = (ReplaceDirective)((CompilerDirectiveToken)nextToken).CompilerDirective;
                    updatedReplaceOperations = replaceDirective.ReplaceOperations.ToArray();
                }
                // else: it is a REPLACE OFF, update replace operations is null

                nextToken = getNextToken();
            }

            return new CheckTokenStatus()
                   {
                       ApplyReplace = true,
                       NextToken = nextToken,
                       UpdatedReplaceOperations = updatedReplaceOperations
                   };
        }
    }

    public class AutoReplace : AbstractReplaceTokensLinesIterator
    {
        protected override CheckTokenStatus CheckTokenBeforeReplace(Func<Token> getNextToken, ReplaceOperation[] currentReplaceOperations)
        {
            var nextToken = getNextToken();
            if (nextToken.TokenType == TokenType.PartialCobolWord)
            {
                //basic replacement mechanic, remove the ':' from the tag.
                //NOTE: Altered token is scanned as if it was located at the beginning of the line because we only have InitialScanState here.
                //NOTE: Does not handle '::-item' or 'item-::' partial names as '::' will turn into empty string and will produce invalid data names.
                var originalToken = nextToken;
                string replacedTokenText = originalToken.NormalizedText.Replace(":", string.Empty);
                var scanState = originalToken.TokensLine.InitialScanState;
                var generatedReplacementToken = GenerateReplacementToken(originalToken, replacedTokenText, scanState, _compilerOptions);

                nextToken = new ReplacedPartialCobolWord(generatedReplacementToken, null, originalToken);
            }

            return new CheckTokenStatus()
                   {
                       ApplyReplace = false,
                       NextToken = nextToken,
                       UpdatedReplaceOperations = currentReplaceOperations
                   };
        }
    }

    public class Replacing : AbstractReplaceTokensLinesIterator
    {
        private readonly CopyDirective _copyReplacingDirective;

        protected override CheckTokenStatus CheckTokenBeforeReplace(Func<Token> getNextToken, ReplaceOperation[] currentReplaceOperations)
        {
            
        }
    }
}
