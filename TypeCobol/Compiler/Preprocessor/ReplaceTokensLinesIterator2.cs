using System;
using System.Collections.Generic;
using System.Linq;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{

    /*
    public class AutoReplace : AbstractReplaceTokensLinesIterator
    {
        protected override CheckTokenStatus CheckTokenBeforeReplace(Func<Token> getNextToken, IReadOnlyList<ReplaceOperation> currentReplaceOperations)
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
    }*/

    /// <summary>
    /// Handle replacing clause and specific replacement for EUROINFO_RULES.
    /// Replacing clause is applied before Replace clause.
    /// </summary>
    public class Replacing : AbstractReplaceTokensLinesIterator
    {
        private CopyDirective CopyReplacingDirective { get; }

        public Replacing(ITokensLinesIterator sourceIterator, CopyDirective copyReplacingDirective, TypeCobolOptions compilerOptions) 
            : base(sourceIterator, (IReadOnlyList<ReplaceOperation>)copyReplacingDirective.ReplaceOperations, compilerOptions)
        {
            this.CopyReplacingDirective = copyReplacingDirective;
        }

        protected override CheckTokenStatus CheckTokenBeforeReplace(Func<Token> getNextToken, IReadOnlyList<ReplaceOperation> currentReplaceOperations)
        {
            var nextToken = getNextToken();

            //Remove level 01
#if EUROINFO_RULES
            if (CompilerOptions.UseEuroInformationLegacyReplacingSyntax)
            {
                // Support for legacy replacing syntax semantics : 
                // Remove the first 01 level data item found in the COPY text
                // before copying it into the main program
                // But do not remove data from debug lines
                if (CopyReplacingDirective != null && CopyReplacingDirective.RemoveFirst01Level && nextToken.TokensLine.Type != CobolTextLineType.Debug)
                {
                    //A Data description entry starts with an integer literal
                    if (nextToken.TokenType == TokenType.LevelNumber)
                    {
                        if (nextToken.Text == "01" && nextToken.Column < 10)
                        {
                            var firstLevelFound = true;
                            // Skip all tokens after 01 until the next period separator 
                            while (firstLevelFound && nextToken.TokenType != TokenType.EndOfFile)
                            {
                                nextToken = getNextToken();

                                if (nextToken.TokenType == TokenType.PeriodSeparator)
                                {
                                    nextToken = getNextToken();
                                    if (nextToken.Text != "01" || nextToken.Column > 9)
                                        firstLevelFound = false;

                                }
                            }
                        }
                    }
                }
            }
#endif

#if EUROINFO_RULES
            if (CompilerOptions.UseEuroInformationLegacyReplacingSyntax)
            {
                // Support for legacy replacing syntax semantics : 
                // Insert Suffix before the first '-' in all user defined words found in the COPY text 
                // before copying it into the main program
                if (CopyReplacingDirective != null && CopyReplacingDirective.InsertSuffixChar && nextToken.TokenType == TokenType.UserDefinedWord)
                {
                    string originalText = nextToken.Text;
                    if (originalText.IndexOf(CopyReplacingDirective.PreSuffix, StringComparison.Ordinal) > -1)
                    {
                        string replacement = CopyReplacingDirective.PreSuffix.Insert(3, CopyReplacingDirective.Suffix);
                        string replacedText = originalText.Replace(CopyReplacingDirective.PreSuffix, replacement);
                        int additionalSpaceRequired = replacedText.Length - originalText.Length;
                        if (CheckTokensLineOverflow(nextToken, additionalSpaceRequired))
                        {
                            TokensLine virtualTokensLine = TokensLine.CreateVirtualLineForInsertedToken(0, replacedText, nextToken.TokensLine.ColumnsLayout);
                            Token replacementToken = new Token(TokenType.UserDefinedWord, 0, replacedText.Length - 1, virtualTokensLine);

                            nextToken = new ReplacedToken(replacementToken, nextToken);
                        }
                    }
                }
            }
#endif

            //Replacing directive never change
            
            return new CheckTokenStatus()
            {
                ApplyReplace = currentReplaceOperations != null,
                NextToken = nextToken,
                UpdatedReplaceOperations = currentReplaceOperations
            };
        }

#if EUROINFO_RULES
        private bool CheckTokensLineOverflow(Token token, int additionalSpaceRequired)
        {
            var tokensLine = token.TokensLine;
            if (tokensLine.ColumnsLayout == ColumnsLayout.FreeTextFormat)
                // No check on free format
                return true;

            const int MAX_LINE_LENGTH = (int)CobolFormatAreas.End_B;
            var lastToken = tokensLine.SourceTokens.Last(); //This is safe as the line contains at least one token, the one which is being considered for replacement
            int endColumn;
            if (tokensLine.HasTokenContinuedOnNextLine && LastTokenIsLiteralAllowingSpace())
            {
                //The literal may be altered by suffixing, check how the line ends
                endColumn = lastToken.EndColumn;
                if (endColumn == MAX_LINE_LENGTH)
                {
                    //No trailing space, but we can't apply suffix because there is no room left
                    AddWarningOnToken();
                }
                else
                {
                    //Suffixing would cause an alteration to the VALUE, add error
                    AddErrorOnToken();
                }

                return false;
            }

            //We cannot use lastToken.EndColumn here because some tokens are 'grabbing' the space following them
            //Example PeriodSeparator having text '. '
            endColumn = lastToken.StartIndex + lastToken.Text.TrimEnd().Length;
            if (endColumn + additionalSpaceRequired > MAX_LINE_LENGTH)
            {
                AddWarningOnToken();
                return false;
            }

            return true;

            bool LastTokenIsLiteralAllowingSpace()
            {
                switch (lastToken.TokenType)
                {
                    case TokenType.AlphanumericLiteral:
                    case TokenType.NullTerminatedAlphanumericLiteral:
                    case TokenType.DBCSLiteral:
                    case TokenType.NationalLiteral:
                        return true;
                    default:
                        return false;
                }
            }

            void AddWarningOnToken()
            {
                const string WARNING_MESSAGE_TEMPLATE = "'{0}' could not be suffixed because line ends at column {1}.";
                string message = string.Format(WARNING_MESSAGE_TEMPLATE, token.Text, endColumn);
                AddDiagnosticOnToken(MessageCode.Warning, message);
            }

            void AddErrorOnToken()
            {
                const string ERROR_MESSAGE_TEMPLATE = "Suffixing '{0}' will alter VALUE clause, cannot use CPY suffixing here.";
                string message = string.Format(ERROR_MESSAGE_TEMPLATE, token.Text);
                AddDiagnosticOnToken(MessageCode.SyntaxErrorInParser, message);
            }

            void AddDiagnosticOnToken(MessageCode messageCode, string message)
            {
                //token is part of a COPY however it has not been wrapped into ImportedToken yet. We have to create Position manually with the proper including directive.
                var position = new Diagnostic.Position(token.Line, token.Column, token.Line, token.EndColumn, CopyReplacingDirective);
                var diagnostic = new Diagnostic(messageCode, position, message);
                CopyReplacingDirective.AddProcessingDiagnostic(diagnostic);
            }
        }
#endif
    }
}
