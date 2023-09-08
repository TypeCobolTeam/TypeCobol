using System;
using System.Collections.Generic;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Handle replacing clause and specific replacement for EUROINFO_RULES.
    /// Replacing clause is applied before Replace clause.
    /// </summary>
    public class ReplacingTokensLinesIterator : AbstractReplaceTokensLinesIterator
    {
        private readonly CopyDirective _copyReplacingDirective;

        public ReplacingTokensLinesIterator(ITokensLinesIterator sourceIterator, CopyDirective copyReplacingDirective, TypeCobolOptions compilerOptions) 
            : base(sourceIterator, (IReadOnlyList<ReplaceOperation>)copyReplacingDirective.ReplaceOperations, compilerOptions)
        {
            _copyReplacingDirective = copyReplacingDirective;
        }

        protected override CheckTokenStatus CheckNextTokenBeforeReplace([CanBeNull] IReadOnlyList<ReplaceOperation> currentReplaceOperations)
        {
            var nextToken = SourceIteratorNextToken();

            // This parser currently does not support REPLACE that are altered by the REPLACING clause
            if (nextToken.TokenType == TokenType.REPLACE_DIRECTIVE)
            {
                // Get REPLACE directive
                CompilerDirectiveToken compilerDirectiveToken = nextToken is ImportedToken importedToken
                    ? (CompilerDirectiveToken)importedToken.OriginalToken
                    : (CompilerDirectiveToken)nextToken;
                var replaceDirective = (ReplaceDirective)compilerDirectiveToken.CompilerDirective;

                // Check REPLACE directive nature
                if (replaceDirective.Type == CompilerDirectiveType.REPLACE)
                {
                    //TODO ReplaceAndReplacing better code to find the real ReplaceDirective on the line. Property ReplaceDirective is the LAST on the line
                    foreach (var replaceOperation in replaceDirective.ReplaceOperations)
                    {
                        CheckReplace(replaceOperation, replaceOperation.GetComparisonTokens());
                        CheckReplace(replaceOperation, replaceOperation.GetReplacementTokens());
                    }

                    //Use a ReplaceTokensLinesIterator to detect if the replacing clause can alter a Replace
                    void CheckReplace(ReplaceOperation replaceOperation, IList<Token> tokens)
                    {
                        var tokensIterator = new TokensIterator(DocumentPath, tokens);
                        var replaceIterator = new ReplaceTokensLinesIterator(tokensIterator, (IReadOnlyList<ReplaceOperation>)_copyReplacingDirective.ReplaceOperations, CompilerOptions);

                        bool replaceMatch = false;
                        Token currentToken = null;
                        while (currentToken?.TokenType != TokenType.EndOfFile && !replaceMatch)
                        {
                            currentToken = replaceIterator.NextToken();
                            replaceMatch = currentToken is ReplacedToken;
                        }

                        if (replaceMatch)
                        {
                            _copyReplacingDirective.AddProcessingDiagnostic(new ParserDiagnostic("Copy directive " + _copyReplacingDirective + " will alter REPLACE " + replaceOperation + " inside a COPY. This is not supported", nextToken, ""));
                        }
                    }
                }
                // else it's a REPLACE OFF, no need to create a diagnostic
            }

#if EUROINFO_RULES
            // Support for legacy replacing syntax semantics : 
            // Remove the first 01 level data item found in the COPY text
            // before copying it into the main program
            // But do not remove data from debug lines
            if (_copyReplacingDirective.RemoveFirst01Level && nextToken.TokensLine.Type != CobolTextLineType.Debug)
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
                            nextToken = SourceIteratorNextToken();

                            if (nextToken.TokenType == TokenType.PeriodSeparator)
                            {
                                nextToken = SourceIteratorNextToken();
                                if (nextToken.Text != "01" || nextToken.Column > 9)
                                    firstLevelFound = false;
                            }
                        }
                    }
                }
            }

            // Support for legacy replacing syntax semantics : 
            // Insert Suffix before the first '-' in all user defined words found in the COPY text 
            // before copying it into the main program
            if (_copyReplacingDirective.InsertSuffixChar && nextToken.TokenType == TokenType.UserDefinedWord)
            {
                string originalText = nextToken.Text;
                if (originalText.IndexOf(_copyReplacingDirective.PreSuffix, StringComparison.Ordinal) > -1)
                {
                    string replacement = _copyReplacingDirective.PreSuffix.Insert(3, _copyReplacingDirective.Suffix);
                    string replacedText = originalText.Replace(_copyReplacingDirective.PreSuffix, replacement);
                    int additionalSpaceRequired = replacedText.Length - originalText.Length;
                    if (CheckTokensLineOverflow(nextToken, additionalSpaceRequired))
                    {
                        TokensLine virtualTokensLine = TokensLine.CreateVirtualLineForInsertedToken(nextToken.TokensLine.LineIndex, replacedText, nextToken.TokensLine.ColumnsLayout);
                        Token replacementToken = new Token(TokenType.UserDefinedWord, 0, replacedText.Length - 1, virtualTokensLine);

                        nextToken = new ReplacedToken(replacementToken, nextToken);
                    }
                }
            }
#endif

            //Replacing directive never changes
            return new CheckTokenStatus()
                   {
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
                var position = new Diagnostic.Position(token.Line, token.Column, token.Line, token.EndColumn, _copyReplacingDirective);
                var diagnostic = new Diagnostic(messageCode, position, message);
                _copyReplacingDirective.AddProcessingDiagnostic(diagnostic);
            }
        }
#endif
    }
}
