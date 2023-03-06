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
    public class ReplaceTokensLinesIterator : ITokensLinesIterator
    {
        // Underlying tokens iterator returning :
        // - significant source tokens
        // - REPLACE CompilerDirectiveTokens
        private ITokensLinesIterator sourceIterator;

        // Current COPY REPLACING directive in effect for a file import
        // (optional : null if the iterator was NOT created in the context of an imported document)
        public CopyDirective CopyReplacingDirective { get; }

        // Iterator position
        private struct ReplaceTokensLinesIteratorPosition
        {
            // Underlying tokens iterator position
            public object SourceIteratorPosition;

            // Current REPLACE directive in effect in the file
            // (optional : null if the iterator WAS created in the context of an imported document)
            public ReplaceDirective ReplaceDirective;

            // Optimization : current replace operation in the most common case when there is only one 
            public ReplaceOperation ReplaceOperation;
            // More general case where there are several replace operations in effect
            public IList<ReplaceOperation> ReplaceOperations;

            // Used when a replace operation returns several replacement tokens
            public Token[] ReplacementTokensBeingReturned;
            // Index of the last token returned in a replacement tokens group
            public int ReplacementTokenIndexLastReturned;

            // Last Token that was returned by the NextToken method
            public Token CurrentToken;
        }

        // Current iterator position
        private ReplaceTokensLinesIteratorPosition currentPosition;

        // Previous snapshot position
        private object snaphsotPosition;

        // Options entered in CLI
        private TypeCobolOptions CompilerOptions;

        /// <summary>
        /// Implement REPLACE directives on top of a CopyTokensLinesIterator
        /// </summary>
        public ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, TypeCobolOptions compilerOptions)
        {
            this.sourceIterator = sourceIterator;
            CompilerOptions = compilerOptions;
        }

        /// <summary>
        /// Implement COPY REPLACING on top of an underlying tokens line iterator
        /// </summary>
        public ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, CopyDirective copyReplacingDirective, TypeCobolOptions compilerOptions)
        {
            this.sourceIterator = sourceIterator;
            this.CopyReplacingDirective = copyReplacingDirective;
            CompilerOptions = compilerOptions;

            if (copyReplacingDirective.ReplaceOperations.Count > 0)
            {
                if (copyReplacingDirective.ReplaceOperations.Count == 1)
                {
                    currentPosition.ReplaceOperation = copyReplacingDirective.ReplaceOperations[0];
                }
                else
                {
                    currentPosition.ReplaceOperations = copyReplacingDirective.ReplaceOperations;
                }
            }
        }

        /// <summary>
        /// Name or path of the text document where the current Token was found.
        /// If the current token was found in COPY CPY1 imported by PROGRAM PRGM1 :
        /// DocumentPath = "PGM1/CPY1"
        /// </summary>
        public string DocumentPath
        {
            get { return sourceIterator.DocumentPath; }
        }

        /// <summary>
        /// Current line index in the main text document 
        /// (PROGRAM or CLASS source file).
        /// If a COPY directive was found on line index 12 in the main program file,
        /// when the iterator returns the first token of the secondary copy file :
        /// DocumentPath = "PGM1/CPY1"
        /// LineIndex = 0 (in file CPY1)
        /// LineIndexInMainDocument = 12 (in file PGM1)
        /// </summary>
        public int LineIndexInMainDocument
        {
            get { return sourceIterator.LineIndexInMainDocument; }
        }

        /// <summary>
        /// Current column index 
        /// (in the text document identified by DocumentPath)
        /// </summary>
        public int ColumnIndex
        {
            get { return sourceIterator.ColumnIndex; }
        }

        /// <summary>
        /// Current line index 
        /// (in the text document identified by DocumentPath)
        /// </summary>
        public int LineIndex
        {
            get { return sourceIterator.LineIndex; }
        }

        /// <summary>
        /// Current tokens line
        /// </summary>
        public ITokensLine CurrentLine
        {
            get { return sourceIterator.CurrentLine; }
        }

        /// <summary>
        /// Returns the last token of the last line before EOF
        /// </summary>
        public ITokensLine LastLine
        {
            get { return sourceIterator.LastLine; }
        }

        /// <summary>
        /// Get next token after REPLACE processing or EndOfFile
        /// </summary>
        public Token NextToken()
        {
            // If the iterator is in the process of returning several replacement tokens
            // => return the next replacement tokens until we reach then end of the group
            if (currentPosition.ReplacementTokensBeingReturned != null)
            {
                currentPosition.ReplacementTokenIndexLastReturned++;
                Token nextToken = currentPosition.ReplacementTokensBeingReturned[currentPosition.ReplacementTokenIndexLastReturned];
                if (currentPosition.ReplacementTokenIndexLastReturned == currentPosition.ReplacementTokensBeingReturned.Length - 1)
                {
                    currentPosition.ReplacementTokensBeingReturned = null;
                    currentPosition.ReplacementTokenIndexLastReturned = 0;
                }

                currentPosition.CurrentToken = nextToken;
                return nextToken;
            }
            // Analyze the next token returned by the underlying iterator
            else
            {
                Token nextToken = sourceIterator.NextToken();

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
                                    nextToken = sourceIterator.NextToken();

                                    if (nextToken.TokenType == TokenType.PeriodSeparator)
                                    {
                                        nextToken = sourceIterator.NextToken();
                                        if (nextToken.Text != "01" || nextToken.Column > 9)
                                            firstLevelFound = false;

                                    }
                                }
                            }
                        }
                    }
                }                
#endif

                // If the next token is a REPLACE directive, update the current replace directive in effect
                while (nextToken.TokenType == TokenType.REPLACE_DIRECTIVE)
                {
                    // Reset previous replace operations
                    currentPosition.ReplaceOperation = null;
                    currentPosition.ReplaceOperations = null;

                    // A REPLACE OFF directive simply cancels the previous directive 
                    if (((CompilerDirectiveToken)nextToken).CompilerDirective.Type == CompilerDirectiveType.REPLACE_OFF)
                    {
                        currentPosition.ReplaceDirective = null;
                    }
                    // A new REPLACE directive replaces the previous directive in effect
                    else
                    {
                        currentPosition.ReplaceDirective = (ReplaceDirective)((CompilerDirectiveToken)nextToken).CompilerDirective; ;

                        // In case of syntax error, if the replace directive does not define any replace operation, do nothing
                        if (currentPosition.ReplaceDirective.ReplaceOperations.Count == 0)
                        {
                            currentPosition.ReplaceDirective = null;
                        }
                        // Optimization for the most common case, when the replace directive defines only one replace operation   
                        else if (currentPosition.ReplaceDirective.ReplaceOperations.Count == 1)
                        {
                            currentPosition.ReplaceOperation = currentPosition.ReplaceDirective.ReplaceOperations[0];
                        }
                        // More general case when there are several replace operations
                        else
                        {
                            currentPosition.ReplaceOperations = currentPosition.ReplaceDirective.ReplaceOperations;
                        }
                    }
                    nextToken = sourceIterator.NextToken();
                }

                // Apply the current REPLACE operations in effect
                ReplaceStatus status;
                do
                {
                    status = TryAndReplace(nextToken, currentPosition.ReplaceOperation);
                    if (status.replacedToken != null) return status.replacedToken;
                    if (status.tryAgain)
                    {
                        nextToken = sourceIterator.NextToken();
                    }
                    else
                    {
                        if (currentPosition.ReplaceOperations != null)
                        {
                            Token lastReplacedToken = null;
                            bool matchingMode = false;
                            foreach (ReplaceOperation replaceOperation in currentPosition.ReplaceOperations)
                            {
                                status = TryAndReplace(lastReplacedToken ?? nextToken, replaceOperation);
                                lastReplacedToken = status.replacedToken ?? lastReplacedToken;
                                matchingMode = lastReplacedToken != null;
                                if (status.tryAgain)
                                {
                                    if (matchingMode)
                                    {
                                        sourceIterator.ReturnToLastPositionSnapshot();
                                    }
                                    else
                                    {
                                        nextToken = sourceIterator.NextToken();
                                        break;
                                    }
                                }
                            }
                            if (matchingMode)
                            {
                                return lastReplacedToken;
                            }
                        }
                    }
                } while (status.tryAgain);

                // If no replacement took place, simply return the next token of the underlying iterator
                currentPosition.CurrentToken = nextToken;
                return nextToken;
            }
        }

        private class ReplaceStatus
        {
            public bool tryAgain = false;
            public Token replacedToken = null;
        }

        private ReplaceStatus TryAndReplace(Token nextToken, ReplaceOperation replaceOperation)
        {
            ReplaceStatus status = new ReplaceStatus();
            IList<Token> originalMatchingTokens;

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
                            Token replacementToken = new Token(TokenType.UserDefinedWord, 0, replacedText.Length - 1,
                                virtualTokensLine);

                            status.replacedToken = new ReplacedToken(replacementToken, nextToken);
                            currentPosition.CurrentToken = status.replacedToken;
                        }
                    }
                }
            }
#endif

            if (replaceOperation != null && TryMatchReplaceOperation(nextToken, replaceOperation, out originalMatchingTokens))
            {
                status.replacedToken = CreateReplacedTokens(nextToken, replaceOperation, originalMatchingTokens);
                if (status.replacedToken != null)
                {
                    // REPLACE pattern matched => return the first replaced token
                    currentPosition.CurrentToken = status.replacedToken;
                }
                else
                {
                    // If the replacement token set is empty (REPLACE == ... = BY == ==), get next token and try again
                    status.tryAgain = true;
                }
            }
            return status;
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

        /// <summary>
        /// Get null (before the first call to NextToken()), current token, or EndOfFile
        /// </summary>
        public Token CurrentToken
        {
            get { return currentPosition.CurrentToken; }
        }

        /// <summary>
        /// Check if the current tokens match with the comparison tokens of the current replace operation
        /// </summary>
        private bool TryMatchReplaceOperation(Token originalToken, ReplaceOperation replaceOperation, out IList<Token> originalMatchingTokens)
        {
            // Check if the first token matches the replace pattern
            if (originalToken.CompareForReplace(replaceOperation.ComparisonToken))
            {
                // Multiple tokens pattern => check if the following tokens returned by the underlying iterator all match the pattern
                if (replaceOperation.Type == ReplaceOperationType.MultipleTokens)
                {
                    MultipleTokensReplaceOperation multipleTokensReplaceOperation = (MultipleTokensReplaceOperation)replaceOperation;
                    originalMatchingTokens = new List<Token>();
                    originalMatchingTokens.Add(originalToken);
                    sourceIterator.SaveCurrentPositionSnapshot();
                    bool comparisonInterrupted = false;
                    foreach (Token comparisonToken in multipleTokensReplaceOperation.FollowingComparisonTokens)
                    {
                        Token nextCandidateToken = sourceIterator.NextToken();
                        if (!comparisonToken.CompareForReplace(nextCandidateToken))
                        {
                            comparisonInterrupted = true;
                            break;
                        }
                        else
                        {
                            originalMatchingTokens.Add(nextCandidateToken);
                        }
                    }
                    // The following tokens did not match
                    if (comparisonInterrupted)
                    {
                        // Restore the uderlying iterator position
                        sourceIterator.ReturnToLastPositionSnapshot();
                        // Match failed
                        originalMatchingTokens = null;
                        return false;
                    }
                    // Multiple tokens match OK
                    else
                    {
                        return true;
                    }
                }
                // Single token comparison => match OK
                else
                {
                    originalMatchingTokens = null;
                    return true;
                }
            }
            // First token does not match
            else
            {
                originalMatchingTokens = null;
                return false;
            }
        }

        /// <summary>
        /// Create replaced tokens for all matched tokens
        /// (a ReplacedToken references both the original token and the replacement token)
        /// </summary>
        private Token CreateReplacedTokens(Token originalToken, ReplaceOperation replaceOperation, IList<Token> originalMatchingTokens)
        {
            switch (replaceOperation.Type)
            {
                // One comparison token => zero or one replacement token 
                case ReplaceOperationType.SingleToken:
                    SingleTokenReplaceOperation singleTokenReplaceOperation = (SingleTokenReplaceOperation)replaceOperation;
                    if (singleTokenReplaceOperation.ReplacementToken != null)
                    {
                        var replacedTokens = RescanReplacedTokenTypes<ReplacedToken>(t => new ReplacedToken(t, originalToken), originalToken, singleTokenReplaceOperation.ReplacementToken);
                        return replacedTokens[0];
                    }
                    else
                    {
                        return null;
                    }
                // One pure partial word => one replacement token
                case ReplaceOperationType.PartialWord:
                    PartialWordReplaceOperation partialWordReplaceOperation = (PartialWordReplaceOperation)replaceOperation;
                    string normalizedTokenText = originalToken.NormalizedText;
                    string normalizedPartToReplace = partialWordReplaceOperation.ComparisonToken.NormalizedText;
                    //#258 - PartialReplacementToken can be null. In this case, we consider that it's an empty replacement
                    var replacementPart = partialWordReplaceOperation.PartialReplacementToken != null ? partialWordReplaceOperation.PartialReplacementToken.Text : "";
                    string replacedTokenText = Regex.Replace(normalizedTokenText, normalizedPartToReplace, replacementPart, RegexOptions.IgnoreCase);
                    var generatedToken = GenerateReplacementToken(originalToken, replacedTokenText, CompilerOptions);
                    return new ReplacedPartialCobolWord(generatedToken, partialWordReplaceOperation.PartialReplacementToken, originalToken);

                // One comparison token => more than one replacement tokens
                case ReplaceOperationType.SingleToMultipleTokens:
                    {
                        SingleToMultipleTokensReplaceOperation singleToMultipleTokensReplaceOperation = (SingleToMultipleTokensReplaceOperation)replaceOperation;
                        currentPosition.ReplacementTokensBeingReturned = new Token[singleToMultipleTokensReplaceOperation.ReplacementTokens.Length];
                        currentPosition.ReplacementTokensBeingReturned = 
                            RescanReplacedTokenTypes<ReplacedToken>(t => new ReplacedToken(t, originalToken), originalToken, singleToMultipleTokensReplaceOperation.ReplacementTokens);
                        currentPosition.ReplacementTokenIndexLastReturned = 0;
                        return currentPosition.ReplacementTokensBeingReturned[currentPosition.ReplacementTokenIndexLastReturned];
                    }

                // One first + several following comparison tokens => zero to many replacement tokens                
                //case ReplaceOperationType.MultipleTokens:
                default:
                    {
                        MultipleTokensReplaceOperation multipleTokensReplaceOperation = (MultipleTokensReplaceOperation)replaceOperation;
                        if (multipleTokensReplaceOperation.ReplacementTokens != null)
                        {
                            System.Diagnostics.Debug.Assert(multipleTokensReplaceOperation.ReplacementTokens.Length > 0);
                            var replacedTokenGroups = RescanReplacedTokenTypes<ReplacedTokenGroup>(t => new ReplacedTokenGroup(t, originalMatchingTokens),
                            originalMatchingTokens.Count > 0 ? originalMatchingTokens[0] : null, multipleTokensReplaceOperation.ReplacementTokens);
                            if (multipleTokensReplaceOperation.ReplacementTokens.Length > 1)
                            {
                                currentPosition.ReplacementTokenIndexLastReturned = 0;
                                currentPosition.ReplacementTokensBeingReturned = replacedTokenGroups;
                            }
                            return replacedTokenGroups[0];
                        }
                        else
                        {
                            return null;
                        }
                    }
            }
        }

        internal static Token GenerateReplacementToken(Token originalToken, string replacedTokenText, TypeCobolOptions scanOptions)
        {
            // Transfer the scanner context the of original token to the call below
            MultilineScanState scanState = originalToken.ScanStateSnapshot;
            System.Diagnostics.Debug.Assert(scanState != null);

            Token generatedToken = Scanner.Scanner.ScanIsolatedToken(replacedTokenText, scanState, scanOptions, originalToken.TokensLine.ColumnsLayout, out _);
            // TODO : find a way to report the error above ...

            if (originalToken.PreviousTokenType != null)
                //In case original token was previously an other type of token reset it back to it's original type. 
                generatedToken.TokenType = originalToken.PreviousTokenType.Value;

            return generatedToken;
        }

        /// <summary>
        /// Rescan the TokenType of a set of replaced Tokens.
        /// </summary>
        /// <param name="creator">A function that create a Replaced Token from a replacement</param>
        /// <param name="firstOriginalToken">The first original token to be replaced</param>
        /// <param name="replacementTokens">The array of replacement tokens.</param>
        /// <returns>An array of new calculated replaced tokens</returns>
        private T[] RescanReplacedTokenTypes<T>(Func<Token, T> creator, Token firstOriginalToken, params Token[] replacementTokens)
            where T : Token
        {
            MultilineScanState scanState = firstOriginalToken?.ScanStateSnapshot;
            if (scanState != null && replacementTokens.Any(MultilineScanState.IsScanStateDependent))
            {
                int i = 0;
                int[] columns = new int[replacementTokens.Length + 1];                
                StringBuilder sb = new StringBuilder();
                foreach (var t in replacementTokens)
                {
                    columns[i++] = sb.Length;
                    sb.Append(t.Text);
                }
                columns[i] = sb.Length;
                string tokenText = sb.ToString();
                int startTokIdx = 0;
                int endTokIdx = 0;                
                List<T> newReplacedTokens = new List<T>(replacementTokens.Length);
                TokensLine tempTokensLine = TokensLine.CreateVirtualLineForInsertedToken(0, tokenText, firstOriginalToken.TokensLine.ColumnsLayout);
                tempTokensLine.InitializeScanState(scanState);
                var tempScanner = new TypeCobol.Compiler.Scanner.Scanner(tokenText, 0, tokenText.Length - 1, tempTokensLine, CompilerOptions, true);
                Token rescannedToken = null;
                List<Token> tokens = new List<Token>((replacementTokens.Length / 2) + 1);
                while ((rescannedToken = tempScanner.GetNextToken()) != null)
                {
                    tempTokensLine.AddToken(rescannedToken);
                    if (rescannedToken.TokenFamily != TokenFamily.Whitespace &&
                        rescannedToken.TokenFamily != TokenFamily.Comments)
                    {
                        startTokIdx = CreateReplacedToken();
                    }
                    else 
                    {
                        startTokIdx++;
                    }                    
                }
                return newReplacedTokens.ToArray();
                // Create the replaced token from the rescanned replacement token.
                // If the rescanned token used more then one replacement token a
                // TokensGroup instance is used.
                int  CreateReplacedToken()
                {
                    tokens.Clear();
                    for (endTokIdx = startTokIdx; endTokIdx < replacementTokens.Length && columns[endTokIdx] < rescannedToken.EndColumn; endTokIdx++)
                    {
                        if (replacementTokens[endTokIdx].TokenFamily != TokenFamily.Whitespace &&
                            replacementTokens[endTokIdx].TokenFamily != TokenFamily.Comments)
                        {
                            tokens.Add(replacementTokens[endTokIdx]);
                        }
                    }
                    System.Diagnostics.Debug.Assert(tokens.Count >= 1);
                    bool isGroup = tokens.Count > 1;
                    T replacedToken = isGroup
                        ? creator(new TokensGroup(rescannedToken.TokenType, new List<Token>(tokens)))
                        : creator(tokens[0]);
                    replacedToken.TokenType = rescannedToken.TokenType;
                    newReplacedTokens.Add(replacedToken);
                    return endTokIdx;
                }
            }
            else 
            {
                T[] replacedTokens = new T[replacementTokens.Length];
                for(int i = 0;i <  replacementTokens.Length; i++)
                {
                    replacedTokens[i] = creator(replacementTokens[i]);
                }
                return replacedTokens;
            }
        }

        /// <summary>
        /// Get an opaque object representing the current position of the iterator.
        /// Use it with the SeekToPosition method to restore this position later.
        /// </summary>
        public object GetCurrentPosition()
        {
            currentPosition.SourceIteratorPosition = sourceIterator.GetCurrentPosition();
            return currentPosition;
        }

        /// <summary>
        /// Sets the current iterator position to a previous position returned by GetCurrentPosition.
        /// After a call to this method, GetNextToken returns the token FOLLOWING the current position.
        /// </summary>
        public void SeekToPosition(object iteratorPosition)
        {
            // Restore iterators positions
            currentPosition = (ReplaceTokensLinesIteratorPosition)iteratorPosition;
            sourceIterator.SeekToPosition(currentPosition.SourceIteratorPosition);
        }

        public void SeekToLineInMainDocument(int line)
        {
            // TODO actual REPLACE directive/operations in effect are lost here, this is equivalent to a reset of this ReplaceIterator
            currentPosition = new ReplaceTokensLinesIteratorPosition();
            sourceIterator.SeekToLineInMainDocument(line);
        }

        /// <summary>
        /// Saves the current position of the iterator, to be able to restore it later
        /// </summary>
        public void SaveCurrentPositionSnapshot()
        {
            snaphsotPosition = GetCurrentPosition();
        }

        /// <summary>
        /// Restores the last position snapshot
        /// </summary>
        public void ReturnToLastPositionSnapshot()
        {
            SeekToPosition(snaphsotPosition);
        }
    }
}
