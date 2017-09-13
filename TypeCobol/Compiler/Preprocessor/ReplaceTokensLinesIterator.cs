using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

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
        public CopyDirective CopyReplacingDirective { get; private set; }

        // Iterator position
        private struct ReplaceTokensLinesIteratorPosition
        {
            // Underlying tokens iterator position
            public object SourceIteratorPosition;

#if EUROINFO_LEGACY_REPLACING_SYNTAX

            // Support for legacy replacing syntax semantics : 
            // Remove the first 01 level data item found in the COPY text
            // before copying it into the main program
            public bool SawFirstIntegerLiteral;
#endif

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

        /// <summary>
        /// Implement REPLACE directives on top of a CopyTokensLinesIterator
        /// </summary>
        public ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator)
        {
            this.sourceIterator = sourceIterator;
        }

        /// <summary>
        /// Implement COPY REPLACING on top of an underlying tokens line iterator
        /// </summary>
        public ReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, CopyDirective copyReplacingDirective)
        {
            this.sourceIterator = sourceIterator;
            this.CopyReplacingDirective = copyReplacingDirective;

            if(copyReplacingDirective.ReplaceOperations.Count > 0)
            {
                if(copyReplacingDirective.ReplaceOperations.Count == 1)
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

#if EUROINFO_LEGACY_REPLACING_SYNTAX

                // Support for legacy replacing syntax semantics : 
                // Remove the first 01 level data item found in the COPY text
                // before copying it into the main program
                if(CopyReplacingDirective != null && CopyReplacingDirective.RemoveFirst01Level && CopyReplacingDirective.ReplaceOperations.Count == 0)
                {
                    //A Data description entry starts with an integer literal
                    if(nextToken.TokenType == TokenType.LevelNumber)
                    {
                        if (nextToken.Text == "01" && nextToken.Column <= 10) {
                            var firstLevelFound = true;
                            // Register that we saw the first "01" integer literal in the underlying file
                            currentPosition.SawFirstIntegerLiteral = true;
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
#endif

                // If the next token is a REPLACE directive, update the current replace directive in effect
                while (nextToken.TokenType == TokenType.ReplaceDirective)
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
                            foreach (ReplaceOperation replaceOperation in currentPosition.ReplaceOperations)
                            {
                                status = TryAndReplace(nextToken, replaceOperation);
                                if (status.replacedToken != null) return status.replacedToken;
                                if (status.tryAgain)
                                {
                                    nextToken = sourceIterator.NextToken();
                                    break;
                                }
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

#if EUROINFO_LEGACY_REPLACING_SYNTAX

            // Support for legacy replacing syntax semantics : 
            // Insert Suffix before the first '-' in all user defined words found in the COPY text 
            // before copying it into the main program
            if (CopyReplacingDirective != null && CopyReplacingDirective.InsertSuffixChar && nextToken.TokenType == TokenType.UserDefinedWord)
            {
                string originalText = nextToken.Text;
                if (originalText.Contains(CopyReplacingDirective.PreSuffix))
                {
                    string replacedText = originalText.Replace(CopyReplacingDirective.PreSuffix , CopyReplacingDirective.PreSuffix.Insert(3, CopyReplacingDirective.Suffix));
                    TokensLine virtualTokensLine = TokensLine.CreateVirtualLineForInsertedToken(0, replacedText);
                    Token replacementToken = new Token(TokenType.UserDefinedWord, 0, replacedText.Length - 1,
                        virtualTokensLine);

                    status.replacedToken = new ReplacedToken(replacementToken, nextToken);
                    currentPosition.CurrentToken = status.replacedToken;
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
                        ReplacedToken replacedToken = new ReplacedToken(singleTokenReplaceOperation.ReplacementToken, originalToken);
                        return replacedToken;
                    }
                    else
                    {
                        return null;
                    }

                // One pure partial word => one replacement token
                case ReplaceOperationType.PartialWord:
                    PartialWordReplaceOperation partialWordReplaceOperation = (PartialWordReplaceOperation)replaceOperation;
                    string originalTokenText = originalToken.Text;
                    string partToReplace = partialWordReplaceOperation.ComparisonToken.Text;
                    //#258 - PartialReplacementToken can be null. In this case, we consider that it's an empty replacement
                    var replacementPart = partialWordReplaceOperation.PartialReplacementToken != null ? partialWordReplaceOperation.PartialReplacementToken.Text : "";
                    // The index below is always >= 0 because CompareForReplace() above was true
                    int indexOfPartToReplace = originalTokenText.IndexOf(partToReplace, StringComparison.OrdinalIgnoreCase);
                    string replacedTokenText =
                        (indexOfPartToReplace > 0 ? originalTokenText.Substring(0, indexOfPartToReplace) : String.Empty) +
                        replacementPart +
                        ((indexOfPartToReplace + partToReplace.Length) < (originalTokenText.Length) ? originalTokenText.Substring(indexOfPartToReplace + partToReplace.Length) : String.Empty);
                    // TO DO : find a way to transfer the scanner context the of original token to the call below
                    Diagnostic error = null;
                    Token generatedToken = Scanner.Scanner.ScanIsolatedTokenInDefaultContext(replacedTokenText, out error);
                    // TO DO : find a way to report the error above ...

                    if (originalToken.PreviousTokenType != null) //In case orignal token was previously an other type of token reset it back to it's orignal type. 
                        generatedToken.TokenType = originalToken.PreviousTokenType.Value;

                    ReplacedPartialCobolWord replacedPartialCobolWord = new ReplacedPartialCobolWord(generatedToken, partialWordReplaceOperation.PartialReplacementToken, originalToken);
                    return replacedPartialCobolWord;

                // One comparison token => more than one replacement tokens
                case ReplaceOperationType.SingleToMultipleTokens:
                    SingleToMultipleTokensReplaceOperation singleToMultipleTokensReplaceOperation = (SingleToMultipleTokensReplaceOperation)replaceOperation;
                    currentPosition.ReplacementTokensBeingReturned = new Token[singleToMultipleTokensReplaceOperation.ReplacementTokens.Length];
                    int i = 0;
                    foreach (Token replacementToken in singleToMultipleTokensReplaceOperation.ReplacementTokens)
                    {
                        currentPosition.ReplacementTokensBeingReturned[i] = new ReplacedToken(replacementToken, originalToken);
                        i++;
                    }
                    currentPosition.ReplacementTokenIndexLastReturned = 0;
                    return currentPosition.ReplacementTokensBeingReturned[currentPosition.ReplacementTokenIndexLastReturned];

                // One first + several following comparison tokens => zero to many replacement tokens                
                //case ReplaceOperationType.MultipleTokens:
                default:
                    MultipleTokensReplaceOperation multipleTokensReplaceOperation = (MultipleTokensReplaceOperation)replaceOperation;
                    if (multipleTokensReplaceOperation.ReplacementTokens != null)
                    {
                        if (multipleTokensReplaceOperation.ReplacementTokens.Length == 1)
                        {
                            ReplacedTokenGroup replacedTokenGroup = new ReplacedTokenGroup(multipleTokensReplaceOperation.ReplacementTokens[0], originalMatchingTokens);
                            return replacedTokenGroup;
                        }
                        else
                        {
                            currentPosition.ReplacementTokensBeingReturned = new Token[multipleTokensReplaceOperation.ReplacementTokens.Length];
                            i = 0;
                            foreach (Token replacementToken in multipleTokensReplaceOperation.ReplacementTokens)
                            {
                                currentPosition.ReplacementTokensBeingReturned[i] = new ReplacedTokenGroup(replacementToken, originalMatchingTokens);
                                i++;
                            }
                            currentPosition.ReplacementTokenIndexLastReturned = 0;
                            return currentPosition.ReplacementTokensBeingReturned[currentPosition.ReplacementTokenIndexLastReturned];
                        }
                    }
                    else
                    {
                        return null;
                    }
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
