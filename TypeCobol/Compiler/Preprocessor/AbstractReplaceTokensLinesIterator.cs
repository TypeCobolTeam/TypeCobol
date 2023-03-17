using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Base class for both ReplaceTokensLinesIterator and ReplacingTokensLinesIterator.
    /// Handles replace operations coming from either REPLACE directive REPLACING clause.
    /// Uses a ScanStateTracker to implement replace operations properly.
    /// </summary>
    public abstract class AbstractReplaceTokensLinesIterator : ITokensLinesIterator
    {
        // Underlying tokens iterator returning :
        // - significant source tokens
        // - REPLACE CompilerDirectiveTokens
        private readonly ITokensLinesIterator _sourceIterator;

        // Iterator position
        private struct Position
        {
            // Underlying tokens iterator position
            public object SourceIteratorPosition;

            // Replace operations in effect
            public IReadOnlyList<ReplaceOperation> ReplaceOperations;

            // Used when a replace operation returns several replacement tokens
            public Token[] ReplacementTokensBeingReturned;
            // Index of the last token returned in a replacement tokens group
            public int ReplacementTokenIndexLastReturned;

            // Last Token that was returned by the NextToken method
            public Token CurrentToken;
        }

        /// <summary>
        /// Initialize this iterator with NO ReplaceOperations
        /// </summary>
        protected AbstractReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, TypeCobolOptions compilerOptions)
        {
            this._sourceIterator = sourceIterator;
            this.CompilerOptions = compilerOptions;
            this._scanStateTracker = new ScanStateTracker(this);
        }

        /// <summary>
        /// Initialize this iterator with ReplaceOperations
        /// </summary>
        protected AbstractReplaceTokensLinesIterator(ITokensLinesIterator sourceIterator, IReadOnlyList<ReplaceOperation> replaceOperations, TypeCobolOptions compilerOptions)
            : this(sourceIterator, compilerOptions)
        {
            this._currentPosition.ReplaceOperations = replaceOperations;
        }

        /// <summary>
        /// Save and update the relevant ScanState used when rescanning tokens during a REPLACE operation.
        /// </summary>
        private class ScanStateTracker
        {
            private readonly AbstractReplaceTokensLinesIterator _parentIterator;
            private readonly List<Token> _returnedTokensForCurrentLine;
            private MultilineScanState _scanState;
            private ITokensLine _currentLine;
            private int _scanStateIndex;

            public ScanStateTracker(AbstractReplaceTokensLinesIterator parentIterator)
            {
                _parentIterator = parentIterator;
                _returnedTokensForCurrentLine = new List<Token>();
                _scanState = null;
                _currentLine = null;
                _scanStateIndex = -1;
            }

            public void AccumulateToken(Token returnedToken)
            {
                //Detect new line
                if (_currentLine != returnedToken.TokensLine)
                {
                    //Reset if it's not a continuation line
                    if (_returnedTokensForCurrentLine.Count > 0 && !returnedToken.IsContinuationToken)
                    {
                        Reset();
                    }

                    _currentLine = returnedToken.TokensLine;
                }

                _returnedTokensForCurrentLine.Add(returnedToken);
            }

            private void Reset()
            {
                _returnedTokensForCurrentLine.Clear();
                _scanState = null;
                _scanStateIndex = -1;
            }

            public MultilineScanState GetCurrentScanState()
            {
                if (_currentLine == null)
                {
                    // No token accumulated yet.
                    return null;
                }

                int tokenIndex;
                MultilineScanState initialScanState;
                if (_scanState == null)
                {
                    // Get ScanState by rescanning whole line
                    tokenIndex = 0;
                    initialScanState = _currentLine.InitialScanState;
                }
                else if (_returnedTokensForCurrentLine.Count - 1 > _scanStateIndex)
                {
                    // ScanState has been computed but is outdated, rescan starting from first token after current index
                    tokenIndex = _scanStateIndex + 1;
                    initialScanState = _scanState;
                }
                else
                {
                    // ScanState is up-to-date
                    return _scanState;
                }

                // Build text to scan from accumulated tokens
                var line = new StringBuilder();
                for (; tokenIndex < _returnedTokensForCurrentLine.Count; tokenIndex++)
                {
                    line.Append(_returnedTokensForCurrentLine[tokenIndex].Text);
                    line.Append(' ');
                }

                // Scan
                var virtualLine = TokensLine.CreateVirtualLineForInsertedToken(0, line.ToString(), ColumnsLayout.FreeTextFormat);
                Scanner.Scanner.ScanTokensLine(virtualLine, initialScanState, _parentIterator.CompilerOptions, new List<RemarksDirective.TextNameVariation>());

                // Update state variables
                _scanState = virtualLine.ScanState;
                _scanStateIndex = tokenIndex - 1;
                return _scanState;
            }
        }

        // Current iterator position
        private Position _currentPosition;

        // Previous snapshot position
        private object _snapshotPosition;

        // Options entered in CLI
        protected TypeCobolOptions CompilerOptions { get; }

        private readonly ScanStateTracker _scanStateTracker;

        /// <summary>
        /// Name or path of the text document where the current Token was found.
        /// If the current token was found in COPY CPY1 imported by PROGRAM PGM1 :
        /// DocumentPath = "PGM1/CPY1"
        /// </summary>
        public string DocumentPath => _sourceIterator.DocumentPath;

        /// <summary>
        /// Current line index in the main text document 
        /// (PROGRAM or CLASS source file).
        /// If a COPY directive was found on line index 12 in the main program file,
        /// when the iterator returns the first token of the secondary copy file :
        /// DocumentPath = "PGM1/CPY1"
        /// LineIndex = 0 (in file CPY1)
        /// LineIndexInMainDocument = 12 (in file PGM1)
        /// </summary>
        public int LineIndexInMainDocument => _sourceIterator.LineIndexInMainDocument;

        /// <summary>
        /// Current column index 
        /// (in the text document identified by DocumentPath)
        /// </summary>
        public int ColumnIndex => _sourceIterator.ColumnIndex;

        /// <summary>
        /// Current line index 
        /// (in the text document identified by DocumentPath)
        /// </summary>
        public int LineIndex => _sourceIterator.LineIndex;

        /// <summary>
        /// Current tokens line
        /// </summary>
        public ITokensLine CurrentLine => _sourceIterator.CurrentLine;

        /// <summary>
        /// Returns the last token of the last line before EOF
        /// </summary>
        public ITokensLine LastLine => _sourceIterator.LastLine;

        public Token NextToken()
        {
            // If the iterator is in the process of returning several replacement tokens
            // => return the next replacement tokens until we reach then end of the group
            if (_currentPosition.ReplacementTokensBeingReturned != null)
            {
                _currentPosition.ReplacementTokenIndexLastReturned++;
                Token nextToken = _currentPosition.ReplacementTokensBeingReturned[_currentPosition.ReplacementTokenIndexLastReturned];
                if (_currentPosition.ReplacementTokenIndexLastReturned == _currentPosition.ReplacementTokensBeingReturned.Length - 1)
                {
                    _currentPosition.ReplacementTokensBeingReturned = null;
                    _currentPosition.ReplacementTokenIndexLastReturned = 0;
                }

                _currentPosition.CurrentToken = nextToken;
                _scanStateTracker.AccumulateToken(nextToken);
                return nextToken;
            }
            // Analyze the next token returned by the underlying iterator
            else
            {
                var check = CheckNextTokenBeforeReplace(_currentPosition.ReplaceOperations);

                Token nextToken = check.NextToken;
                _currentPosition.ReplaceOperations = check.UpdatedReplaceOperations;

                if (!check.ApplyReplace)
                {
                    _currentPosition.CurrentToken = nextToken;
                    _scanStateTracker.AccumulateToken(nextToken);
                    return nextToken;
                }

                // Apply the current REPLACE operations in effect
                if (_currentPosition.ReplaceOperations != null)
                {
                    ReplaceStatus status = null;
                    do
                    {
                        Token lastReplacedToken = null;
                        bool matchingMode = false;
                        foreach (ReplaceOperation replaceOperation in _currentPosition.ReplaceOperations)
                        {
                            status = TryAndReplace(lastReplacedToken ?? nextToken, replaceOperation);
                            lastReplacedToken = status.ReplacedToken ?? lastReplacedToken;
                            matchingMode = lastReplacedToken != null;
                            if (status.TryAgain)
                            {
                                if (matchingMode)
                                {
                                    _sourceIterator.ReturnToLastPositionSnapshot();
                                }
                                else
                                {
                                    nextToken = _sourceIterator.NextToken();
                                    break;
                                }
                            }
                            else if (status.ReplacedToken != null)
                            {
                                if (replaceOperation.Type == ReplaceOperationType.PartialWord)
                                {
                                    var partialWordReplaceOperation = (PartialWordReplaceOperation)replaceOperation;
                                    var replacement = partialWordReplaceOperation.PartialReplacementToken;
                                    if (status.ReplacedToken.Text != replacement?.Text)
                                    {
                                        continue;
                                    }
                                }

                                break;
                            }
                        }

                        if (matchingMode)
                        {
                            _scanStateTracker.AccumulateToken(lastReplacedToken);
                            return lastReplacedToken;
                        }
                    } while (status != null && status.TryAgain);
                }

                // If no replacement took place, simply return the next token of the underlying iterator
                _currentPosition.CurrentToken = nextToken;
                _scanStateTracker.AccumulateToken(nextToken);
                return nextToken;
            }
        }

        protected struct CheckTokenStatus
        {
            //TODO wait to rewrite AutoReplace iterator with this class to see if this bool is useful or not
            public bool ApplyReplace;
            public Token NextToken;
            public IReadOnlyList<ReplaceOperation> UpdatedReplaceOperations;
        }

        protected abstract CheckTokenStatus CheckNextTokenBeforeReplace(IReadOnlyList<ReplaceOperation> currentReplaceOperations);

        protected Token SourceIteratorNextToken() => _sourceIterator.NextToken();

        private class ReplaceStatus
        {
            public bool TryAgain;
            public Token ReplacedToken;
        }

        private ReplaceStatus TryAndReplace(Token nextToken, ReplaceOperation replaceOperation)
        {
            ReplaceStatus status = new ReplaceStatus();
            if (replaceOperation != null && TryMatchReplaceOperation(nextToken, replaceOperation, out var originalMatchingTokens))
            {
                status.ReplacedToken = CreateReplacedTokens(nextToken, replaceOperation, originalMatchingTokens);
                if (status.ReplacedToken != null)
                {
                    // REPLACE pattern matched => return the first replaced token
                    _currentPosition.CurrentToken = status.ReplacedToken;
                }
                else
                {
                    // If the replacement token set is empty (REPLACE == ... = BY == ==), get next token and try again
                    status.TryAgain = true;
                }
            }

            return status;
        }

        /// <summary>
        /// Get null (before the first call to NextToken()), current token, or EndOfFile
        /// </summary>
        public Token CurrentToken => _currentPosition.CurrentToken;

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
                    _sourceIterator.SaveCurrentPositionSnapshot();
                    bool comparisonInterrupted = false;
                    foreach (Token comparisonToken in multipleTokensReplaceOperation.FollowingComparisonTokens)
                    {
                        Token nextCandidateToken = _sourceIterator.NextToken();
                        if (!nextCandidateToken.CompareForReplace(comparisonToken))
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
                        // Restore the underlying iterator position
                        _sourceIterator.ReturnToLastPositionSnapshot();
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
                        // Special case for PictureCharacterString, handle as PartialWord
                        if (originalToken.TokenType == TokenType.PictureCharacterString)
                        {
                            var generatedTokenForSingleToken = RegexReplace(singleTokenReplaceOperation.ComparisonToken, singleTokenReplaceOperation.ReplacementToken);
                            return new ReplacedToken(generatedTokenForSingleToken, originalToken);
                        }

                        var replacedTokens = RescanReplacedTokenTypes(t => new ReplacedToken(t, originalToken), originalToken, singleTokenReplaceOperation.ReplacementToken);
                        return replacedTokens[0];
                    }
                    else
                    {
                        return null;
                    }
                // One pure partial word => one replacement token
                case ReplaceOperationType.PartialWord:
                    PartialWordReplaceOperation partialWordReplaceOperation = (PartialWordReplaceOperation)replaceOperation;
                    var generatedTokenForPartialCobolWord = RegexReplace(partialWordReplaceOperation.ComparisonToken, partialWordReplaceOperation.PartialReplacementToken);
                    return new ReplacedPartialCobolWord(generatedTokenForPartialCobolWord, partialWordReplaceOperation.PartialReplacementToken, originalToken);

                // One comparison token => more than one replacement tokens
                case ReplaceOperationType.SingleToMultipleTokens:
                    {
                        SingleToMultipleTokensReplaceOperation singleToMultipleTokensReplaceOperation = (SingleToMultipleTokensReplaceOperation)replaceOperation;
                        _currentPosition.ReplacementTokensBeingReturned = new Token[singleToMultipleTokensReplaceOperation.ReplacementTokens.Length];
                        _currentPosition.ReplacementTokensBeingReturned =
                            RescanReplacedTokenTypes(t => new ReplacedToken(t, originalToken), originalToken, singleToMultipleTokensReplaceOperation.ReplacementTokens);
                        _currentPosition.ReplacementTokenIndexLastReturned = 0;
                        return _currentPosition.ReplacementTokensBeingReturned[_currentPosition.ReplacementTokenIndexLastReturned];
                    }

                // One first + several following comparison tokens => zero to many replacement tokens                
                //case ReplaceOperationType.MultipleTokens:
                default:
                    {
                        MultipleTokensReplaceOperation multipleTokensReplaceOperation = (MultipleTokensReplaceOperation)replaceOperation;
                        if (multipleTokensReplaceOperation.ReplacementTokens != null)
                        {
                            System.Diagnostics.Debug.Assert(multipleTokensReplaceOperation.ReplacementTokens.Length > 0);
                            var replacedTokenGroups = RescanReplacedTokenTypes(t => new ReplacedTokenGroup(t, originalMatchingTokens),
                            originalMatchingTokens.Count > 0 ? originalMatchingTokens[0] : null, multipleTokensReplaceOperation.ReplacementTokens);
                            if (multipleTokensReplaceOperation.ReplacementTokens.Length > 1)
                            {
                                _currentPosition.ReplacementTokenIndexLastReturned = 0;
                                _currentPosition.ReplacementTokensBeingReturned = replacedTokenGroups;
                            }
                            return replacedTokenGroups[0];
                        }
                        else
                        {
                            return null;
                        }
                    }
            }

            // Performs a Regex Replace on original token using one comparison token and one replacement token
            Token RegexReplace(Token comparisonToken, Token replacementToken)
            {
                string normalizedTokenText = originalToken.NormalizedText;
                string normalizedPartToReplace = comparisonToken.NormalizedText;
                //#258 - ReplacementToken can be null. In this case, we consider that it's an empty replacement
                var replacementPart = replacementToken != null ? replacementToken.Text : string.Empty;
                string replacedTokenText = Regex.Replace(normalizedTokenText, normalizedPartToReplace, replacementPart, RegexOptions.IgnoreCase);
                var scanState = _scanStateTracker.GetCurrentScanState() ?? originalToken.TokensLine.InitialScanState;
                return GenerateReplacementToken(originalToken, replacedTokenText, scanState, CompilerOptions);
            }
        }

        internal static Token GenerateReplacementToken(Token originalToken, string replacedTokenText, MultilineScanState scanState, TypeCobolOptions scanOptions)
        {
            TokensLine tempTokensLine = TokensLine.CreateVirtualLineForInsertedToken(0, replacedTokenText, originalToken.TokensLine.ColumnsLayout);
            tempTokensLine.InitializeScanState(scanState);

            Token generatedToken;
            if (replacedTokenText.Length > 0)
            {
                Scanner.Scanner tempScanner = new Scanner.Scanner(replacedTokenText, 0, replacedTokenText.Length - 1, tempTokensLine, scanOptions);
                generatedToken = tempScanner.GetNextToken();
            }
            else
            {
                // Create an empty SpaceSeparator token.
                generatedToken = new Token(TokenType.SpaceSeparator, 0, -1, tempTokensLine);
            }

            // TODO scanning may have produced errors, they are lost here.

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
            if (replacementTokens.Any(MultilineScanState.IsScanStateDependent))
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
                int endTokIdx;
                List<T> newReplacedTokens = new List<T>(replacementTokens.Length);
                TokensLine tempTokensLine = TokensLine.CreateVirtualLineForInsertedToken(0, tokenText, firstOriginalToken.TokensLine.ColumnsLayout);
                var initialScanState = _scanStateTracker.GetCurrentScanState() ?? firstOriginalToken.TokensLine.InitialScanState;
                tempTokensLine.InitializeScanState(initialScanState);
                var tempScanner = new TypeCobol.Compiler.Scanner.Scanner(tokenText, 0, tokenText.Length - 1, tempTokensLine, CompilerOptions);
                Token rescannedToken;
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
                int CreateReplacedToken()
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
                for (int i = 0; i < replacementTokens.Length; i++)
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
            _currentPosition.SourceIteratorPosition = _sourceIterator.GetCurrentPosition();
            return _currentPosition;
        }

        /// <summary>
        /// Sets the current iterator position to a previous position returned by GetCurrentPosition.
        /// After a call to this method, GetNextToken returns the token FOLLOWING the current position.
        /// </summary>
        public void SeekToPosition(object iteratorPosition)
        {
            // Restore iterators positions
            _currentPosition = (Position)iteratorPosition;
            _sourceIterator.SeekToPosition(_currentPosition.SourceIteratorPosition);
        }

        public virtual void SeekToLineInMainDocument(int line)
        {
            _currentPosition = new Position();
            _sourceIterator.SeekToLineInMainDocument(line);
            _currentPosition.ReplaceOperations = (IReadOnlyList<ReplaceOperation>)((CodeElementsLine)CurrentLine).ActiveReplaceDirective?.ReplaceOperations;
        }

        /// <summary>
        /// Saves the current position of the iterator, to be able to restore it later
        /// </summary>
        public void SaveCurrentPositionSnapshot()
        {
            _snapshotPosition = GetCurrentPosition();
        }

        /// <summary>
        /// Restores the last position snapshot
        /// </summary>
        public void ReturnToLastPositionSnapshot()
        {
            SeekToPosition(_snapshotPosition);
        }
    }
}
