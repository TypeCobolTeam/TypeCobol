using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Antlr4.Runtime;
using TUVienna.CS_CUP.Runtime;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CupCommon
{
    /// <summary>
    /// Cobol Words Tokenizer to be used with CUP parser.
    /// </summary>
    public class CobolWordsTokenizer : TokensLinesIterator, TUVienna.CS_CUP.Runtime.Scanner, IEnumerable<TUVienna.CS_CUP.Runtime.Symbol>, IEnumerator<TUVienna.CS_CUP.Runtime.Symbol>
    {
        /// <summary>
        /// With CS CUP real toke start at 0, 0 is for EOF and 1 for error
        /// and we have introduced the CUP_ANY_TOKEN token and END_EXEC_PERIOD_SEPARATOR token.
        /// </summary>
        public const int CsCupStartToken = 4;
        
        /// <summary>
        /// The ID of the Any Token
        /// </summary>
        public const int ANY_TOKEN = 2;

        /// <summary>
        /// The ID of the CUP_END_EXEC_PERIOD_SEPARATOR
        /// </summary>
        public const int END_EXEC_PERIOD_SEPARATOR = 3;

        /// <summary>
        /// Any Token Category Modes
        /// </summary>
        public enum AnyTokenCategory
        {
            None,
            PseudoText,
            ControlCblCompilerStatement,
            DeleteCompilerStatement,
            EnterCompilerStatement,
            InsertCompilerStatement,
            ReadyOrResetTraceCompilerStatement,
            /// <summary>
            /// No further scanning mode ==> return EOF.
            /// </summary>
            StopScanningMode,
        }

        private AnyTokenCategory AnyTokenCategoryMode { get; set; }

        /// <summary>
        /// The EOF symbol
        /// </summary>
        public static TUVienna.CS_CUP.Runtime.Symbol EOF => new TUVienna.CS_CUP.Runtime.Symbol(0, null);

        /// <summary>
        /// Internal Symbol Yielder
        /// </summary>
        private IEnumerator<TUVienna.CS_CUP.Runtime.Symbol> _symbolYielder;
        /// <summary>
        /// The firts token read
        /// </summary>
        public Token FirstToken { get; private set; }
        /// <summary>
        /// The Last tOken read.
        /// </summary>
        public Token LastToken { get; private set; }

        /// <summary>
        /// The Last stop symbol encountered.
        /// </summary>
        public Symbol LastStopSymbol { get; private set; }

        /// <summary>
        /// Constructor à la TokensLinesIterator
        /// </summary>
        /// <param name="textName"></param>
        /// <param name="tokensLines"></param>
        /// <param name="startToken"></param>
        /// <param name="channelFilter"></param>
        public CobolWordsTokenizer(string textName, ISearchableReadOnlyList<ITokensLine> tokensLines, Token startToken, int channelFilter) : base(textName, tokensLines, startToken, channelFilter)
        {
            Reset();
        }

        /// <summary>
        /// Next Symbol for Cup Scanner
        /// </summary>
        /// <returns></returns>
        public Symbol next_token()
        {
            if (_symbolYielder.MoveNext())
                return _symbolYielder.Current;
            return EOF;
        }

        /// <summary>
        /// Revert the Last token to be the first one
        /// </summary>
        public void RevertLastToken()
        {
            LastToken = PreviousToken();
        }
        /// <summary>
        /// Is the Tokenizer in the Any Token Nmode ?
        /// </summary>
        public bool IsAnyTokenMode { get; private set; }
        /// <summary>
        /// Excluded Tokens from the Current Any Token Mode.
        /// </summary>
        private TokenType[] ExcludedTokens { get; set; }

        /// <summary>
        /// Call this method to enter in the any token mode
        /// </summary>
        /// <param name="excluded">Excludes Tokens from the Any Tokne Mode</param>
        public void EnterAnyTokenMode(params TokenType[] excluded)
        {
            ExcludedTokens = excluded;
            if (excluded != null && excluded.Length != 0)
                IsAnyTokenMode = true;
        }

        /// <summary>
        /// Call this method to enter in the any token mode only and only if the current token is the given one.
        /// </summary>
        /// <param name="current">The current to enter in the Any Mode</param>
        /// <param name="excluded">Excludes Tokens from the Any Token Mode</param>
        public void EnterAnyTokenModeIfCurrentToken(TokenType current, params TokenType[] excluded)
        {
            if (this.Current == null)
                return;
            if (this.Current.sym == TokenType2CupTokenType(current))
                EnterAnyTokenMode(excluded);
        }

        /// <summary>
        /// Enter Any Token Mode for Pseudotext
        /// </summary>
        public void EnterPseudoTextAnyTokenMode()
        {
            if (this.Current == null)
                return;
            if (this.Current.sym == TokenType2CupTokenType(TokenType.PseudoTextDelimiter)) {
                AnyTokenCategoryMode = AnyTokenCategory.PseudoText;
                EnterAnyTokenMode(TokenType.PseudoTextDelimiter, TokenType.PseudoTextDelimiter, TokenType.COPY);
            }
        }

        /// <summary>
        /// Leave Any Token Mode for Pseudotext
        /// </summary>
        public void LeavePseudoTextAnyTokenMode()
        {
            AnyTokenCategoryMode = AnyTokenCategory.None;
            LeaveAnyTokenMode();
        }

        /// <summary>
        /// Enter the Stop Scanning Mode.
        /// </summary>
        public void EnterStopScanningMode()
        {
            LeaveAnyTokenMode();
            AnyTokenCategoryMode = AnyTokenCategory.StopScanningMode;
        }

        /// <summary>
        /// Leave the stop scanning mode
        /// </summary>
        public void LeaveStopScanningMode()
        {
            AnyTokenCategoryMode = AnyTokenCategory.None;
            LeaveAnyTokenMode();
        }

        /// <summary>
        /// Enter the Contol Cbl Compiler Statement mode: in this mode
        /// all UserDefinedWord are recognized and the PeriodSeparator also,
        /// otherwise we enter in the StopScanningMode.
        /// </summary>
        public void EnterControlCblCompilerStatementMode()
        {
            LeaveAnyTokenMode();
            AnyTokenCategoryMode = AnyTokenCategory.ControlCblCompilerStatement;
        }

        /// <summary>
        /// Enter the Delete Compiler Statement mode: in this mode
        /// Only IntegerLiteral are recognized, otherwise we enter in the StopScanningMode.
        /// </summary>
        public void EnterDeleteCompilerStatementMode()
        {
            LeaveAnyTokenMode();
            AnyTokenCategoryMode = AnyTokenCategory.DeleteCompilerStatement;
        }

        /// <summary>
        /// Enter the Enter Compiler Statement scanning mode.
        /// </summary>
        public void EnterEnterCompilerStatementMode()
        {
            LeaveAnyTokenMode();
            AnyTokenCategoryMode = AnyTokenCategory.EnterCompilerStatement;
        }

        /// <summary>
        /// Enter the Insert Compiler Statement mode: in this mode
        /// Only IntegerLiteral are recognized, otherwise we enter in the StopScanningMode.
        /// </summary>
        public void EnterInsertCompilerStatementMode()
        {
            LeaveAnyTokenMode();
            AnyTokenCategoryMode = AnyTokenCategory.InsertCompilerStatement;
        }

        /// <summary>
        /// Enter the Ready or Reset Compiler Statement mode: in this mode
        /// </summary>
        public void EnterReadyOrResetTraceCompilerStatementMode()
        {
            LeaveAnyTokenMode();
            AnyTokenCategoryMode = AnyTokenCategory.ReadyOrResetTraceCompilerStatement;
        }

        /// <summary>
        /// Leave the any token mode.
        /// </summary>
        public void LeaveAnyTokenMode(bool bAllMode = false)
        {
            if (bAllMode)
            {
                AnyTokenCategoryMode = AnyTokenCategory.None;
            }
            IsAnyTokenMode = false;
        }

        /// <summary>
        /// Consume the given token type if it is the next token type at the same line than the current Token.
        /// </summary>
        /// <param name="nextTokenType">The next token type to check</param>
        public void ConsumeNextTokenOnTheSameLine(TokenType nextTokenType)
        {
            Token currentToken = base.CurrentToken;
            if (currentToken == Token.END_OF_FILE)
                return;//Ignore if end of file
            Token nextToken = base.NextToken();
            if (nextToken != null && currentToken != null &&
                nextToken.TokensLine == currentToken.TokensLine && nextToken.TokenType == nextTokenType)
            {
                LastToken = nextToken;
                return;//Consume it
            }
            else if (nextToken != null && currentToken != null)
            {//Rollback
                base.PreviousToken();
            }
        }

        /// <summary>
        /// Same as ConsumeNextTokenOnTheSameLine(..) but stop scanning afterward.
        /// </summary>
        /// <param name="nextTokenType"></param>
        public void ConsumeNextTokenOnTheSameLineAndStop(TokenType nextTokenType)
        {
            ConsumeNextTokenOnTheSameLine(nextTokenType);
            EnterStopScanningMode();
        }

        /// <summary>
        /// Basic Behavior for the Any Tokern Mode
        /// </summary>
        /// <param name="token"></param>
        /// <param name="symbol"></param>
        /// <returns>true if the AnyTokenMode was handled, fals eotherwise.</returns>
        private bool BasicAnyTokenMode(Token token, Symbol symbol)
        {
            if (IsAnyTokenMode)
            {
                if (ExcludedTokens.Contains(token.TokenType))
                {
                    //Leave the mode, the token is an excluded token.
                    IsAnyTokenMode = false;
                }
                else
                {
                    //Change the symbol to the any token symbol.
                    symbol.sym = ANY_TOKEN;
                }
                return true;
            }
            else
            {
                return false;
            }
        }

        /// <summary>
        /// Try to match the next token with the given expected token
        /// If match a resulting token is returned, otherwise a default token is returned.
        /// </summary>
        /// <param name="expected">The expected next token</param>
        /// <param name="resulting">The resulting token if matching</param>
        /// <param name="defaultToken">The defualt token if no matching</param>
        /// <returns></returns>
        private int TryMatchNextToken(TokenType expected, int resulting, int defaultToken)
        {
            Token nextToken = base.NextToken();
            if (nextToken != null)
            {
                base.PreviousToken();
                if (nextToken.TokenType == expected)
                {                    
                    return resulting;
                }
            }
            return defaultToken;
        }

        /// <summary>
        /// Handle the any token mode.
        /// </summary>
        /// <param name="token"></param>
        /// <param name="symbol"></param>
        private bool HandleAnyTokenMode(Token token, Symbol symbol)
        {
            switch (AnyTokenCategoryMode)
            {
                case AnyTokenCategory.None:
                {
                    BasicAnyTokenMode(token, symbol);
                }
                    break;
                case AnyTokenCategory.PseudoText:
                {
                    if (!BasicAnyTokenMode(token, symbol))
                        if (token.TokenType == TokenType.PseudoTextDelimiter)
                        {//Enter again in the any token mode
                            EnterAnyTokenMode(TokenType.PseudoTextDelimiter, TokenType.PseudoTextDelimiter, TokenType.COPY);
                        }
                    if (token.TokenType == TokenType.END_EXEC && symbol.sym == TokenType2CupTokenType(TokenType.END_EXEC))
                    {
                        symbol.sym = TryMatchNextToken(TokenType.PeriodSeparator, END_EXEC_PERIOD_SEPARATOR, symbol.sym);
                    }
                }
                    break;
                case AnyTokenCategory.ControlCblCompilerStatement:
                {
                    return (token.TokenType == TokenType.UserDefinedWord || token.TokenType == TokenType.PeriodSeparator);
                }
                case AnyTokenCategory.DeleteCompilerStatement:
                {
                    return token.TokenType == TokenType.IntegerLiteral;
                }
                case AnyTokenCategory.InsertCompilerStatement:
                {   //Only one time
                    EnterStopScanningMode();
                    return token.TokenType == TokenType.IntegerLiteral;
                }
                case AnyTokenCategory.EnterCompilerStatement:
                {
                    return (token.TokenType == TokenType.UserDefinedWord || token.TokenType == TokenType.PeriodSeparator);
                }
                case AnyTokenCategory.ReadyOrResetTraceCompilerStatement:
                {
                    return (token.TokenType == TokenType.TRACE || token.TokenType == TokenType.PeriodSeparator);
                }
                    break;
                case AnyTokenCategory.StopScanningMode:
                {
                    return false;
                }
            }
            return true;
        }

        /// <summary>
        /// Symbol Enumerator over Scanner.Token
        /// </summary>
        /// <returns></returns>
        public IEnumerator<Symbol> GetEnumerator()
        {
            FirstToken = null;
            LastToken = null;            
            Token token = null;
            while ((token = base.NextToken()) != Token.END_OF_FILE)
            {
                if (FirstToken == null)
                {
                    this.FirstToken = token;
                }
                TUVienna.CS_CUP.Runtime.Symbol symbol = new TUVienna.CS_CUP.Runtime.Symbol(((int)token.TokenType) + CsCupStartToken - 1, token);
                if (!HandleAnyTokenMode(token, symbol))
                {
                    LastStopSymbol = symbol;
                    break;
                }
                this.LastToken = token;
                yield return symbol;
            }
            yield return EOF;
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public void Dispose()
        {
            _symbolYielder = null;
        }

        /// <summary>
        /// Move to the Next Symbol
        /// </summary>
        /// <returns>true if there is Symbol, false otherwise.</returns>
        public bool MoveNext()
        {
            return _symbolYielder != null && _symbolYielder.MoveNext();
        }

        /// <summary>
        /// Reset the Tokenizer
        /// <param name="bBaseAlso">true if the base class must be reset also.</param>
        /// </summary>
        public new void Reset(bool bBaseAlso = true)
        {
            if (bBaseAlso)
                base.Reset();
            LeaveAnyTokenMode(true);
            _symbolYielder = GetEnumerator();
        }

        /// <summary>
        /// The Current Symbol
        /// </summary>
        public TUVienna.CS_CUP.Runtime.Symbol Current => _symbolYielder.Current;

        object IEnumerator.Current
        {
            get { return Current; }
        }

        /// <summary>
        /// Get the string representation of a CodeElementType
        /// </summary>
        /// <param name="ceType"></param>
        /// <returns></returns>
        public static string ToString(TokenType ceType)
        {
            return TokenUtils.GetDisplayNameForTokenType(ceType);
        }

        /// <summary>
        /// Get the string representation of the CodeElementType correponding to a Cup Token.
        /// </summary>
        /// <param name="token"></param>
        /// <returns></returns>
        public static string CupTokenToString(int token)
        {
            return ToString((TokenType)(token - CsCupStartToken + 1));
        }

        /// <summary>
        /// Get the Cup Token Type corresponding to a TokenType.
        /// </summary>
        /// <param name="t">The TokenType to get the Cupt Token Type.</param>
        /// <returns>The Cup Token Type.</returns>
        public int TokenType2CupTokenType(TokenType t)
        {
            return (int)t + CsCupStartToken - 1;
        }
    }
}
