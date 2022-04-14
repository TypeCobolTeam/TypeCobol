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
        /// and we have introduced the CUP_ANY_TOKEN token , END_EXEC_PERIOD_SEPARATOR token
        /// CUP_LiteralOrUserDefinedWordOReservedWordExceptCopy token, CUP_PSEUDO_TEXT_DELIMITER_BY
        /// </summary>
        public const int CsCupStartToken = 6;
        
        /// <summary>
        /// The ID of the Any Token
        /// </summary>
        public const int ANY_TOKEN = 2;

        /// <summary>
        /// The ID of the CUP_END_EXEC_PERIOD_SEPARATOR
        /// </summary>
        public const int END_EXEC_PERIOD_SEPARATOR = 3;

        /// <summary>
        /// The ID of the CUP_LiteralOrUserDefinedWordOReservedWordExceptCopy
        /// </summary>
        public const int CUP_LITERALORUSERDEFINEDWORDORESERVEDWORDEXCEPTCOPY = 4;

        /// <summary>
        /// Combination of == BY in REPLACING mode
        /// </summary>
        public const int PSEUDO_TEXT_DELIMITER_BY = 5;

        /// <summary>
        /// Any Token Category Modes
        /// </summary>
        public enum AnyTokenCategory
        {
            None,
            PseudoText,
            ControlCblCompilerStatement,
            /// <summary>
            /// No further scanning mode ==> return EOF.
            /// </summary>
            StopScanningMode,
        }

        /// <summary>
        /// Flag to detect iteralOrUserDefinedWordOReservedWordExceptCopy mode
        /// </summary>
        public bool CheckLiteralOrUserDefinedWordOReservedWordExceptCopy { get; set; }

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
        public void RevertLastToken(Symbol lastMismatchedSymbol)
        {
            if (LastToken == null)
                return;
            if (LastToken.TokenType == TokenType.PeriodSeparator)
                return;
            Token prevToken = PreviousToken();
            if (prevToken == null)
                return;            
            LastToken = prevToken;
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
        /// Are we in the Stop Scanning Mode.
        /// </summary>
        public bool IsStopScanningMode => AnyTokenCategoryMode == AnyTokenCategory.StopScanningMode;

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
            if (currentToken != null && currentToken.TokenType == TokenType.EndOfFile)
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
        /// Enter in the Stop Scanning Mode if the next is not of the given type
        /// </summary>
        /// <param name="nextTokenType"></param>
        public void EnterStopScanningModeIfNextNotToken(TokenType nextTokenType)
        {
            Token currentToken = base.CurrentToken;
            if (currentToken == null)
                return;
            if (currentToken.TokenType == TokenType.EndOfFile)
                return;//Ignore if end of file
            Token nextToken = base.NextToken();
            base.PreviousToken();
            if (nextToken != null && nextToken.TokenType != nextTokenType)
            {
                EnterStopScanningMode();
            }
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
        /// <param name="defaultToken">The default token if no matching</param>
        /// <returns></returns>
        private int TryMatchNextToken(TokenType expected, int resulting, int defaultToken)
        {
            var currentToken = base.CurrentToken;
            if (currentToken != null && currentToken.TokenType == TokenType.EndOfFile)
                return defaultToken;
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
        /// Try to match the prev token with the given expected token
        /// If match a resulting token is returned, otherwise a default token is returned.
        /// </summary>
        /// <param name="expected">The expected next token</param>
        /// <param name="resulting">The resulting token if matching</param>
        /// <param name="defaultToken">The default token if no matching</param>
        /// <returns></returns>
        private int TryMatchPrevToken(TokenType expected, int resulting, int defaultToken)
        {
            if (base.CurrentToken == null)
                return defaultToken;
            Token prevToken = base.PreviousToken();
            if (prevToken != null)
            {
                base.NextToken();
                if (prevToken.TokenType == expected)
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
        /// <returns>true if the scanning can continue, false otherwise</returns>
        private bool HandleAnyTokenMode(Token token, Symbol symbol)
        {
            if (this.CheckLiteralOrUserDefinedWordOReservedWordExceptCopy && !IsAnyTokenMode)
            {
                if (token.TokenType == TokenType.END_EXEC && symbol.sym == TokenType2CupTokenType(TokenType.END_EXEC))
                {
                    symbol.sym = TryMatchNextToken(TokenType.PeriodSeparator, END_EXEC_PERIOD_SEPARATOR, symbol.sym);
                    return true;
                }
                if (IsLiteralOrUserDefinedWordOReservedWordExceptCopy(token))
                {
                    if (token.TokenType == TokenType.BY)
                    {
                        symbol.sym = TryMatchPrevToken(TokenType.PseudoTextDelimiter, PSEUDO_TEXT_DELIMITER_BY, symbol.sym);
                    }
                    else
                    {
                        symbol.sym = CUP_LITERALORUSERDEFINEDWORDORESERVEDWORDEXCEPTCOPY;
                    }                    
                }
            }
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
                    {
                        if (token.TokenType == TokenType.PseudoTextDelimiter)
                        {//Enter again in the any token mode
                            EnterAnyTokenMode(TokenType.PseudoTextDelimiter, TokenType.PseudoTextDelimiter, TokenType.COPY);
                        }
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
            while ((token = base.NextToken(!IsAnyTokenMode)).TokenType != TokenType.EndOfFile)
            {
                if (FirstToken == null)
                {
                    this.FirstToken = token;
                }
                TUVienna.CS_CUP.Runtime.Symbol symbol = new TUVienna.CS_CUP.Runtime.Symbol(((int)token.TokenType) + CsCupStartToken - 1, token);
                if (_inPerformModeState > 0)
                {
                    if (!HandlePerformMode(token))
                    {
                        _inPerformModeState = 0;
                        break;
                    }
                }
                else if (!HandleAnyTokenMode(token, symbol))
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

        private int _inPerformModeState = 0;
        public void EnterPerformMode()
        {
            _inPerformModeState = 1;
        }
        public void LeavePerformMode()
        {
            _inPerformModeState = 0;
        }
        private bool HandlePerformMode(Token token)
        {
            switch(_inPerformModeState)
            {
                case 0:
                    return true;
                case 1:
                    if (token.TokenType != TokenType.UserDefinedWord)
                        return false;
                    _inPerformModeState = 2;
                    return true;
                case 2:
                    if (token.TokenType != TokenType.TIMES)
                        return false;
                    _inPerformModeState = 3;
                    return true;
                case 3:
                    return false;
            }
            return true;
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
        /// </summary>

        public override void Reset()
        {
            Reset(true);
        }

        /// <summary>
        /// Reset the Tokenizer
        /// <param name="bBaseAlso">true if the base class must be reset also.</param>
        /// </summary>
        public void Reset(bool bBaseAlso)
        {
            if (bBaseAlso)
                base.Reset();
            CheckLiteralOrUserDefinedWordOReservedWordExceptCopy = false;
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
            switch (token)
            {
                case 0:
                    return "EOF";
                case ANY_TOKEN:
                    return "AnyToken";
                case END_EXEC_PERIOD_SEPARATOR:
                    return null;
                case CUP_LITERALORUSERDEFINEDWORDORESERVEDWORDEXCEPTCOPY:
                    return "Literal, UserDefinedWord, ReservedWord";
                case PSEUDO_TEXT_DELIMITER_BY:
                    return null;
            }
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

        /// <summary>
        /// Get the TokenType corresponding to a Cup Token.
        /// </summary>
        /// <param name="t"></param>
        /// <returns>The Corresponding TokenType</returns>
        public TokenType CupTokenType2TokenType(int t)
        {
            return (TokenType)(t - CsCupStartToken + 1);
        }

        /// <summary>
        /// Checks if the given Token is a candidate for:
        /// LiteralOrUserDefinedWordOReservedWordExceptCopy
        ///         
        // This list of reserved words is useful to parse the COPY REPLACING operands
        // -> it must be updated each time a new token type is added int the CobolWordsSymbols.cup
        /// 
        /// </summary>
        /// <param name="token"></param>
        /// <returns></returns>
        public static bool IsLiteralOrUserDefinedWordOReservedWordExceptCopy(Token token)
        {
            switch (token.TokenType)
            {
                case TokenType.AlphanumericLiteral:
                case TokenType.HexadecimalAlphanumericLiteral:
                case TokenType.NullTerminatedAlphanumericLiteral:
                case TokenType.NationalLiteral:
                case TokenType.HexadecimalNationalLiteral:
                case TokenType.DBCSLiteral:
                // Literals - Numeric
                case TokenType.LevelNumber:
                case TokenType.IntegerLiteral:
                case TokenType.DecimalLiteral:
                case TokenType.FloatingPointLiteral:
                // Literals - Syntax tokens
                // => excluded


                // Symbols
                case TokenType.SectionParagraphName:
                case TokenType.IntrinsicFunctionName:
                case TokenType.ExecTranslatorName:
                case TokenType.PartialCobolWord:
                case TokenType.UserDefinedWord:
                // Keywords - Compiler directive starting tokens
                case TokenType.ASTERISK_CBL:
                case TokenType.ASTERISK_CONTROL:
                case TokenType.BASIS:
                case TokenType.CBL:
                // COPY => excluded
                case TokenType.DELETE_CD:
                case TokenType.EJECT:
                case TokenType.ENTER:
                case TokenType.EXEC_SQL:
                case TokenType.INSERT:
                case TokenType.PROCESS:
                case TokenType.READY:
                case TokenType.RESET:
                case TokenType.REPLACE:
                case TokenType.SERVICE_CD:
                case TokenType.SKIP1:
                case TokenType.SKIP2:
                case TokenType.SKIP3:
                case TokenType.TITLE:
                // Keywords - Code element starting tokens
                case TokenType.ACCEPT:
                case TokenType.ADD:
                case TokenType.ALTER:
                case TokenType.APPLY:
                case TokenType.CALL:
                case TokenType.CANCEL:
                case TokenType.CLOSE:
                case TokenType.COMPUTE:
                case TokenType.CONFIGURATION:
                case TokenType.CONTINUE:
                case TokenType.DATA:
                case TokenType.DECLARATIVES:
                case TokenType.DECLARE:
                case TokenType.DELETE:
                case TokenType.DISPLAY:
                case TokenType.DIVIDE:
                case TokenType.ELSE:
                case TokenType.END:
                case TokenType.END_ADD:
                case TokenType.END_CALL:
                case TokenType.END_COMPUTE:
                case TokenType.END_DECLARE:
                case TokenType.END_DELETE:
                case TokenType.END_DIVIDE:
                case TokenType.END_EVALUATE:
                case TokenType.END_EXEC:
                case TokenType.END_IF:
                case TokenType.END_INVOKE:
                case TokenType.END_MULTIPLY:
                case TokenType.END_PERFORM:
                case TokenType.END_READ:
                case TokenType.END_RETURN:
                case TokenType.END_REWRITE:
                case TokenType.END_SEARCH:
                case TokenType.END_START:
                case TokenType.END_STRING:
                case TokenType.END_SUBTRACT:
                case TokenType.END_UNSTRING:
                case TokenType.END_WRITE:
                case TokenType.END_XML:
                case TokenType.ENTRY:
                case TokenType.ENVIRONMENT:
                case TokenType.EVALUATE:
                case TokenType.EXEC:
                case TokenType.EXECUTE:
                case TokenType.EXIT:
                case TokenType.FD:
                case TokenType.FILE:
                case TokenType.FILE_CONTROL:
                case TokenType.GO:
                case TokenType.GOBACK:
                case TokenType.I_O_CONTROL:
                case TokenType.ID:
                case TokenType.IDENTIFICATION:
                case TokenType.IF:
                case TokenType.INITIALIZE:
                case TokenType.INPUT_OUTPUT:
                case TokenType.INSPECT:
                case TokenType.INVOKE:
                case TokenType.LINKAGE:
                case TokenType.LOCAL_STORAGE:
                case TokenType.MERGE:
                case TokenType.MOVE:
                case TokenType.MULTIPLE:
                case TokenType.MULTIPLY:
                case TokenType.NEXT:
                case TokenType.OBJECT_COMPUTER:
                case TokenType.OPEN:
                case TokenType.PERFORM:
                case TokenType.PROCEDURE:
                case TokenType.READ:
                case TokenType.RELEASE:
                case TokenType.REPOSITORY:
                case TokenType.RERUN:
                case TokenType.RETURN:
                case TokenType.REWRITE:
                case TokenType.SAME:
                case TokenType.SD:
                case TokenType.SEARCH:
                case TokenType.SELECT:
                case TokenType.SERVICE:
                case TokenType.SET:
                case TokenType.SORT:
                case TokenType.SOURCE_COMPUTER:
                case TokenType.SPECIAL_NAMES:
                case TokenType.START:
                case TokenType.STOP:
                case TokenType.STRING:
                case TokenType.SUBTRACT:
                case TokenType.UNSTRING:
                case TokenType.USE:
                case TokenType.WHEN:
                case TokenType.WORKING_STORAGE:
                case TokenType.WRITE:
                case TokenType.XML:
                case TokenType.GLOBAL_STORAGE:
                // Keywords - Special registers
                case TokenType.ADDRESS:
                case TokenType.DEBUG_CONTENTS:
                case TokenType.DEBUG_ITEM:
                case TokenType.DEBUG_LINE:
                case TokenType.DEBUG_NAME:
                case TokenType.DEBUG_SUB_1:
                case TokenType.DEBUG_SUB_2:
                case TokenType.DEBUG_SUB_3:
                case TokenType.JNIENVPTR:
                case TokenType.JSON_CODE:
                case TokenType.JSON_STATUS:
                case TokenType.LENGTH:
                case TokenType.LINAGE_COUNTER:
                case TokenType.RETURN_CODE:
                case TokenType.SHIFT_IN:
                case TokenType.SHIFT_OUT:
                case TokenType.SORT_CONTROL:
                case TokenType.SORT_CORE_SIZE:
                case TokenType.SORT_FILE_SIZE:
                case TokenType.SORT_MESSAGE:
                case TokenType.SORT_MODE_SIZE:
                case TokenType.SORT_RETURN:
                case TokenType.TALLY:
                case TokenType.WHEN_COMPILED:
                case TokenType.XML_CODE:
                case TokenType.XML_EVENT:
                case TokenType.XML_INFORMATION:
                case TokenType.XML_NAMESPACE:
                case TokenType.XML_NAMESPACE_PREFIX:
                case TokenType.XML_NNAMESPACE:
                case TokenType.XML_NNAMESPACE_PREFIX:
                case TokenType.XML_NTEXT:
                case TokenType.XML_TEXT:
                // Keywords - Figurative constants
                case TokenType.HIGH_VALUE:
                case TokenType.HIGH_VALUES:
                case TokenType.LOW_VALUE:
                case TokenType.LOW_VALUES:
                case TokenType.NULL:
                case TokenType.NULLS:
                case TokenType.QUOTE:
                case TokenType.QUOTES:
                case TokenType.SPACE:
                case TokenType.SPACES:
                case TokenType.ZERO:
                case TokenType.ZEROES:
                case TokenType.ZEROS:
                case TokenType.SymbolicCharacter:
                // Keywords - Special object identifiers
                case TokenType.SELF:
                case TokenType.SUPER:
                // Keywords - Syntax tokens
                case TokenType.ACCESS:
                case TokenType.ADVANCING:
                case TokenType.AFTER:
                case TokenType.ALL:
                case TokenType.ALPHABET:
                case TokenType.ALPHABETIC:
                case TokenType.ALPHABETIC_LOWER:
                case TokenType.ALPHABETIC_UPPER:
                case TokenType.ALPHANUMERIC:
                case TokenType.ALPHANUMERIC_EDITED:
                case TokenType.ALSO:
                case TokenType.ALTERNATE:
                case TokenType.AND:
                case TokenType.ANY:
                case TokenType.ARE:
                case TokenType.AREA:
                case TokenType.AREAS:
                case TokenType.ASCENDING:
                case TokenType.ASSIGN:
                case TokenType.AT:
                case TokenType.AUTHOR:
                case TokenType.BEFORE:
                case TokenType.BEGINNING:
                case TokenType.BINARY:
                case TokenType.BLANK:
                case TokenType.BLOCK:
                case TokenType.BOTTOM:
                case TokenType.BY:
                case TokenType.CHARACTER:
                case TokenType.CHARACTERS:
                case TokenType.CLASS:
                case TokenType.CLASS_ID:
                case TokenType.COBOL:
                case TokenType.CODE:
                case TokenType.CODE_SET:
                case TokenType.COLLATING:
                case TokenType.COM_REG:
                case TokenType.COMMA:
                case TokenType.COMMON:
                case TokenType.COMP:
                case TokenType.COMP_1:
                case TokenType.COMP_2:
                case TokenType.COMP_3:
                case TokenType.COMP_4:
                case TokenType.COMP_5:
                case TokenType.COMPUTATIONAL:
                case TokenType.COMPUTATIONAL_1:
                case TokenType.COMPUTATIONAL_2:
                case TokenType.COMPUTATIONAL_3:
                case TokenType.COMPUTATIONAL_4:
                case TokenType.COMPUTATIONAL_5:
                case TokenType.CONTAINS:
                case TokenType.CONTENT:
                case TokenType.CONVERTING:
                case TokenType.CORR:
                case TokenType.CORRESPONDING:
                case TokenType.COUNT:
                case TokenType.CURRENCY:
                case TokenType.DATE:
                case TokenType.DATE_COMPILED:
                case TokenType.DATE_WRITTEN:
                case TokenType.DAY:
                case TokenType.DAY_OF_WEEK:
                case TokenType.DBCS:
                case TokenType.DEBUGGING:
                case TokenType.DECIMAL_POINT:
                case TokenType.DELIMITED:
                case TokenType.DELIMITER:
                case TokenType.DEPENDING:
                case TokenType.DESCENDING:
                case TokenType.DISPLAY_1:
                case TokenType.DIVISION:
                case TokenType.DOWN:
                case TokenType.DUPLICATES:
                case TokenType.DYNAMIC:
                case TokenType.EGCS:
                case TokenType.END_OF_PAGE:
                case TokenType.ENDING:
                case TokenType.EOP:
                case TokenType.EQUAL:
                case TokenType.ERROR:
                case TokenType.EVERY:
                case TokenType.EXCEPTION:
                case TokenType.EXTEND:
                case TokenType.EXTERNAL:
                case TokenType.FACTORY:
                case TokenType.FALSE:
                case TokenType.FILLER:
                case TokenType.FIRST:
                case TokenType.FOOTING:
                case TokenType.FOR:
                case TokenType.FROM:
                case TokenType.FUNCTION:
                case TokenType.FUNCTION_POINTER:
                case TokenType.GENERATE:
                case TokenType.GIVING:
                case TokenType.GLOBAL:
                case TokenType.GREATER:
                case TokenType.GROUP_USAGE:
                case TokenType.I_O:
                case TokenType.IN:
                case TokenType.INDEX:
                case TokenType.INDEXED:
                case TokenType.INHERITS:
                case TokenType.INITIAL:
                case TokenType.INPUT:
                case TokenType.INSTALLATION:
                case TokenType.INTO:
                case TokenType.INVALID:
                case TokenType.IS:
                case TokenType.JUST:
                case TokenType.JUSTIFIED:
                case TokenType.KANJI:
                case TokenType.KEY:
                case TokenType.LABEL:
                case TokenType.LEADING:
                case TokenType.LEFT:
                case TokenType.LESS:
                case TokenType.LINAGE:
                case TokenType.LINE:
                case TokenType.LINES:
                case TokenType.LOCK:
                case TokenType.MEMORY:
                case TokenType.METHOD:
                case TokenType.METHOD_ID:
                case TokenType.MODE:
                case TokenType.MODULES:
                case TokenType.MORE_LABELS:
                case TokenType.NATIONAL:
                case TokenType.NATIONAL_EDITED:
                case TokenType.NATIVE:
                case TokenType.NEGATIVE:
                case TokenType.NEW:
                case TokenType.NO:
                case TokenType.NOT:
                case TokenType.NUMERIC:
                case TokenType.NUMERIC_EDITED:
                case TokenType.OBJECT:
                case TokenType.OCCURS:
                case TokenType.OF:
                case TokenType.OFF:
                case TokenType.OMITTED:
                case TokenType.ON:
                case TokenType.OPTIONAL:
                case TokenType.OR:
                case TokenType.ORDER:
                case TokenType.ORGANIZATION:
                case TokenType.OTHER:
                case TokenType.OUTPUT:
                case TokenType.OVERFLOW:
                case TokenType.OVERRIDE:
                case TokenType.PACKED_DECIMAL:
                case TokenType.PADDING:
                case TokenType.PAGE:
                case TokenType.PASSWORD:
                case TokenType.PIC:
                case TokenType.PICTURE:
                case TokenType.POINTER:
                case TokenType.POSITION:
                case TokenType.POSITIVE:
                case TokenType.PROCEDURE_POINTER:
                case TokenType.PROCEDURES:
                case TokenType.PROCEED:
                case TokenType.PROCESSING:
                case TokenType.PROGRAM:
                case TokenType.PROGRAM_ID:
                case TokenType.RANDOM:
                case TokenType.RECORD:
                case TokenType.RECORDING:
                case TokenType.RECORDS:
                case TokenType.RECURSIVE:
                case TokenType.REDEFINES:
                case TokenType.REEL:
                case TokenType.REFERENCE:
                case TokenType.REFERENCES:
                case TokenType.RELATIVE:
                case TokenType.RELOAD:
                case TokenType.REMAINDER:
                case TokenType.REMOVAL:
                case TokenType.RENAMES:
                case TokenType.REPLACING:
                case TokenType.RESERVE:
                case TokenType.RETURNING:
                case TokenType.REVERSED:
                case TokenType.REWIND:
                case TokenType.RIGHT:
                case TokenType.ROUNDED:
                case TokenType.RUN:
                case TokenType.SECTION:
                case TokenType.SECURITY:
                case TokenType.SEGMENT_LIMIT:
                case TokenType.SENTENCE:
                case TokenType.SEPARATE:
                case TokenType.SEQUENCE:
                case TokenType.SEQUENTIAL:
                case TokenType.SIGN:
                case TokenType.SIZE:
                case TokenType.SORT_MERGE:
                case TokenType.SQL:
                case TokenType.SQLIMS:
                case TokenType.STANDARD:
                case TokenType.STANDARD_1:
                case TokenType.STANDARD_2:
                case TokenType.STATUS:
                case TokenType.SUPPRESS:
                case TokenType.SYMBOL:
                case TokenType.SYMBOLIC:
                case TokenType.SYNC:
                case TokenType.SYNCHRONIZED:
                case TokenType.TALLYING:
                case TokenType.TAPE:
                case TokenType.TEST:
                case TokenType.THAN:
                case TokenType.THEN:
                case TokenType.THROUGH:
                case TokenType.THRU:
                case TokenType.TIME:
                case TokenType.TIMES:
                case TokenType.TO:
                case TokenType.TOP:
                case TokenType.TRACE:
                case TokenType.TRAILING:
                case TokenType.TRUE:
                case TokenType.TYPE:
                case TokenType.UNBOUNDED:
                case TokenType.UNIT:
                case TokenType.UNTIL:
                case TokenType.UP:
                case TokenType.UPON:
                case TokenType.USAGE:
                case TokenType.USING:
                case TokenType.VALUE:
                case TokenType.VALUES:
                case TokenType.VARYING:
                case TokenType.WITH:
                case TokenType.WORDS:
                case TokenType.WRITE_ONLY:
                case TokenType.XML_SCHEMA:

                case TokenType.ALLOCATE : 
                case TokenType.CD:
                case TokenType.CF:
                case TokenType.CH:
                case TokenType.CLOCK_UNITS:
                case TokenType.COLUMN:
                case TokenType.COMMUNICATION:
                case TokenType.CONTROL:
                case TokenType.CONTROLS:
                case TokenType.DE:
                case TokenType.DEFAULT:
                case TokenType.DESTINATION:
                case TokenType.DETAIL:
                case TokenType.DISABLE:
                case TokenType.EGI:
                case TokenType.EMI:
                case TokenType.ENABLE:
                case TokenType.END_RECEIVE:
                case TokenType.ESI:
                case TokenType.FINAL:
                case TokenType.FREE:
                case TokenType.GROUP:
                case TokenType.HEADING:
                case TokenType.INDICATE:
                case TokenType.INITIATE:
                case TokenType.LAST:
                case TokenType.LIMIT:
                case TokenType.LIMITS:
                case TokenType.LINE_COUNTER:
                case TokenType.MESSAGE:
                case TokenType.NUMBER:
                case TokenType.PAGE_COUNTER:
                case TokenType.PF:
                case TokenType.PH:
                case TokenType.PLUS:
                case TokenType.PRINTING:
                case TokenType.PURGE:
                case TokenType.QUEUE:
                case TokenType.RD:
                case TokenType.RECEIVE:
                case TokenType.REPORT:
                case TokenType.REPORTING:
                case TokenType.REPORTS:
                case TokenType.RF:
                case TokenType.RH:
                case TokenType.SEGMENT:
                case TokenType.SEND:
                case TokenType.SOURCE:
                case TokenType.SUB_QUEUE_1:
                case TokenType.SUB_QUEUE_2:
                case TokenType.SUB_QUEUE_3:
                case TokenType.SUM:
                case TokenType.TABLE:
                case TokenType.TERMINAL:
                case TokenType.TERMINATE:
                case TokenType.TEXT:
                // Keywords - Cobol V6
                case TokenType.END_JSON:
                case TokenType.JSON:
                case TokenType.VOLATILE:
                // Keywords - Cobol 2002
                case TokenType.TYPEDEF:
                case TokenType.STRONG:
                // Keywords - TypeCobol
                case TokenType.UNSAFE:
                case TokenType.PUBLIC:
                case TokenType.PRIVATE:
                case TokenType.IN_OUT:
                case TokenType.STRICT:
                case TokenType.QUESTION_MARK:
                // FOR SQL
                case TokenType.SQL_COMMIT:
                    return true;
                default:
                    return false;
            }
        }
    }
}
