using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// List of tokens and diagnostics found by scanning one line of text
    /// </summary>
    public class TokensLine
    {
        /// <summary>
        /// Constructor used for the first tokens line
        /// </summary>
        internal TokensLine(TextLineMap textLineMap, bool insideDataDivision, bool decimalPointIsComma, Encoding encodingForHexadecimalAlphanumericLiterals)
        {
            TextLineMap = textLineMap;
            lastSourceIndex = textLineMap.Source.EndIndex;

            SourceTokens = new List<Token>();
            ScannerDiagnostics = new List<Diagnostic>();
            InitialScanState = new MultilineScanState(insideDataDivision, decimalPointIsComma, encodingForHexadecimalAlphanumericLiterals);
            ScanState = InitialScanState.Clone();
        }

        /// <summary>
        /// Constructor used to link a previous tokens line to the following tokens line
        /// </summary>
        internal TokensLine(TextLineMap textLineMap, TokensLine previousLine)
        {
            TextLineMap = textLineMap;
            lastSourceIndex = textLineMap.Source.EndIndex;

            SourceTokens = new List<Token>();
            ScannerDiagnostics = new List<Diagnostic>();
            PreviousLine = previousLine;
            InitialScanState = previousLine.ScanState.Clone();
            ScanState = previousLine.ScanState.Clone();
        }

        /// <summary>
        /// Constructor used to compute a continuation between two lines
        /// </summary>
        internal TokensLine(TextLineMap textLineMap, TokensLine previousLine, bool revertToPreviousState)
        {
            TextLineMap = textLineMap;
            lastSourceIndex = textLineMap.Source.EndIndex;

            SourceTokens = new List<Token>();
            ScannerDiagnostics = new List<Diagnostic>();
            PreviousLine = previousLine;
            if (revertToPreviousState)
            {
                InitialScanState = previousLine.ScanStateBeforeLastTokenOfLine;
                ScanState = previousLine.ScanStateBeforeLastTokenOfLine;
            }
            else
            {
                InitialScanState = previousLine.ScanState.Clone();
                ScanState = previousLine.ScanState.Clone();
            }
        }
        
        /// <summary>
        /// Source text line
        /// </summary>
        public TextLineMap TextLineMap { get; private set; }

        // Cache last index of a source char on this line
        private int lastSourceIndex;

        /// <summary>
        /// Tokens found while scanning the raw source text line
        /// (before text manipulation phase)
        /// </summary>
        public IList<Token> SourceTokens { get; private set; }

        /// <summary>
        /// Always use this method add new token to this line (never call directly Tokens.Add)
        /// </summary>
        internal void AddToken(Token token)
        {
            // Check if this is the last token of the line ?
            if(token.StopIndex == lastSourceIndex)
            {
                // If it is, save the scan state for the previous
                // token, because we may need to revert to it in case 
                // of continuation on the next line
                if(SourceTokens.Count > 0)
                {
                    ScanStateBeforeLastTokenOfLine = ScanState.Clone();
                }
                else
                {
                    ScanStateBeforeLastTokenOfLine = InitialScanState;
                }
            }

            // Identify pseudo-text tokens : could be necessary to filter them from "real" source text tokens
            if(ScanState.KeywordsState == KeywordsSequenceState.InsidePseudoText && token.TokenType != TokenType.PseudoTextDelimiter)
            {
                token.IsPseudoText = true;
            }

            // Register new token in list
            SourceTokens.Add(token);

            // Advance MultilineScanState
            if (TextLineMap.Type != TextLineType.Blank) // see p54 : for continuation, blank lines are treated like comment lines
            {
                ScanState.AdvanceToNextState(token);
            }
        }
        
        /// <summary>
        /// Error and warning messages produced while scanning the raw source text line
        /// (before text manipulation phase)
        /// </summary>
        public IList<Diagnostic> ScannerDiagnostics { get; private set; }

        /// <summary>
        /// Use this method to attach a diagnostic to this line 
        /// (never call directly Diagnostics.Add)
        /// </summary>
        internal void AddDiagnostic(MessageCode messageCode, int columnStart, int columnEnd, params object[] messageArgs)
        {
            Diagnostic diag = new Diagnostic(messageCode, columnStart, columnEnd, messageArgs);
            ScannerDiagnostics.Add(diag);
        }

        /// <summary>
        /// Use this method to attach a diagnostic for a specific token to this line 
        /// (never call directly Diagnostics.Add)
        /// </summary>
        internal void AddDiagnostic(MessageCode messageCode, Token token, params object[] messageArgs)
        {
            Diagnostic diag = new TokenDiagnostic(messageCode, token, messageArgs);
            if(diag.Info.Severity == Severity.Error)
            {
                token.HasError = true;
            }
            ScannerDiagnostics.Add(diag);
        }     

        /// <summary>
        /// Filters only the diagnostics attached to a specific token of this line
        /// </summary>
        internal IEnumerable<Diagnostic> GetDiagnosticsForToken(Token filterToken)
        {
            return ScannerDiagnostics.Where(diag => diag is TokenDiagnostic ? ((TokenDiagnostic)diag).Token == filterToken : false );
        }

        /// <summary>
        /// In case a continuation of the last token of this line is discovered on the next line,
        /// remove all diagnostics specifically attached to this incomplete token
        /// </summary>
        internal void RemoveDiagnosticsForLastSourceToken()
        {
            Token lastToken = SourceTokens.Last();
            if(lastToken != null)
            {
                foreach(Diagnostic diag in GetDiagnosticsForToken(lastToken).ToArray())
                {
                    ScannerDiagnostics.Remove(diag);
                }
            }
        }

        // --- State for context-sensitive tokens ---

        /// <summary>
        /// We need to reach the previous line when a continuation is discovered on the current line
        /// </summary>
        internal TokensLine PreviousLine { get; private set; }

        /// <summary>
        /// True if we used the last token of the previous line to build this line tokens
        /// </summary>
        internal bool UsedLastTokenOfPreviousLine { get; set; }

        /// <summary>
        /// Internal state that was used to start scanning this line
        /// (we need to remember this to avoid a full rescan when previous lines are update in a compatible way)
        /// </summary>
        internal MultilineScanState InitialScanState { get; private set; }

        /// <summary>
        /// Because the last token of this line could be continued on the next line,
        /// we need to remember the previous scan state computed just before the last token,
        /// as we may need to revert to it when the continuation is encountered on the next line
        /// </summary>
        internal MultilineScanState ScanStateBeforeLastTokenOfLine { get; private set; }

        /// <summary>
        /// Internal state used by the Scanner to disambiguate context-sensitive keywords
        /// </summary>
        internal MultilineScanState ScanState { get; private set; }

        /// <summary>
        /// Call this method when a continuation is discovered on the current line
        /// </summary>
        internal void AdjustScanStatesForContinuedAndContinuationLines()
        {
            if(PreviousLine != null)
            {
                // Cancel the effect of the last token on the state of the previous line
                PreviousLine.ScanState = PreviousLine.ScanStateBeforeLastTokenOfLine;
                // Remove all diagnostics associated with the last token on the previous line
                PreviousLine.RemoveDiagnosticsForLastSourceToken();
                
                // Adjust the initial state of the current line accordingly
                InitialScanState = PreviousLine.ScanState.Clone();
                ScanState = PreviousLine.ScanState.Clone();

                // Replay the state transitions for the tokens already added to this line
                foreach(Token token in SourceTokens)
                {
                    ScanState.AdvanceToNextState(token);
                }

                // Register that we used the last token of the previous line 
                // to compute the tokens of the current line
                UsedLastTokenOfPreviousLine = true; 
            }
        }


    }    

    /// <summary>
    /// Used to register a diagnostic specifically attached to a Token
    /// </summary>
    public class TokenDiagnostic : Diagnostic
    {
        internal TokenDiagnostic(MessageCode messageCode, Token token, params object[] messageArgs) :
            base(messageCode, token.Column, token.EndColumn, messageArgs)
        {
            Token = token;
        }

        /// <summary>
        /// Token which is the subject of the diagnostics
        /// </summary>
        public Token Token { get; private set; }
    }
}
