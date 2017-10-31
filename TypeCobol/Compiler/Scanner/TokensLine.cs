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
    public class TokensLine : CobolTextLine, ITokensLine
    {
        public TokensLine(ITextLine textLine, ColumnsLayout columnsLayout) : base(textLine, columnsLayout)
        {
            lastSourceIndex = Source.EndIndex;
            SourceTokens = new List<Token>();
            _ScannerDiagnostics = new List<Diagnostic>();
        }

        internal void InitializeScanState(MultilineScanState initialScanState)
        {
            InitialScanState = initialScanState;
            ScanState = initialScanState != null ?  initialScanState.Clone(): null;
        }

        /// <summary>
        /// Factory method used by the parser when it inserts a missing token
        /// in the tokens stream to recover from the error and continue
        /// </summary>
        internal static TokensLine CreateVirtualLineForInsertedToken(int lineIndex, string text)
        {
            return new TokensLine(
                new TextLineSnapshot(lineIndex, text, null),
                ColumnsLayout.FreeTextFormat);
        }
        
        // Cache last index of a source char on this line
        private int lastSourceIndex;

        /// <summary>
        /// Tokens found while scanning the raw source text line
        /// (before the text manipulation phase)
        /// </summary>
        public IList<Token> SourceTokens { get; private set; }

        /// <summary>
        /// True if the first token on the next line continues the last token of this line
        /// </summary>
        public bool HasTokenContinuedOnNextLine { get; private set; }

        /// <summary>
        /// True if the first token on this line continues the last token of the previous line
        /// </summary>
        public bool HasTokenContinuationFromPreviousLine { get; private set; }

        /// <summary>
        /// Always use this method add new token to this line (never call directly Tokens.Add)
        /// </summary>
        internal void AddToken(Token token)
        {            
            // Identify pseudo-text tokens : could be necessary to filter them from "real" source text tokens
            if(ScanState.InsidePseudoText && token.TokenType != TokenType.PseudoTextDelimiter)
            {
                token.IsPseudoText = true;
            }

            // Register new token in list
            SourceTokens.Add(token);

            // Register scan state before COPY and EXECL SQL INCLUDE tokens 
            // (necessary to scan properly the imported document)
            if (token.TokenType == TokenType.COPY || token.TokenType == TokenType.EXEC)
            {
                if(ScanStateBeforeCOPYToken == null)
                {
                    ScanStateBeforeCOPYToken = new Dictionary<Token,MultilineScanState>();
                }
                ScanStateBeforeCOPYToken[token] = ScanState.Clone();
            }

            // Advance MultilineScanState
            if (Type != CobolTextLineType.Blank) // see p54 : for continuation, blank lines are treated like comment lines
            {
                ScanState.AdvanceToNextStateAndAdjustTokenProperties(token);
            }

            // Register multiline continuation tokens
            if (token.IsContinuationToken)
            {
                ContinuationToken continuationToken = (ContinuationToken)token;
                HasTokenContinuationFromPreviousLine = HasTokenContinuationFromPreviousLine || continuationToken.IsContinuationFromPreviousLine;
                HasTokenContinuedOnNextLine = HasTokenContinuedOnNextLine || continuationToken.IsContinuedOnNextLine;
            }
        }

        /// <summary>
        /// Error and warning messages produced while scanning the raw source text line
        /// (before text manipulation phase)
        /// </summary>
        public IList<Diagnostic> ScannerDiagnostics
        {
            get { return _ScannerDiagnostics; }
        }

        private IList<Diagnostic> _ScannerDiagnostics;

        /// <summary>
        /// Use this method to attach a diagnostic to this line 
        /// (never call directly Diagnostics.Add)
        /// </summary>
        internal void AddDiagnostic(MessageCode messageCode, int columnStart, int columnEnd, params object[] messageArgs)
        {
            Diagnostic diag = new Diagnostic(messageCode, columnStart, columnEnd, this.LineIndex, messageArgs);
            _ScannerDiagnostics.Add(diag);
        }

        /// <summary>
        /// Use this method to attach a diagnostic for a specific token to this line 
        /// (never call directly Diagnostics.Add)
        /// </summary>
        internal void AddDiagnostic(MessageCode messageCode, Token token, params object[] messageArgs)
        {
            Diagnostic diag = new TokenDiagnostic(messageCode, token, this.LineIndex, messageArgs);
            if(diag.Info.Severity == Severity.Error)
            {
                token.HasError = true;
            }
            _ScannerDiagnostics.Add(diag);
        }     

        /// <summary>
        /// Filters only the diagnostics attached to a specific token of this line
        /// </summary>
        internal IEnumerable<Diagnostic> GetDiagnosticsForToken(Token filterToken)
        {
            return _ScannerDiagnostics.Where(diag => diag is TokenDiagnostic && ((TokenDiagnostic)diag).Token == filterToken );
        }



        /// <summary>
        /// In case a continuation of the last token of this line is discovered on the next line,
        /// remove all diagnostics specifically attached to this incomplete token
        /// </summary>
        internal void RemoveDiagnosticsForLastSourceToken()
        {
            Token lastToken = SourceTokens.Last();
            RemoveDiagnosticsForToken(lastToken);
        }

        /// <summary>
        /// Remove all diagnostics already registered for a given token 
        /// </summary>
        internal void RemoveDiagnosticsForToken(Token token)
        {
            if (token != null)
            {
                foreach (Diagnostic diag in GetDiagnosticsForToken(token).ToArray()) //ToArray in order to remove reference
                {
                    _ScannerDiagnostics.Remove(diag);
                }
            }
        }

        // --- State for context-sensitive tokens ---

        /// <summary>
        /// Internal state that was used to start scanning this line
        /// (we need to remember this to avoid a full rescan when previous lines are update in a compatible way)
        /// </summary>
        internal MultilineScanState InitialScanState { get; private set; }

        /// <summary>
        /// The preprocessor needs to know the exact ScanState just before each COPY token is encountered
        /// </summary>
        internal IDictionary<Token,MultilineScanState> ScanStateBeforeCOPYToken { get; private set; }

        /// <summary>
        /// Internal state used by the Scanner to disambiguate context-sensitive keywords
        /// </summary>
        public MultilineScanState ScanState { get; private set; }
       
        // --- Incremental compilation process ---

        protected void CopyTokensLineProperties(TokensLine previousLineVersion)
        {
            this.lastSourceIndex = previousLineVersion.lastSourceIndex;
            this.InitialScanState = previousLineVersion.InitialScanState;
            this.ScanStateBeforeCOPYToken = previousLineVersion.ScanStateBeforeCOPYToken;
            this.ScanState = previousLineVersion.ScanState;
            this.SourceTokens = previousLineVersion.SourceTokens;
            this._ScannerDiagnostics = previousLineVersion.ScannerDiagnostics;
            this.HasTokenContinuationFromPreviousLine = previousLineVersion.HasTokenContinuationFromPreviousLine;
            this.HasTokenContinuedOnNextLine = previousLineVersion.HasTokenContinuedOnNextLine;

            CompilationStep = Concurrency.CompilationStep.Scanner;
        }
    }
}
