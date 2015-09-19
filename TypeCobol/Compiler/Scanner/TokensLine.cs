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
    public class TokensLine : ITokensLine
    {
        /// <summary>
        /// Constructor used for the first tokens line
        /// </summary>
        internal TokensLine(ICobolTextLine textLine, MultilineScanState initialScanState)
        {
            this.textLine = textLine;
            lastSourceIndex = textLine.Source.EndIndex;

            SourceTokens = new List<Token>();
            ScannerDiagnostics = new List<Diagnostic>();
            InitialScanState = initialScanState;
            ScanState = initialScanState.Clone();
        }

        /// <summary>
        /// Constructor used to link a previous tokens line to the following tokens line
        /// </summary>
        internal TokensLine(CobolTextLine textLine, TokensLine previousLine)
        {
            this.textLine = textLine;
            lastSourceIndex = textLine.Source.EndIndex;

            SourceTokens = new List<Token>();
            ScannerDiagnostics = new List<Diagnostic>();
            PreviousLine = previousLine;
            InitialScanState = previousLine.ScanState.Clone();
            ScanState = previousLine.ScanState.Clone();
        }

        /// <summary>
        /// Constructor used to compute a continuation between two lines
        /// </summary>
        internal TokensLine(CobolTextLine textLine, TokensLine previousLine, bool revertToPreviousState)
        {
            this.textLine = textLine;
            lastSourceIndex = textLine.Source.EndIndex;

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
        /// Factory method used by the parser when it inserts a missing token
        /// in the tokens stream to recover from the error and continue
        /// </summary>
        internal static ITokensLine CreateVirtualLineForInsertedToken(int initialLineIndex, string text)
        {
            return new TokensLine(
                new CobolTextLine(
                    new TextLineSnapshot(initialLineIndex, text, null),
                    ColumnsLayout.FreeTextFormat),
                new MultilineScanState(false, false, false, Encoding.Unicode));
        }


        // Cache last index of a source char on this line
        private int lastSourceIndex;

        /// <summary>
        /// Tokens found while scanning the raw source text line
        /// (before the text manipulation phase)
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
            if (textLine.Type != CobolTextLineType.Blank) // see p54 : for continuation, blank lines are treated like comment lines
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
            RemoveDiagnosticsForToken(lastToken);
        }

        /// <summary>
        /// Remove all diagnostics already registered for a given token 
        /// </summary>
        internal void RemoveDiagnosticsForToken(Token token)
        {
            if (token != null)
            {
                foreach (Diagnostic diag in GetDiagnosticsForToken(token).ToArray())
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

        // --- ICobolTextLine wrapper ---

        // Underlying Cobol text line
        private ICobolTextLine textLine;

        /// <summary>
        /// Reference to the underlying Cobol text line, reusable in several TokensLines when the context changes
        /// </summary>
        public ICobolTextLine TextLine { get { return textLine; } }

        /// <summary>
        /// Text of the line, without the end of line delimiters
        /// </summary>
        public string Text { get { return textLine.Text; } }

        /// <summary>
        /// Part of the text of the line, from start index to end index (included)
        /// </summary>
        public string TextSegment(int startIndex, int endIndexInclusive) { return textLine.TextSegment(startIndex, endIndexInclusive); }

        /// <summary>
        /// Number of characters in the line, end of line delimiters excluded
        /// </summary>
        public int Length { get { return textLine.Length; } }

        // Position of the text line in the source text document

        /// <summary>
        /// Index of this line when it first appeared in the document.
        /// WARNING : if lines are later inserted or removed in the document before it,
        /// InitialLineIndex no longer reflects the current position of the line.
        /// It can however provide a good starting point to start searching for a line
        /// in a snapshot of the document at a given point in time.
        /// When a line is created outside of a document, InitialLineIndex = -1.
        /// </summary>
        public int InitialLineIndex { get { return textLine.InitialLineIndex; } }

        /// <summary>
        /// A text line instance can be reused simultaneously in different snapshots of the document
        /// (if it wasn't modified between two versions).
        /// You can NOT get a line number from an isolated text line, because this line instance can
        /// have different positions in two different snapshots of the document (if other lines were 
        /// inserted or removed before).
        /// 
        /// The line number is only defined :
        /// 
        /// 1. In a specific snapshot of the document :
        /// - pass the ITextLine object to the IndexOf method on the list of lines in a document snapshot 
        ///   (WARNING : expensive O(n) operation !)
        /// 
        /// 2. In the live text document (for example a text editor accessed in the specific thread where it lives) :
        /// - pass the property LineTrackingReferenceInSourceDocument to a dedicated method of the text source 
        ///   (much less expensive O(log n) operation)
        /// 
        /// This property returns an opaque reference to a line tracking object from the live text document,
        /// which will enable an efficient retrieval of the line number for this line in the document.
        /// </summary>
        public object LineTrackingReferenceInSourceDocument { get { return textLine.LineTrackingReferenceInSourceDocument; } }

        /// <summary>
        /// Cobol text line type : Source, Debug, Comment or Continuation
        /// </summary>
        public CobolTextLineType Type { get { return textLine.Type; } }

        /// <summary>
        /// Sequence number area : Columns 1 through 6
        /// </summary>
        public TextArea SequenceNumber { get { return textLine.SequenceNumber; } }

        /// <summary>
        /// Sequence number text : Columns 1 through 6
        /// </summary>
        public string SequenceNumberText { get { return textLine.SequenceNumberText; } }

        /// <summary>
        /// Indicator area : Column 7
        /// </summary>
        public TextArea Indicator { get { return textLine.Indicator; } }

        /// <summary>
        /// Indicator char : Column 7
        /// </summary>
        public char IndicatorChar { get { return textLine.IndicatorChar; } }

        /// <summary>
        /// Area A : Columns 8 through 11 
        /// Area B : Columns 12 through 72 
        /// </summary>
        public TextArea Source { get { return textLine.Source; } }

        /// <summary>
        /// Area A text : Columns 8 through 11 
        /// Area B text : Columns 12 through 72 
        /// </summary>
        public string SourceText { get { return textLine.SourceText; } }

        /// <summary>
        /// Comment area : Columns 73 through 80+
        /// </summary>
        public TextArea Comment { get { return textLine.Comment; } }

        /// <summary>
        /// Comment text : Columns 73 through 80+
        /// </summary>
        public string CommentText { get { return textLine.CommentText; } }        
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
