using System;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Substring of the source text corresponding to a character string or a separator.
    /// A character-string is a character or a sequence of contiguous characters that forms a
    /// COBOL word, a literal, a PICTURE character-string, or a comment-entry. 
    /// A separator is a string of contiguous characters used to delimit character strings.
    /// </summary>
    public class Token : Antlr4.Runtime.IToken, IVisitable
    {
        private ITokensLine tokensLine;
        private int startIndex;
        private int stopIndex;

		/// <summary>Empty constructor for mock.</summary>
		public Token() { }

        /// <summary>
        /// Constructor for tokens without delimiters
        /// </summary>
        public Token(TokenType tokenType, int startIndex, int stopIndex, ITokensLine tokensLine) :
            this(tokenType, startIndex, stopIndex, false, tokensLine)
        { }

        /// <summary>
        /// Constructor for tokens without delimiters, using the virtual space at the end of the line
        /// </summary>
        internal Token(TokenType tokenType, int startIndex, int stopIndex, bool usesVirtualSpaceAtEndOfLine, ITokensLine tokensLine)
        {
            TokenType = tokenType;
            TokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            HasError = false;
            SetInitialChannelFromTokenFamily();

            this.startIndex = startIndex;
            this.stopIndex = stopIndex;
            this.tokensLine = tokensLine;

            UsesDelimiters = false;
            HasClosingDelimiter = false;

            UsesVirtualSpaceAtEndOfLine = usesVirtualSpaceAtEndOfLine;
        }  
      
        /// <summary>
        /// Constructor for tokens with delimiters
        /// </summary>
        internal Token(TokenType tokenType, int startIndex, int stopIndex, ITokensLine tokensLine, bool hasOpeningDelimiter, bool hasClosingDelimiter, char expectedClosingDelimiter) :
            this(tokenType, startIndex, stopIndex, false, tokensLine, hasOpeningDelimiter, hasClosingDelimiter, expectedClosingDelimiter)
        { }

        /// <summary>
        /// Constructor for tokens with delimiters, using the virtual space at the end of the line
        /// </summary>
        internal Token(TokenType tokenType, int startIndex, int stopIndex, bool usesVirtualSpaceAtEndOfLine, ITokensLine tokensLine, bool hasOpeningDelimiter, bool hasClosingDelimiter, char expectedClosingDelimiter)
        {
            TokenType = tokenType;
            TokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            SetInitialChannelFromTokenFamily();

            HasError = false;

            this.startIndex = startIndex;
            this.stopIndex = stopIndex;
            this.tokensLine = tokensLine;

            UsesDelimiters = true;
            HasOpeningDelimiter = hasOpeningDelimiter;
            HasClosingDelimiter = hasClosingDelimiter;
            ExpectedClosingDelimiter = expectedClosingDelimiter;
            
            UsesVirtualSpaceAtEndOfLine = usesVirtualSpaceAtEndOfLine;
        }

        /// <summary>
        /// Type of the token as an int value (interface expected by the parser)
        /// </summary>
        public int Type
        {
            get { return (int)TokenType; }
        }

        /// <summary>
        /// Type from the TokenType enumeration
        /// </summary>
        public TokenType TokenType { get; internal set; }

        /// <summary>
        /// Family from the TokenFamily Enumeration
        /// </summary>
        public virtual TokenFamily TokenFamily { get; private set; }

        /// <summary>
        /// True if this token is pseudo text in a REPLACE or COPY REPLACING directive
        /// </summary>
        public bool IsPseudoText { get; set; }

        public static int CHANNEL_SourceTokens = 0;    
        public static int CHANNEL_WhitespaceAndComments = 1;
        public static int CHANNEL_ContinuationTokens = 2;
        public static int CHANNEL_CompilerDirectives = 3;
        public static int CHANNEL_InternalTokenGroups = 4;
        public static int CHANNEL_InvalidTokens = 5;

        /// <summary>
        /// Token readers can "tune" to different channels
        /// </summary>
        public int Channel { get; set; }

        private void SetInitialChannelFromTokenFamily()
        {
            switch(TokenFamily)
            {
                case TokenFamily.Whitespace:
                case TokenFamily.Comments:
                    Channel = CHANNEL_WhitespaceAndComments;
                    break;
                case TokenFamily.CompilerDirective:
                    Channel = CHANNEL_CompilerDirectives;
                    break;
                case TokenFamily.InternalTokenGroup:
                    Channel = CHANNEL_InternalTokenGroups;
                    break;
                case TokenFamily.Invalid:
                    Channel = CHANNEL_InvalidTokens;
                    break;
                default:
                    Channel = CHANNEL_SourceTokens;
                    break;
            }
        }

        /// <summary>
        /// True if an error diagnostic was associated to this token
        /// </summary>
        public bool HasError { get; set; }

        /// <summary>
        /// Underlying text line
        /// </summary>
        public ITokensLine TokensLine { get { return tokensLine; } }
        
        /// <summary>
        /// First character index of the token on the line
        /// In case of a token with delimiters (like an alphanumeric literal), the StartColumn is the column of the first delimiter char. 
        /// In case of a continuation token, only the current line is considered, not the absolute starting column on a previous line.
        /// </summary>
        public int StartIndex { get { return startIndex; } }
        
        /// <summary>
        /// Last character index of the token on the line
        /// In case of a token with delimiters (like an alphanumeric literal), the EndColumn is the column of the last delimiter char. 
        /// In case of a continued token, only the current line is considered, not the absolute ending column on a following line.
        /// </summary>
        public int StopIndex { get { return stopIndex; } }

        /// <summary>
        /// --- Necessary implementation of the interface Antlr4.Runtime.IToken ---
        /// WARNING, this property always returns the INITIAL line number, before any change was applied to the current document :
        /// => this.TokensLine.InitialLineIndex + 1 
        /// The CURRENT line index is only defined in the context of a snapshot of the source document :
        /// => ISearchableReandOnlyList.IndexOf(token.TokensLine, token.TokensLine.InitialLineIndex)
        /// </summary>
        public int Line { get { return this.TokensLine.InitialLineIndex + 1; } }

        /// <summary>
        /// Column number (starting count at 1) where the first character of the token was found in the source text.
        /// In case of a token with delimiters (like an alphanumeric literal), the StartColumn is the column of the first delimiter char. 
        /// In case of a continuation token, only the current line is considered, not the absolute starting column on a previous line.
        /// </summary>
        public int Column { get { return startIndex + 1; } }

        /// <summary>
        /// Column number (starting count at 1) where the last character of the token was found in the source text.
        /// In case of a token with delimiters (like an alphanumeric literal), the EndColumn is the column of the last delimiter char. 
        /// In case of a continued token, only the current line is considered, not the absolute ending column on a following line.
        /// </summary>
        public int EndColumn { get { return stopIndex + 1; } }

        /// <summary>
        /// True if the the virtual space at the end of the line was consumed to establish the type of the token.
        /// Language Reference p54: If there is no hyphen (-) in the indicator area (column 7) of a line, the last character
        /// of the preceding line is assumed to be followed by a space.
        /// </summary>
        public bool UsesVirtualSpaceAtEndOfLine { get; private set; }

        /// <summary>
        /// Length of the token in the source text.
        /// In case of a token with delimiters (like an alphanumeric literal), the length counts both the opening and closing delimiter chars. 
        /// The length does not include the virtual space at the end of the line event when it is used  to compute the type of the token.
        /// In case of a continued token, only the current line is considered.
        /// </summary>
        public int Length
        {
            get
            {
                return stopIndex - startIndex + 1;
            }
        }

        /// <summary>
        /// Returns the substring of raw source text comprised between the starting and ending column of the token.
        /// </summary>
        public string SourceText
        {
            get
            {
                return tokensLine.TextSegment(startIndex, stopIndex);
            }
        }

        /// <summary>
        /// Text returned to the parser :
        /// - SourceText if the token is not continued on the next line
        /// - MultilineContinuationText if the token IsContinuationToken
        /// </summary>
        public virtual string Text
        {
            get
            {
                return SourceText;
            }
        }

        // --- Literals with or without delimiters ---

        /// <summary>
        /// True if the start and the end of this token are marked by dedicated delimiter characters.
        /// For example : " and ' are delimiters for alphanumeric literals.
        /// The StartColumn, EndColumn, Length and SourceText properties include the delimiters, but the ProcessedText property excludes them.
        /// </summary>
        public bool UsesDelimiters { get; protected set; }

        /// <summary>
        /// True if the token type uses delimiter characters, and if the expected opening delimiter characters were found
        /// in the current line and are part of this token. 
        /// If false, the token must be a continuation of a previous token which contains the opening delimiter characters.
        /// </summary>
        public bool HasOpeningDelimiter { get; protected set; }

        /// <summary>
        /// True if the token type uses delimiter characters, and if the expected closing delimiter characters were found
        /// in the current line and are part of this token. 
        /// If false, the token is considered invalid until a continuation token is found on the next line.
        /// </summary>
        public bool HasClosingDelimiter { get; protected set; }

        /// <summary>
        /// In case of alphanumeric literals, the closing delimiter : " or ' must match the opening delimiter.
        /// </summary>
        public char ExpectedClosingDelimiter { get; protected set; }
        
        /// <summary>
        /// Enables the lexer to attach a strongly typed value for literals
        /// </summary>
        public LiteralTokenValue LiteralValue { get; set; }

        // --- Ambiguous tokens resolved after having been created ---
        
        internal void CorrectType(TokenType tokenType)
        {
            // Copy token type and family from the continuation token
            TokenType = tokenType;
            TokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);

            // If it is the first continued token on the top of a list of continuation lines
            // => set its channel according to the global token type
            if (Channel != CHANNEL_ContinuationTokens)
            {
                SetInitialChannelFromTokenFamily();
            }
            // If it is a continued ContinuationToken, a token in the middle of a list of continuation lines
            // => leave its channel to the original value CHANNEL_ContinuationTokens 
            //    because it should be filtered at the parser stage
        }

        internal void DegradePotentialCodeElementStartingKeywordToSyntaxKeyword()
        {
            if (TokenFamily == TokenFamily.CodeElementStartingKeyword)
            {
                TokenFamily = TokenFamily.SyntaxKeyword;
            }
        }

        // --- Continuation lines & multiline tokens ---

        /// <summary>
        /// True if this token participates in a multiline continuation
        /// </summary>
        public virtual bool IsContinuationToken { get { return false; } }

        internal void CorrectTokensLine(ITokensLine tokensLine, int startIndex, int stopIndex)
        {
            this.tokensLine = tokensLine;
            this.startIndex = startIndex;
            this.stopIndex = stopIndex;
        }

        // --- Debugging

        /// <summary>
        /// Text representation of a token for debugging or test purposes
        /// </summary>
        public override string ToString()
        {
            string tokenText = "[" + Column + "," + EndColumn + (UsesVirtualSpaceAtEndOfLine ? "+" : "") + ":" + Text + "]<" + TokenType.ToString() + ">";
            if(UsesDelimiters)
            {
                tokenText += "(" + ExpectedClosingDelimiter + "," + (HasOpeningDelimiter ? "Y" : "N") + "," + (HasClosingDelimiter ? "Y" : "N") + ")";
            }
            if(TokenFamily == TokenFamily.AlphanumericLiteral || TokenFamily == TokenFamily.NumericLiteral)
            {
                tokenText += "{" + (LiteralValue == null ? "NULL" : LiteralValue.ToString()) + "}";
            }
            return tokenText;
        }

        public virtual bool AcceptASTVisitor(IASTVisitor astVisitor) {
            return astVisitor.Visit(this);
        }

        // --- Methods from Antlr IToken, must be set for error handling in Antlr parser ---

        internal void SetAntlrSource(Antlr4.Runtime.ITokenSource source)
        {
            tokenSource = source;
        }

        private Antlr4.Runtime.ITokenSource tokenSource;

        /// <summary>
        /// Returns null until SetAntlrTokenSource() is called
        /// </summary>
        public Antlr4.Runtime.ITokenSource TokenSource
        {
            get { return tokenSource; }
        }

        /// <summary>
        /// Returns a AntlrUtils.TextLineCharStream
        /// </summary>
        public Antlr4.Runtime.ICharStream InputStream
        {
            get { return new AntlrUtils.TextLineCharStream(tokensLine); }
        }

        /// <summary>
        /// Alway returns -1
        /// </summary>
        public int TokenIndex
        {
            get { return -1; }
        }

        // Common token for End of file
        public static Token END_OF_FILE = new Token(TokenType.EndOfFile, 0, -1, TypeCobol.Compiler.Scanner.TokensLine.CreateVirtualLineForInsertedToken(-1, String.Empty));

        // --- Token comparison for REPLACE directive ---

        /// <summary>
        /// Compare two tokens to implement the REPLACE directive
        /// </summary>
        public bool CompareForReplace(Token comparisonToken)
        {
             // 1. First compare the token type                 
            if(this.TokenType != comparisonToken.TokenType)
            {
                return false;
            }
            // 2. For partial Cobol words, chech if the comparison token text (":TAG:") 
            //    is contained in the current token text (":TAG:-AMOUNT")
            else if(TokenType == TokenType.PartialCobolWord)
            {
                return Text.IndexOf(comparisonToken.Text, StringComparison.OrdinalIgnoreCase) >= 0;
            }
            // 3. For token families
            //    - AlphanumericLiteral
            //    - NumericLiteral
            //    - SyntaxLiteral
            //    - Symbol
            //    => compare Token text
            else if (TokenFamily == TokenFamily.AlphanumericLiteral || TokenFamily == TokenFamily.NumericLiteral ||
                     TokenFamily == TokenFamily.Symbol || TokenFamily == TokenFamily.SyntaxLiteral)
            {
                return Text.Equals(comparisonToken.Text, StringComparison.OrdinalIgnoreCase);
            }
            // 4. In all other cases, token type comparison was enough
            {
                return true;
            }
        }
    }
}
