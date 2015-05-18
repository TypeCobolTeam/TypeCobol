using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Substring of the source text corresponding to a character string or a separator.
    /// A character-string is a character or a sequence of contiguous characters that forms a
    /// COBOL word, a literal, a PICTURE character-string, or a comment-entry. 
    /// A separator is a string of contiguous characters used to delimit character strings.
    /// </summary>
    public class Token : Antlr4.Runtime.IToken
    {
        private ITextLine textLine;
        private int startIndex;
        private int stopIndex;

        /// <summary>
        /// Constructor for tokens without delimiters
        /// </summary>
        internal Token(TokenType tokenType, int startIndex, int stopIndex, ITextLine textLine) :
            this(tokenType, startIndex, stopIndex, false, textLine)
        { }

        /// <summary>
        /// Constructor for tokens without delimiters, using the virtual space at the end of the line
        /// </summary>
        internal Token(TokenType tokenType, int startIndex, int stopIndex, bool usesVirtualSpaceAtEndOfLine, ITextLine textLine)
        {
            TokenType = tokenType;
            TokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            HasError = false;
            SetInitialChannelFromTokenFamily();

            this.startIndex = startIndex;
            this.stopIndex = stopIndex;
            this.textLine = textLine;

            UsesDelimiters = false;
            HasClosingDelimiter = false;

            UsesVirtualSpaceAtEndOfLine = usesVirtualSpaceAtEndOfLine;
        }  
      
        /// <summary>
        /// Constructor for tokens with delimiters
        /// </summary>
        internal Token(TokenType tokenType, int startIndex, int stopIndex, ITextLine textLine, bool hasOpeningDelimiter, bool hasClosingDelimiter, char expectedClosingDelimiter) :
            this(tokenType, startIndex, stopIndex, false, textLine, hasOpeningDelimiter, hasClosingDelimiter, expectedClosingDelimiter)
        { }

        /// <summary>
        /// Constructor for tokens with delimiters, using the virtual space at the end of the line
        /// </summary>
        internal Token(TokenType tokenType, int startIndex, int stopIndex, bool usesVirtualSpaceAtEndOfLine, ITextLine textLine, bool hasOpeningDelimiter, bool hasClosingDelimiter, char expectedClosingDelimiter)
        {
            TokenType = tokenType;
            TokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            SetInitialChannelFromTokenFamily();

            HasError = false;

            this.startIndex = startIndex;
            this.stopIndex = stopIndex;
            this.textLine = textLine;

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
        public TokenType TokenType { get; private set; }

        /// <summary>
        /// Family from the TokenFamily Enumeration
        /// </summary>
        public TokenFamily TokenFamily { get; private set; }

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
        /// Internal access to the underlying text line
        /// </summary>
        internal ITextLine TextLine { get { return textLine; } }

        /// <summary>
        /// The line index (starting at 0) on which the 1st character of this token was matched.
        /// </summary>
        public int LineIndex { get { return textLine.LineIndex; } }

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
        /// The line number (starting count at 1) on which the 1st character of this token was matched.
        /// </summary>
        public int Line { get { return textLine.LineIndex + 1; } }

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
        public virtual string Text
        {
            get
            {
                return textLine.TextSegment(startIndex, stopIndex);
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
        public LiteralValue LiteralValue { get; set; }

        // --- Ambiguous tokens resolved after having been created ---
        
        internal void CorrectType(TokenType tokenType)
        {
            TokenType = tokenType;
            TokenFamily = TokenUtils.GetTokenFamilyFromTokenType(tokenType);
            SetInitialChannelFromTokenFamily();
        }

        // --- Continuation lines & multiline tokens ---

        internal void LinkToContinuationToken(ContinuationToken continuationToken)
        {
            // Register continuation token
            IsContinuedOnTheNextLine = true;
            Channel = CHANNEL_ContinuationTokens;
            ContinuationToken = continuationToken;

            // Recursively inherit the token type from the continuation token
            SetPropertiesFromContinuationToken();
        }

        /// <summary>
        /// Recursively set the type of the continued tokens to the same type as the continuation token
        /// </summary>
        internal virtual void SetPropertiesFromContinuationToken()
        {
            TokenType = ContinuationToken.TokenType;
            TokenFamily = ContinuationToken.TokenFamily;            
        }

        /// <summary>
        /// True if a token found on the next line continues the current token 
        /// </summary>
        public bool IsContinuedOnTheNextLine { get; private set; }

        /// <summary>
        /// Token found on the next line which continues the current token 
        /// </summary>
        public ContinuationToken ContinuationToken { get; private set; }

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
            if(IsContinuedOnTheNextLine)
            {
                tokenText = "=>continued:" + tokenText;
            }
            return tokenText;
        }

        // --- Methods from Antlr IToken, must be set for error handling in Antlr parser ---

        internal void SetAntlrSource(Tuple<Antlr4.Runtime.ITokenSource, Antlr4.Runtime.ICharStream> source)
        {
            tokenSource = source.Item1;
            inputStream = source.Item2;
        }

        private Antlr4.Runtime.ITokenSource tokenSource;

        /// <summary>
        /// Always returns null
        /// </summary>
        public Antlr4.Runtime.ITokenSource TokenSource
        {
            get { return tokenSource; }
        }

        private Antlr4.Runtime.ICharStream inputStream;

        /// <summary>
        /// Always returns null
        /// </summary>
        public Antlr4.Runtime.ICharStream InputStream
        {
            get { return inputStream; }
        }

        /// <summary>
        /// Alway returns -1
        /// </summary>
        public int TokenIndex
        {
            get { return -1; }
        }

        // Common token for End of file
        public static Token END_OF_FILE = new Token(TokenType.EndOfFile, 0, -1, new TextLine(-1, 0, String.Empty));

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
