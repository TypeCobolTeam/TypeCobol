using System;
using System.Linq;
using System.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Internal Scanner state propagated from one line to the other when compiling a complete source file
    /// </summary>
    public partial class MultilineScanState : IEquatable<MultilineScanState>
    {
        /// <summary>
        /// Last keyword or symbol token encountered in the text file
        /// </summary>
        public Token LastSignificantToken { get; set; }

        /// <summary>
        /// Last keyword or symbol token encountered in the text file
        /// </summary>
        public Token BeforeLastSignificantToken { get; set; }

        /// <summary>
        /// True if we know from the keyword stream that we are inside a DATA DIVISION.
        /// Used by the Scanner to disambiguate similar keywords based on their context of appearance. 
        /// </summary>
        public bool InsideDataDivision { get; private set; }

        /// <summary>
        /// True if we know from the keyword stream that we are inside a PROCEDURE DIVISION.
        /// Used by the Scanner to disambiguate similar keywords based on their context of appearance. 
        /// </summary>
        public bool InsideProcedureDivision { get; private set; }

        /// <summary>
        /// True if we are between two PseudoTextDelimiters : tokens are in fact pseudo text
        /// </summary>
        public bool InsidePseudoText { get; private set; }

        /// <summary>
        /// Special names defined for this scan state
        /// </summary>
        public SpecialNamesContext SpecialNames { get; }

        /// <summary>
        /// True if we are between two formalizedComments markups
        /// </summary>
        public bool InsideFormalizedComment { get; private set; }

        /// <summary>
        /// True if we are inside the Params Field of a Formalized Comment
        /// </summary>
        public bool InsideParamsField { get; private set; }

        /// <summary>
        /// True if we are between two MultilineComments markups
        /// </summary>
        public bool InsideMultilineComments { get; private set; }

        /// <summary>
        /// True as soon as the keywords DEBUGGING MODE have been encountered
        /// </summary>
        public bool WithDebuggingMode { get; private set; }

        /// <summary>
        /// True if we are scanning inside a Copy.
        /// </summary>
        public bool InsideCopy
        {
            get;
            internal set; //Setter is used only at import time when we jump from including document to included copy.
        }

        /// <summary>
        /// Encoding of the text file : used to decode the value of an hexadecimal alphanumeric literal
        /// </summary>
        public Encoding EncodingForAlphanumericLiterals { get; }

        // Used to track the end of a REPLACE directive.
        private bool _afterReplacementPseudoText;

        /// <summary>
        /// True if we are scanning inside a REPLACE directive.
        /// </summary>
        public bool InsideReplaceDirective { get; private set; }

#if EUROINFO_RULES
        /// <summary>
        /// True when we are existing a remarks directive. 
        /// </summary>
        public bool LeavingRemarksDirective { get; set; }
        /// <summary>
        /// True if we detect in the comments lines stream that we are inside a REMARKS compiler directive.
        /// </summary>
        public bool InsideRemarksDirective { get; set; }

        /// <summary>
        /// True if we are inside a COPY=(..) of a REMARKS compiler directive.
        /// </summary>
        public bool InsideRemarksParentheses { get; set; }
#endif

        /// <summary>
        /// True if we are inside a portion of SQL code introduced by EXEC SQL ... and ended by END-EXEC.
        /// </summary>
        public bool InsideSql { get; set; }

        /// <summary>
        /// Initialize scanner state for the first line
        /// </summary>
        public MultilineScanState(Encoding encodingForAlphanumericLiterals, bool insideDataDivision = false, bool decimalPointIsComma = false, bool withDebuggingMode = false, bool insideCopy = false) :
            this(insideDataDivision, false, false, new SpecialNamesContext(decimalPointIsComma), false, false, false, withDebuggingMode, insideCopy, encodingForAlphanumericLiterals, false, false, false)
        { }

        /// <summary>
        /// Initialize scanner state
        /// </summary>
        private MultilineScanState(bool insideDataDivision, bool insideProcedureDivision, bool insidePseudoText,
            SpecialNamesContext specialNamesContext, bool insideFormalizedComment, bool insideMultilineComments,
            bool insideParamsField, bool withDebuggingMode, bool insideCopy,
            Encoding encodingForAlphanumericLiterals, bool afterReplacementPseudoText, bool insideReplaceDirective, bool insideSql)
        {
            InsideDataDivision = insideDataDivision;
            InsideProcedureDivision = insideProcedureDivision;
            InsidePseudoText = insidePseudoText;
            InsideFormalizedComment = insideFormalizedComment;
            InsideMultilineComments = insideMultilineComments;
            InsideParamsField = insideParamsField;
            SpecialNames = specialNamesContext;
            WithDebuggingMode = withDebuggingMode;
            InsideCopy = insideCopy;
            EncodingForAlphanumericLiterals = encodingForAlphanumericLiterals;
            _afterReplacementPseudoText = afterReplacementPseudoText;
            InsideReplaceDirective = insideReplaceDirective;
            InsideSql = insideSql;
        }

        /// <summary>
        /// Clone the current scanner state 
        /// </summary>
        public MultilineScanState Clone()
        {
            MultilineScanState clone = new MultilineScanState(InsideDataDivision, InsideProcedureDivision, InsidePseudoText, SpecialNames.Clone(),
                InsideFormalizedComment, InsideMultilineComments, InsideParamsField, 
                WithDebuggingMode, InsideCopy, EncodingForAlphanumericLiterals, _afterReplacementPseudoText, InsideReplaceDirective, InsideSql);
            if (LastSignificantToken != null) clone.LastSignificantToken = LastSignificantToken;
            if (BeforeLastSignificantToken != null) clone.BeforeLastSignificantToken = BeforeLastSignificantToken;

#if EUROINFO_RULES
            clone.InsideRemarksDirective = InsideRemarksDirective;
            clone.InsideRemarksParentheses = InsideRemarksParentheses;
            clone.LeavingRemarksDirective = LeavingRemarksDirective;
#endif
            return clone;
        }

        /// <summary>
        /// Compute the next scanner state after recognizing a new token
        /// </summary>
        public void AdvanceToNextStateAndAdjustTokenProperties(Token newToken)
        {
            // Ignore whitespace separators
            if (newToken.TokenFamily == TokenFamily.Whitespace ||
                newToken.TokenFamily == TokenFamily.Comments)
            {
                return;
            }

            // Ignore pseudo-text tokens to update scan state
            if (InsidePseudoText && newToken.TokenType != TokenType.PseudoTextDelimiter && newToken.TokenType != TokenType.COPY)
            {
                return;
            }

            // Adjust token properties based on context
            AdjustPreviousTokenPropertiesBasedOnCurrentToken(newToken);
            AdjustCurrentTokenPropertiesBasedOnPreviousToken(newToken);

            // Inspect the new token and see if it changes the current Scan state
            switch (newToken.TokenType)
            {
                case TokenType.DIVISION:
                    if (LastSignificantToken != null)
                    {
                        // Register the start of the DATA DIVISION
                        if (LastSignificantToken.TokenType == TokenType.DATA)
                        {
                            InsideDataDivision = true;
                            InsideProcedureDivision = false;
                        }
                        // Register the start of the PROCEDURE DIVISION and end of DATA DIVISION
                        else if (LastSignificantToken.TokenType == TokenType.PROCEDURE)
                        {
                            InsideDataDivision = false;
                            InsideProcedureDivision = true;
                        }
                        // Register the end of PROCEDURE DIVISION
                        else if (LastSignificantToken.TokenType == TokenType.ID ||
                                 LastSignificantToken.TokenType == TokenType.IDENTIFICATION)
                        {
                            InsideProcedureDivision = false;
                        }
                    }
                    break;
                case TokenType.PseudoTextDelimiter:
                    // Register the start or the end of a pseudo text section
                    InsidePseudoText = !InsidePseudoText;
                    if (!InsidePseudoText && BeforeLastSignificantToken?.TokenType == TokenType.BY)
                    {
                        // Leaving a PseudoText located after the 'BY' keyword -> we are after a replacement pseudo text
                        _afterReplacementPseudoText = true;
                    }
                    break;
                case TokenType.COPY:
                    // Register the end of a pseudo text section (COPY not allowed in pseudo text)
                    if (InsidePseudoText)
                    {
                        InsidePseudoText = false;
                    }
                    break;
                case TokenType.DECIMAL_POINT:
                    // Register the occurence of a DECIMAL-POINT IS COMMA clause      
                    SpecialNames.DecimalPointIsComma = true;
                    break;
                case TokenType.MODE:
                    // Register the occurence of a WITH? DEBUGGING MODE clause    
                    if (LastSignificantToken != null &&
                        LastSignificantToken.TokenType == TokenType.DEBUGGING)
                    {
                        WithDebuggingMode = true;
                    }
                    break;
                case TokenType.SYMBOLIC:
                    // Register the start of a SYMBOLIC CHARACTERS? clause
                    SpecialNames.InsideSymbolicCharacterDefinitions = true;
                    break;
                case TokenType.FORMALIZED_COMMENTS_START:
                    // Register the begin of the formalized Comments
                    InsideFormalizedComment = true;
                    break;
                case TokenType.FORMALIZED_COMMENTS_STOP:
                    // Register the end of the formalized Comments
                    InsideFormalizedComment = false;
                    InsideParamsField = false;
                    break;
                case TokenType.FORMALIZED_COMMENTS_PARAMETERS:
                    // Register the begin of the params field inside the formalized Comments
                    InsideParamsField = true;
                    break;
                case TokenType.FORMALIZED_COMMENTS_DESCRIPTION:
                case TokenType.FORMALIZED_COMMENTS_DEPRECATED:
                case TokenType.FORMALIZED_COMMENTS_REPLACED_BY:
                case TokenType.FORMALIZED_COMMENTS_RESTRICTION:
                case TokenType.FORMALIZED_COMMENTS_NEED:
                case TokenType.FORMALIZED_COMMENTS_SEE:
                case TokenType.FORMALIZED_COMMENTS_TODO:
                    // Register the end of the params field inside the formalized Comments
                    InsideParamsField = false;
                    break;
                case TokenType.MULTILINES_COMMENTS_START:
                    // Register the begin of the formalized Comments                    
                    InsideMultilineComments = true;
                    break;
                case TokenType.MULTILINES_COMMENTS_STOP:
                    // Register the end of the formalized Comments
                    InsideMultilineComments = false;
                    return;
                case TokenType.CURRENCY:
                    // CURRENCY token is used either to satrt a CURRENCY SIGN clause or as an intrinsic type name in tC
                    if (LastSignificantToken?.TokenType != TokenType.TYPE)
                    {
                        SpecialNames.BeginCurrencySignClause();
                    }
                    break;
                case TokenType.SYMBOL:
                    // SYMBOL keyword is used only in CURRENCY SIGN clause
                    SpecialNames.WithPictureSymbol();
                    break;
                case TokenType.AlphanumericLiteral:
                case TokenType.HexadecimalAlphanumericLiteral:
                case TokenType.NullTerminatedAlphanumericLiteral:
                    if (SpecialNames.InsideCurrencySignDefinitions)
                    {
                        SpecialNames.OnAlphanumericLiteralToken(newToken);
                    }
                    break;
                case TokenType.REPLACE:
                    InsideReplaceDirective = true;
                    break;
                case TokenType.PeriodSeparator:
                    if (_afterReplacementPseudoText)
                    {
                        _afterReplacementPseudoText = false;
                        InsideReplaceDirective = false;
                    }
                    break;
            }

            // Avoid setting last significative token for multiline Comments
            if (InsideMultilineComments) { return; }

            // Register the end of a SYMBOLIC CHARACTERS? clause
            if (SpecialNames.InsideSymbolicCharacterDefinitions &&
                newToken.TokenType != TokenType.SYMBOLIC && newToken.TokenType != TokenType.CHARACTERS &&
                newToken.TokenType != TokenType.SymbolicCharacter &&
                newToken.TokenType != TokenType.IS && newToken.TokenType != TokenType.ARE &&
                newToken.TokenType != TokenType.IntegerLiteral)
            {
                SpecialNames.InsideSymbolicCharacterDefinitions = false;
            }

            // Register the end of all CURRENCY SIGN clauses
            if (SpecialNames.InsideCurrencySignDefinitions)
            {
                switch (newToken.TokenType)
                {
                    case TokenType.CURRENCY:
                    case TokenType.SIGN:
                    case TokenType.IS:
                    case TokenType.AlphanumericLiteral:
                    case TokenType.HexadecimalAlphanumericLiteral:
                    case TokenType.NullTerminatedAlphanumericLiteral:
                    case TokenType.WITH:
                    case TokenType.PICTURE:
                    case TokenType.SYMBOL:
                        break;
                    default:
                        SpecialNames.EndAllCurrencySignClauses();
                        break;
                }
            }

            // Register the last significant token 
            BeforeLastSignificantToken = LastSignificantToken;
            LastSignificantToken = newToken;
        }

        /// <summary>
        /// Determines if the given token may be dependent of a state when scanned.
        /// </summary>
        /// <param name="token">The token to be checked for scanning state dependency</param>
        /// <returns>true if the token may be dependent of a scanning state, false otherwise.</returns>
        public static bool IsScanStateDependent(Token token)
        {
            switch (token.TokenType)
            {
                case TokenType.IntegerLiteral:
                case TokenType.UserDefinedWord:
                case TokenType.DATA:
                case TokenType.DELETE:
                case TokenType.END:
                case TokenType.FILE:
                case TokenType.ID:
                case TokenType.NEXT:
                case TokenType.PROCEDURE:
                case TokenType.SERVICE:
                case TokenType.WHEN:
                    return true;
            }
            return false;
        }

        private void AdjustPreviousTokenPropertiesBasedOnCurrentToken(Token newToken)
        {
            if (LastSignificantToken != null)
            {
                // Adjust previous token properties based on current context            
                switch (LastSignificantToken.TokenType)
                {
                    case TokenType.DATA:
                        if (newToken.TokenType != TokenType.DIVISION)
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;


                    //DELETE_CD is the DELETE for Preprocessor
                    //DELETE is for Cobol statement
                    //Scanner cannot differentiate these 2 DELETE keywords 

                    //DELETE_CD is valid only if followed by an integer or a range of number.
                    //A range of number is scanned as an UserDefinedWord
                    //A range of number contains only numeric character or char '-'
                    case TokenType.DELETE:
                        if (newToken.TokenType == TokenType.IntegerLiteral
                            || (newToken.TokenType == TokenType.UserDefinedWord &&
                                newToken.Text.All(c => char.IsDigit(c) || c == '-')))
                        {
                            LastSignificantToken.CorrectType(TokenType.DELETE_CD);
                        }
                        break;
                    case TokenType.END:
                        if (!(newToken.TokenType == TokenType.PROGRAM || newToken.TokenType == TokenType.CLASS ||
                              newToken.TokenType == TokenType.FACTORY || newToken.TokenType == TokenType.OBJECT ||
                              newToken.TokenType == TokenType.METHOD || newToken.TokenType == TokenType.DECLARATIVES))
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;
                    case TokenType.FILE:
                        if (newToken.TokenType != TokenType.SECTION)
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;
                    case TokenType.ID:
                        if (newToken.TokenType != TokenType.DIVISION)
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;
                    case TokenType.NEXT:
                        if (newToken.TokenType != TokenType.SENTENCE)
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;
                    case TokenType.PROCEDURE:
                        if (newToken.TokenType != TokenType.DIVISION)
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;
                    case TokenType.SERVICE:
                        if (newToken.TokenType == TokenType.LABEL || newToken.TokenType == TokenType.RELOAD)
                        {
                            LastSignificantToken.CorrectType(TokenType.SERVICE_CD);
                        }
                        break;
                    case TokenType.WHEN:
                        if (newToken.TokenType == TokenType.HIGH_VALUE || newToken.TokenType == TokenType.HIGH_VALUES ||
                            newToken.TokenType == TokenType.LOW_VALUE || newToken.TokenType == TokenType.LOW_VALUES ||
                            newToken.TokenType == TokenType.SPACE || newToken.TokenType == TokenType.SPACES ||
                            newToken.TokenType == TokenType.ZERO || newToken.TokenType == TokenType.ZEROS || newToken.TokenType == TokenType.ZEROS)
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;
                }
            }
        }

        private void AdjustCurrentTokenPropertiesBasedOnPreviousToken(Token newToken)
        {
            if (LastSignificantToken != null)
            {
                // Adjust current token properties based on previous context
                switch (newToken.TokenType)
                {
                    case TokenType.DECLARATIVES:
                        if (LastSignificantToken.TokenType == TokenType.END)
                        {
                            LastSignificantToken.DegradePotentialCodeElementStartingKeywordToSyntaxKeyword();
                        }
                        break;
                }
            }
        }

        /// <summary>
        /// True after (EXEC | EXECUTE) (SQL | SQLIMS)
        /// </summary>
        public bool AfterExecSql
        {
            get
            {
                return LastSignificantToken != null &&
                LastSignificantToken.TokenType == TokenType.ExecTranslatorName &&
               (LastSignificantToken.Text.Equals("SQL", StringComparison.InvariantCultureIgnoreCase) ||
                LastSignificantToken.Text.Equals("SQLIMS", StringComparison.InvariantCultureIgnoreCase));
            }
        }

        /// <summary>
        /// True after (EXEC | EXECUTE)
        /// </summary>
        public bool AfterExec
        {
            get { return LastSignificantToken != null && (LastSignificantToken.TokenType == TokenType.EXEC || LastSignificantToken.TokenType == TokenType.EXECUTE); }
        }

        public bool AfterExecTranslatorName
        {
            get { return LastSignificantToken != null && LastSignificantToken.TokenType == TokenType.ExecTranslatorName; }
        }

        public bool AfterExecStatementText
        {
            get { return LastSignificantToken != null && LastSignificantToken.TokenType == TokenType.ExecStatementText; }
        }

        /// <summary>
        /// True after (PIC | PICTURE) IS?
        /// </summary>
        public bool AfterPicture
        {
            get
            {
                bool afterPicOrPicture = LastSignificantToken != null && (LastSignificantToken.TokenType == TokenType.PIC || LastSignificantToken.TokenType == TokenType.PICTURE);
                if (afterPicOrPicture) return true;
                bool afterPicOrPictureIs = BeforeLastSignificantToken != null && (BeforeLastSignificantToken.TokenType == TokenType.PIC || BeforeLastSignificantToken.TokenType == TokenType.PICTURE)
                                        && LastSignificantToken != null && LastSignificantToken.TokenType == TokenType.IS;
                return afterPicOrPictureIs;
            }
        }

        /// <summary>
        /// True after (AUTHOR | INSTALLATION | DATE_WRITTEN | DATE_COMPILED | SECURITY)
        /// </summary>
        public bool AfterCommentEntryKeyword
        {
            get
            {
                return LastSignificantToken != null &&
                     (LastSignificantToken.TokenType == TokenType.AUTHOR ||
                      LastSignificantToken.TokenType == TokenType.INSTALLATION ||
                      LastSignificantToken.TokenType == TokenType.DATE_WRITTEN ||
                      LastSignificantToken.TokenType == TokenType.DATE_COMPILED ||
                      LastSignificantToken.TokenType == TokenType.SECURITY);
            }
        }

        /// <summary>
        /// True after (AUTHOR | INSTALLATION | DATE_WRITTEN | DATE_COMPILED | SECURITY) PeriodSeparator
        /// </summary>
        public bool AfterCommentEntryKeywordPeriod
        {
            get
            {
                return BeforeLastSignificantToken != null &&
                     (BeforeLastSignificantToken.TokenType == TokenType.AUTHOR ||
                      BeforeLastSignificantToken.TokenType == TokenType.INSTALLATION ||
                      BeforeLastSignificantToken.TokenType == TokenType.DATE_WRITTEN ||
                      BeforeLastSignificantToken.TokenType == TokenType.DATE_COMPILED ||
                      BeforeLastSignificantToken.TokenType == TokenType.SECURITY) &&
                    LastSignificantToken != null && LastSignificantToken.TokenType == TokenType.PeriodSeparator;
            }
        }

        public bool AfterCommentEntry
        {
            get { return LastSignificantToken != null && LastSignificantToken.TokenType == TokenType.CommentEntry; }
        }

        public bool AfterFUNCTION
        {
            get { return LastSignificantToken != null && LastSignificantToken.TokenType == TokenType.FUNCTION; }
        }

        /// <summary>
        /// True at the beggining of a parse section, or after PeriodSeparator, or after END-EXEC
        /// </summary>
        public bool AtBeginningOfSentence
        {
            get
            {
                return LastSignificantToken == null || LastSignificantToken.TokenType == TokenType.PeriodSeparator || LastSignificantToken.TokenType == TokenType.END_EXEC || LastSignificantToken.TokenType == TokenType.FORMALIZED_COMMENTS_STOP ||
                  // Special cases : compiler directives sometimes without a final PeriodSeparator
                  // 1. COPY UserDefinedWord <= sometimes PeriodSeparator missing here.
                  //    Has no impact except if the next token is a numeric or alphanumeric literal, which can't happen inside a COPY directive.
                  (BeforeLastSignificantToken != null && (BeforeLastSignificantToken.TokenType == TokenType.COPY || BeforeLastSignificantToken.TokenType == TokenType.EXEC_SQL) && LastSignificantToken.TokenType == TokenType.UserDefinedWord) ||
                  // 2. EJECT | SKIP1 | SKIP2 | SKIP3 <= sometimes PeriodSeparator missing here.
                  (LastSignificantToken != null && (LastSignificantToken.TokenType == TokenType.EJECT || LastSignificantToken.TokenType == TokenType.SKIP1 || LastSignificantToken.TokenType == TokenType.SKIP2 || LastSignificantToken.TokenType == TokenType.SKIP3)) ||
                  // 3. TITLE alphanumericValue2 <= sometimes PeriodSeparator missing here.
                  (BeforeLastSignificantToken != null && BeforeLastSignificantToken.TokenType == TokenType.TITLE && LastSignificantToken.TokenFamily == TokenFamily.AlphanumericLiteral);
            }
        }

        /// <summary>
        /// Used to check if an update to a TokensLine modified the scanner context for the following lines
        /// </summary>
        public override bool Equals(object obj)
        {
            return Equals(obj as MultilineScanState);
        }

        public bool Equals(MultilineScanState otherScanState)
        {
            if (Object.ReferenceEquals(this, otherScanState)) return true;
            if (Object.ReferenceEquals(null, otherScanState)) return false;

            return InsideDataDivision == otherScanState.InsideDataDivision &&
                   InsideProcedureDivision == otherScanState.InsideProcedureDivision &&
                   InsidePseudoText == otherScanState.InsidePseudoText &&
                   SpecialNames.Equals(otherScanState.SpecialNames) &&
                   InsideFormalizedComment == otherScanState.InsideFormalizedComment &&
                   InsideParamsField == otherScanState.InsideParamsField &&
                   InsideMultilineComments == otherScanState.InsideMultilineComments &&
                   InsideCopy == otherScanState.InsideCopy &&

#if EUROINFO_RULES
                   InsideRemarksDirective == otherScanState.InsideRemarksDirective &&
                   //((CopyTextNamesVariations == null && otherScanState.CopyTextNamesVariations == null) ||
                   // (CopyTextNamesVariations != null && otherScanState.CopyTextNamesVariations != null && CopyTextNamesVariations.Count == otherScanState.CopyTextNamesVariations.Count)) &&
#endif
                   WithDebuggingMode == otherScanState.WithDebuggingMode &&
                   EncodingForAlphanumericLiterals == otherScanState.EncodingForAlphanumericLiterals &&
                   _afterReplacementPseudoText == otherScanState._afterReplacementPseudoText &&
                   InsideReplaceDirective == otherScanState.InsideReplaceDirective &&
                   InsideSql == otherScanState.InsideSql;
        }

        /// <summary>
        /// Must stay coherent with Equals method
        /// </summary>
        public override int GetHashCode()
        {
            unchecked // Overflow is fine, just wrap
            {
                int hash = 17;
                // Suitable nullity checks etc, of course :)
                hash = hash * 23 + InsideDataDivision.GetHashCode();
                hash = hash * 23 + InsideProcedureDivision.GetHashCode();
                hash = hash * 23 + InsidePseudoText.GetHashCode();
                hash = hash * 23 + SpecialNames.GetHashCode();
                hash = hash * 23 + InsideFormalizedComment.GetHashCode();
                hash = hash * 23 + InsideParamsField.GetHashCode();
                hash = hash * 23 + InsideMultilineComments.GetHashCode();
                hash = hash * 23 + InsideCopy.GetHashCode();

#if EUROINFO_RULES
                hash = hash * 23 + InsideRemarksDirective.GetHashCode();
#endif
                hash = hash * 23 + WithDebuggingMode.GetHashCode();
                hash = hash * 23 + EncodingForAlphanumericLiterals.GetHashCode();
                hash = hash * 23 + _afterReplacementPseudoText.GetHashCode();
                hash = hash * 23 + InsideReplaceDirective.GetHashCode();
                hash = hash * 23 + InsideSql.GetHashCode();
                return hash;
            }
        }
    }
}
