﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Internal Scanner state propagated from one line to the other when compiling a complete source file
    /// </summary>
    public class MultilineScanState
    {
        /// <summary>
        /// Last keyword or symbol token encountered in the text file
        /// </summary>
        private Token LastSignificantToken { get; set; }

        /// <summary>
        /// Last keyword or symbol token encountered in the text file
        /// </summary>
        private Token BeforeLastSignificantToken { get; set; }

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
        /// True if we are inside the symbolicCharactersClause > symbolicCharacterDefinition+
        /// </summary>
        public bool InsideSymbolicCharacterDefinitions { get; private set; }

        /// <summary>
        /// True as soon as the keyword DECIMAL-POINT has been encountered
        /// </summary>
        public bool DecimalPointIsComma { get; private set; }

        /// <summary>
        /// True as soon as the keywords DEBUGGING MODE have been encountered
        /// </summary>
        public bool WithDebuggingMode { get; private set; }

        /// <summary>
        /// Encoding of the text file : used to decode the value of an hexadecimal alphanumeric literal
        /// </summary>
        public Encoding EncodingForAlphanumericLiterals { get; private set; }

        /// <summary>
        /// Symbolic character names previously defined in the source file
        /// NB : value will be null until at least one symbolic character is defined
        /// => only use method AddSymbolicCharacter to safely add an element to this list
        /// </summary>
        public IList<string> SymbolicCharacters { get; private set; }

        /// <summary>
        /// Register a new symbolic character name found in the source file
        /// </summary>
        public void AddSymbolicCharacter(string tokenText)
        {
            if (SymbolicCharacters == null)
            {
                SymbolicCharacters = new List<string>();
            }
            SymbolicCharacters.Add(tokenText);
        }

#if EUROINFO_LEGACY_REPLACING_SYNTAX

        /// <summary>
        /// True if we detect in the comments lines stream that we are inside a REMARKS compiler directive.
        /// </summary>
        internal bool InsideRemarksDirective { get; set; }

        /// <summary>
        /// True if we are inside a COPY=(..) of a REMARKS compiler directive.
        /// </summary>
        internal bool InsideRemarksParentheses { get; set; }

        /// <summary>
        /// Text names variations declared in REMARS compiler directives.
        /// </summary>
        internal List<RemarksDirective.TextNameVariation> CopyTextNamesVariations { get; set; }

        /// <summary>
        /// Register a new symbolic character name found in the source file
        /// </summary>
        internal void AddCopyTextNamesVariations(IList<RemarksDirective.TextNameVariation> textNamesVariations)
        {
            if (CopyTextNamesVariations == null)
            {
                CopyTextNamesVariations = new List<RemarksDirective.TextNameVariation>();
            }
            else
            {
                CopyTextNamesVariations = new List<RemarksDirective.TextNameVariation>(CopyTextNamesVariations);
            }
            CopyTextNamesVariations.AddRange(textNamesVariations);
        }

#endif

        /// <summary>
        /// Initialize scanner state for the first line
        /// </summary>
        public MultilineScanState(bool insideDataDivision, bool decimalPointIsComma, bool withDebuggingMode, Encoding encodingForAlphanumericLiterals) :
            this(insideDataDivision, false, false, false, decimalPointIsComma, withDebuggingMode, encodingForAlphanumericLiterals)
        { }

        /// <summary>
        /// Initialize scanner state
        /// </summary>
        public MultilineScanState(bool insideDataDivision, bool insideProcedureDivision, bool insidePseudoText, bool insideSymbolicCharacterDefinitions, bool decimalPointIsComma, bool withDebuggingMode, Encoding encodingForAlphanumericLiterals)
        {
            InsideDataDivision = insideDataDivision;
            InsideProcedureDivision = insideProcedureDivision;
            InsidePseudoText = insidePseudoText;
            InsideSymbolicCharacterDefinitions = insideSymbolicCharacterDefinitions;
            DecimalPointIsComma = decimalPointIsComma;
            WithDebuggingMode = withDebuggingMode;
            EncodingForAlphanumericLiterals = encodingForAlphanumericLiterals;
        }

        /// <summary>
        /// Clone the current scanner state 
        /// </summary>
        public MultilineScanState Clone()
        {
            MultilineScanState clone = new MultilineScanState(InsideDataDivision, InsideProcedureDivision, InsidePseudoText, InsideSymbolicCharacterDefinitions, DecimalPointIsComma, WithDebuggingMode, EncodingForAlphanumericLiterals);
            if (LastSignificantToken != null) clone.LastSignificantToken = LastSignificantToken;
            if (BeforeLastSignificantToken != null) clone.BeforeLastSignificantToken = BeforeLastSignificantToken;
            if (SymbolicCharacters != null)
            {
                clone.SymbolicCharacters = new List<string>(SymbolicCharacters);
            }
#if EUROINFO_LEGACY_REPLACING_SYNTAX
            clone.InsideRemarksDirective = InsideRemarksDirective;
            clone.InsideRemarksParentheses = InsideRemarksParentheses;
            if (CopyTextNamesVariations != null)
            {
                clone.CopyTextNamesVariations = CopyTextNamesVariations;
            }
#endif
            return clone;
        }

        /// <summary>
        /// Compute the next scanner state after recognizing a new token
        /// </summary>
        public void AdvanceToNextStateAndAdjustTokenProperties(Token newToken)
        {
            // Ignore whitespace separators and comments to update scan state
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
                    DecimalPointIsComma = true;
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
                    InsideSymbolicCharacterDefinitions = true;
                    break;
            }
            // Register the end of a SYMBOLIC CHARACTERS? clause
            if (InsideSymbolicCharacterDefinitions &&
                newToken.TokenType != TokenType.SYMBOLIC && newToken.TokenType != TokenType.CHARACTERS &&
                newToken.TokenType != TokenType.SymbolicCharacter &&
                newToken.TokenType != TokenType.IS && newToken.TokenType != TokenType.ARE &&
                newToken.TokenType != TokenType.IntegerLiteral)
            {
                InsideSymbolicCharacterDefinitions = false;
            }

            // Register the last significant token 
            BeforeLastSignificantToken = LastSignificantToken;
            LastSignificantToken = newToken;
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
                    case TokenType.DELETE:
                        if (newToken.TokenType == TokenType.IntegerLiteral)
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
            get { return LastSignificantToken == null || LastSignificantToken.TokenType == TokenType.PeriodSeparator || LastSignificantToken.TokenType == TokenType.END_EXEC ||
                    // Special cases : compiler directives sometimes without a final PeriodSeparator
                    // 1. COPY UserDefinedWord <= sometimes PeriodSeparator missing here.
                    //    Has no impact except if the next token is a numeric or alphanumeric literal, which can't happen inside a COPY directive.
                    (BeforeLastSignificantToken!=null && (BeforeLastSignificantToken.TokenType == TokenType.COPY || BeforeLastSignificantToken.TokenType == TokenType.EXEC_SQL_INCLUDE) && LastSignificantToken.TokenType == TokenType.UserDefinedWord) ||
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
            MultilineScanState otherScanState = obj as MultilineScanState;
            if (otherScanState == null)
            {
                return false;
            }
            else
            {
                return InsideDataDivision == otherScanState.InsideDataDivision &&
                       InsideProcedureDivision == otherScanState.InsideProcedureDivision &&
                       InsidePseudoText == otherScanState.InsidePseudoText &&
                       InsideSymbolicCharacterDefinitions == otherScanState.InsideSymbolicCharacterDefinitions &&
#if EUROINFO_LEGACY_REPLACING_SYNTAX
                InsideRemarksDirective == otherScanState.InsideRemarksDirective &&
                    ((CopyTextNamesVariations == null && otherScanState.CopyTextNamesVariations == null) ||
                     (CopyTextNamesVariations != null && otherScanState.CopyTextNamesVariations != null && CopyTextNamesVariations.Count == otherScanState.CopyTextNamesVariations.Count)) &&
#endif
                    DecimalPointIsComma == otherScanState.DecimalPointIsComma &&
                    WithDebuggingMode == otherScanState.WithDebuggingMode &&
                    EncodingForAlphanumericLiterals == otherScanState.EncodingForAlphanumericLiterals &&
                    ((SymbolicCharacters == null && otherScanState.SymbolicCharacters == null) || 
                     (SymbolicCharacters != null && otherScanState.SymbolicCharacters != null && SymbolicCharacters.Count == otherScanState.SymbolicCharacters.Count));
            }
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
                hash = hash * 23 + InsideSymbolicCharacterDefinitions.GetHashCode();
#if EUROINFO_LEGACY_REPLACING_SYNTAX
                hash = hash * 23 + InsideRemarksDirective.GetHashCode();
                if (CopyTextNamesVariations != null)
                {
                    hash = hash * 23 + CopyTextNamesVariations.Count;
                }
#endif
                hash = hash * 23 + DecimalPointIsComma.GetHashCode();
                hash = hash * 23 + WithDebuggingMode.GetHashCode();
                hash = hash * 23 + EncodingForAlphanumericLiterals.GetHashCode();
                if (SymbolicCharacters != null)
                {
                    hash = hash * 23 + SymbolicCharacters.Count;
                }
                return hash;
            }
        }        
    }    
}
