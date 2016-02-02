using System;
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
#if EUROINFO_LEGACY_REPLACING_SYNTAX

        /// <summary>
        /// True if we detect in the comments lines stream that we are inside a REMARKS compiler directive.
        /// </summary>
        public bool InsideRemarksDirective { get; set; }

        /// <summary>
        /// Text names variations declared in REMARS compiler directives.
        /// </summary>
        public List<RemarksDirective.TextNameVariation> CopyTextNamesVariations { get; private set; }

        /// <summary>
        /// Register a new symbolic character name found in the source file
        /// </summary>
        public void AddCopyTextNamesVariations(IList<RemarksDirective.TextNameVariation> textNamesVariations)
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
        /// True if we know from the keyword stream that we are inside a DATA DIVISION.
        /// Used by the Scanner to disambiguate similar keywords based on their context of appearance. 
        /// </summary>
        public bool InsideDataDivision { get; private set; }

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

        /// <summary>
        /// Interesting positions in the sequence of keywords recognized by the scanner,
        /// used to disambiguate context-sensitive keywords
        /// </summary>
        internal KeywordsSequenceState KeywordsState { get; private set; }

        /// <summary>
        /// Last token encountered in the text file, including whitespace but excluding comments
        /// </summary>
        internal Token LastToken { get; private set; }

        /// <summary>
        /// Last keyword or symbol token encountered in the text file
        /// </summary>
        internal Token LastKeywordOrSymbolToken { get; private set; }

        /// <summary>
        /// Initialize scanner state for the first line
        /// </summary>
        public MultilineScanState(bool insideDataDivision, bool decimalPointIsComma, bool withDebuggingMode, Encoding encodingForAlphanumericLiterals)
        {
            InsideDataDivision = insideDataDivision;
            DecimalPointIsComma = decimalPointIsComma;
            WithDebuggingMode = withDebuggingMode;
            EncodingForAlphanumericLiterals = encodingForAlphanumericLiterals;
            KeywordsState = KeywordsSequenceState.Default;
        }

        /// <summary>
        /// Clone the current scanner state 
        /// </summary>
        public MultilineScanState Clone()
        {
            MultilineScanState clone = new MultilineScanState(InsideDataDivision, DecimalPointIsComma, WithDebuggingMode, EncodingForAlphanumericLiterals);
#if EUROINFO_LEGACY_REPLACING_SYNTAX
            clone.InsideRemarksDirective = InsideRemarksDirective;
            if(CopyTextNamesVariations != null)
            {
                clone.CopyTextNamesVariations = CopyTextNamesVariations;
            }
#endif
            if (SymbolicCharacters != null)
            {
                clone.SymbolicCharacters = new List<string>(SymbolicCharacters);
            }
            clone.KeywordsState = KeywordsState;
            if (LastToken != null) clone.LastToken = LastToken;
            if (LastKeywordOrSymbolToken != null) clone.LastKeywordOrSymbolToken = LastKeywordOrSymbolToken;
            return clone;
        }

        /// <summary>
        /// Compute the next scanner state after recognizing a new token
        /// </summary>
        public void AdvanceToNextState(Token newToken)
        {
            // Register the last token (except if it is a comment)
            if (newToken.TokenFamily != TokenFamily.Comments)
            {
                LastToken = newToken;
                // Register the last Keyword or Symbol token 
                if (newToken.TokenFamily >= TokenFamily.Symbol)
                {
                    LastKeywordOrSymbolToken = newToken;
                }
            }

            // Ignore whitespace separators and comments to advance keywords state
            if (newToken.TokenFamily == TokenFamily.Whitespace ||
                newToken.TokenFamily == TokenFamily.Comments)
            {   
                return;
            }

            // Register the occurence of a DECIMAL-POINT IS COMMA clause
            // (The configuration section can be specified only in the ENVIRONMENT
            //  DIVISION of the outermost program of a COBOL source program)
            if(newToken.TokenType == TokenType.DECIMAL_POINT)
            {
                DecimalPointIsComma = true;
            }

            // Keywords state machine
            switch (KeywordsState)
            {
                // Start state -> Token -> End state. Action
                // 0 -> PICTURE -> 1. 
                // 0 -> PIC -> 1. 
                // 0 -> AUTHOR -> 3. 
                // 0 -> INSTALLATION -> 3. 
                // 0 -> DATE_WRITTEN -> 3. 
                // 0 -> DATE_COMPILED -> 3. 
                // 0 -> SECURITY -> 3. 
                // 0 -> FUNCTION -> 6. 
                // 0 -> EXEC -> 7. 
                // 0 -> EXECUTE -> 7. 
                // 0 -> DELETE -> 10. 
                // 0 -> DATA -> 11. 
                // 0 -> PROCEDURE -> 12. 
                // 0 -> TO -> 13. 
                // 0 -> SAME -> 14.
                // 0 -> SYMBOLIC -> 15.
                // 0 -> PseudoTextDelimiter -> 17.
                // 0 -> DEBUGGING -> 18.
                case KeywordsSequenceState.Default:
                    switch (newToken.TokenType)
                    {
                        case TokenType.PICTURE:
                        case TokenType.PIC:
                            KeywordsState = KeywordsSequenceState.After_PIC_orPICTURE;
                            break;
                        case TokenType.AUTHOR:
                        case TokenType.INSTALLATION:
                        case TokenType.DATE_WRITTEN:
                        case TokenType.DATE_COMPILED:
                        case TokenType.SECURITY:
                            KeywordsState = KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY;
                            break;
                        case TokenType.FUNCTION:
                            KeywordsState = KeywordsSequenceState.After_FUNCTION;
                            break;
                        case TokenType.EXEC:
                        case TokenType.EXECUTE:
                            KeywordsState = KeywordsSequenceState.After_EXEC_orEXECUTE;
                            break;
                        case TokenType.DELETE:
                            KeywordsState = KeywordsSequenceState.After_DELETE;
                            break;
                        case TokenType.DATA:
                            KeywordsState = KeywordsSequenceState.After_DATA;
                            break;
                        case TokenType.PROCEDURE:
                            KeywordsState = KeywordsSequenceState.After_PROCEDURE;
                            break;
                        case TokenType.TO:
                            KeywordsState = KeywordsSequenceState.After_TO;
                            break;
                        case TokenType.SAME:
                            KeywordsState = KeywordsSequenceState.After_SAME;
                            break;
                        case TokenType.SYMBOLIC:
                            KeywordsState = KeywordsSequenceState.After_SYMBOLIC;
                            break;
                        case TokenType.PseudoTextDelimiter:
                            KeywordsState = KeywordsSequenceState.InsidePseudoText;
                            break;
                        case TokenType.DEBUGGING:
                            KeywordsState = KeywordsSequenceState.After_DEBUGGING;
                            break;
                    }
                    break;
                // 1 -> SYMBOL -> 0. 
                // 1 -> IS -> 2. 
                // 1 -> pictureCharacterString -> 0. 
                case KeywordsSequenceState.After_PIC_orPICTURE:
                    if (newToken.TokenType == TokenType.IS)
                    {
                        KeywordsState = KeywordsSequenceState.After_PIC_orPICTURE_IS;
                    }
                    else
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 2 -> pictureCharacterString -> 0. 
                case KeywordsSequenceState.After_PIC_orPICTURE_IS:
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
                // 3 -> commentEntry -> 5. 
                // 3 -> PeriodSeparator -> 4. 
                case KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY:
                    if (newToken.TokenType == TokenType.PeriodSeparator)
                    {
                        KeywordsState = KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY_PeriodSeparator;
                    }
                    else if (newToken.TokenType == TokenType.CommentEntry)
                    {
                        KeywordsState = KeywordsSequenceState.After_CommentEntry;
                    }
                    else
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 4 -> commentEntry -> 5. 
                // 4 -> chars in area A -> 0.
                case KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY_PeriodSeparator:
                    if (newToken.TokenType == TokenType.CommentEntry)
                    {
                        KeywordsState = KeywordsSequenceState.After_CommentEntry;
                    }
                    else
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 5 -> commentEntry -> 5. 
                // 5 -> chars in area A -> 0.
                case KeywordsSequenceState.After_CommentEntry:
                    if (newToken.TokenType != TokenType.CommentEntry)
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 6 -> FunctionName -> 0. 
                case KeywordsSequenceState.After_FUNCTION:
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
                // 7 -> execTranslatorName -> 8. 
                case KeywordsSequenceState.After_EXEC_orEXECUTE:
                    KeywordsState = KeywordsSequenceState.After_EXEC_orEXECUTE_ExecTranslatorName;
                    break;
                // 8 -> execStatementText -> 9. 
                // 8 -> END_EXEC -> 0. 
                // 8 -> EXEC_SQL_INCLUDE  -> 0.
                case KeywordsSequenceState.After_EXEC_orEXECUTE_ExecTranslatorName:
                    if (newToken.TokenType == TokenType.ExecStatementText)
                    {
                        KeywordsState = KeywordsSequenceState.After_ExecStatementText;
                    }
                    else
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 9 -> execStatementText -> 9. 
                // 8 -> END_EXEC -> 9. 
                case KeywordsSequenceState.After_ExecStatementText:
                    if (newToken.TokenType != TokenType.ExecStatementText)
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 10 -> UserDefinedWord -> 0. 
                // 10 -> IntegerLiteral -> 0. =rw=> DELETE_CD
                case KeywordsSequenceState.After_DELETE:
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
                // 11 -> DIVISION -> 0. inside data division = true
                case KeywordsSequenceState.After_DATA:
                    if(newToken.TokenType == TokenType.DIVISION)
                    {
                        InsideDataDivision = true;
                    }
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
                // 12 -> DIVISION -> 0. inside data division = false
                case KeywordsSequenceState.After_PROCEDURE:
                    if(newToken.TokenType == TokenType.DIVISION)
                    {
                        InsideDataDivision = false;
                    }
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
                // 13 -> ENTRY -> 0. =rw=> ENTRY_ARG
                case KeywordsSequenceState.After_TO:
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
                // 14 -> SORT -> 0. =rw=> SORT_ARG
                case KeywordsSequenceState.After_SAME:
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
                // 15 -> CHARACTERS -> 15
                // 15 -> SymbolicCharacter -> 15
                // 15 -> any other -> 16
                case KeywordsSequenceState.After_SYMBOLIC:
                    if (newToken.TokenType != TokenType.CHARACTER &&
                        newToken.TokenType != TokenType.SymbolicCharacter)
                    {
                        KeywordsState = KeywordsSequenceState.After_SYMBOLIC_SymbolicCharacters;
                    }
                    break;
                // 16 -> IntegerLiteral -> 16
                // 16 -> SYMBOLIC | SymbolicCharacter -> 15
                // 16 -> any other -> 0
                case KeywordsSequenceState.After_SYMBOLIC_SymbolicCharacters:
                    if (newToken.TokenType == TokenType.SymbolicCharacter || newToken.TokenType == TokenType.SYMBOLIC)
                    {
                        KeywordsState = KeywordsSequenceState.After_SYMBOLIC;
                    }
                    else if(newToken.TokenType != TokenType.IntegerLiteral)
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 17 -> PseudoTextDelimiter -> 0.
                case KeywordsSequenceState.InsidePseudoText:
                    if(newToken.TokenType == TokenType.PseudoTextDelimiter || newToken.TokenType == TokenType.COPY)
                    {
                        KeywordsState = KeywordsSequenceState.Default;
                    }
                    break;
                // 18 -> MODE : WithDebuggingMode = true -> 0. 
                // 1/ -> any other -> 0. 
                case KeywordsSequenceState.After_DEBUGGING:
                    if (newToken.TokenType == TokenType.MODE)
                    {
                        WithDebuggingMode = true;
                    }
                    KeywordsState = KeywordsSequenceState.Default;
                    break;
            }
        }

        /// <summary>
        /// Call to reset keywords state before scanning a new token
        /// </summary>
        public void ResetKeywordsState()
        {
            KeywordsState = KeywordsSequenceState.Default;
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
#if EUROINFO_LEGACY_REPLACING_SYNTAX
                    InsideRemarksDirective == otherScanState.InsideRemarksDirective &&
                    ((CopyTextNamesVariations == null && otherScanState.CopyTextNamesVariations == null) ||
                     (CopyTextNamesVariations != null && otherScanState.CopyTextNamesVariations != null && CopyTextNamesVariations.Count == otherScanState.CopyTextNamesVariations.Count)) &&
#endif
                    DecimalPointIsComma == otherScanState.DecimalPointIsComma &&
                    WithDebuggingMode == otherScanState.WithDebuggingMode &&
                    EncodingForAlphanumericLiterals == otherScanState.EncodingForAlphanumericLiterals &&
                    ((SymbolicCharacters == null && otherScanState.SymbolicCharacters == null) || 
                     (SymbolicCharacters != null && otherScanState.SymbolicCharacters != null && SymbolicCharacters.Count == otherScanState.SymbolicCharacters.Count)) &&
                    KeywordsState == otherScanState.KeywordsState;
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
                hash = hash * 23 + KeywordsState.GetHashCode();
                return hash;
            }
        }        
    }    

    /// <summary>
    /// Interesting positions in the sequence of keywords recognized by the scanner,
    /// used to disambiguate context-sensitive keywords
    /// </summary>
    internal enum KeywordsSequenceState
    {
        Default = 0, 
        After_PIC_orPICTURE = 1,
        After_PIC_orPICTURE_IS = 2,
        After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY = 3,
        After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY_PeriodSeparator = 4,
        After_CommentEntry = 5,
        After_FUNCTION = 6, 
        After_EXEC_orEXECUTE = 7, 
        After_EXEC_orEXECUTE_ExecTranslatorName = 8, 
        After_ExecStatementText = 9,
        After_DELETE = 10,
        After_DATA = 11, 
        After_PROCEDURE = 12, 
        After_TO = 13,
        After_SAME = 14,
        After_SYMBOLIC = 15,
        After_SYMBOLIC_SymbolicCharacters = 16,
        InsidePseudoText = 17,
        After_DEBUGGING = 18
    }
}
