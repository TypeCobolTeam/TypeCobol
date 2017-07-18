using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using Analytics;
using JetBrains.Annotations;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Divides a line of text into a list of tokens
    /// </summary>
    public class Scanner
    {

        /// <summary>
        /// Issue #428, quick fix for this issue.
        /// Method ScanIsolatedTokenInDefaultContext need the scanState of the previous token in order to parser the new token
        /// correctly. But the caller of this method doesn't have the scanState. It only has the scanState at the beginning of the line.
        /// A solution would be to rescan all the line.
        /// </summary>
        public bool BeSmartWithLevelNumber { get; set; }
        /// <summary>
        /// Scan a line of a document when no previous scan state object is available
        /// </summary>
        public static void ScanFirstLine(TokensLine tokensLine, bool insideDataDivision, bool decimalPointIsComma, bool withDebuggingMode, Encoding encodingForAlphanumericLiterals, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {
            MultilineScanState initialScanState = new MultilineScanState(insideDataDivision, decimalPointIsComma, withDebuggingMode, encodingForAlphanumericLiterals);            
            ScanTokensLine(tokensLine, initialScanState, compilerOptions, copyTextNameVariations);
        }

        /// <summary>
        /// Scan a line of a document
        /// </summary>
        public static void ScanTokensLine(TokensLine tokensLine, MultilineScanState initialScanState, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {
            // Updates are forbidden after a snapshot of a specific version of a line
            if(!tokensLine.CanStillBeUpdatedBy(CompilationStep.Scanner))
            {
                throw new InvalidOperationException("Can not update this TokensLine because it was already frozen by compilation step : " + tokensLine.CompilationStep.ToString());
            }

            // Set the initial scan state for the line
            tokensLine.InitializeScanState(initialScanState);

            // Alias to refer to Cobol text line properties
            ICobolTextLine textLine = tokensLine;
            
            // The source section of this line of text must be split into tokens    
            string line = tokensLine.Text;
            int startIndex = textLine.Source.StartIndex;
            int lastIndex = textLine.Source.EndIndex;

#if EUROINFO_LEGACY_REPLACING_SYNTAX
            if(tokensLine.ScanState.LeavingRemarksDirective) //If last scanned line was the end of a remarksDirective then mark scanstate as outside of remarksDirective for this new line
                tokensLine.ScanState.InsideRemarksDirective = tokensLine.ScanState.LeavingRemarksDirective = false;

            if (IsInsideRemarks(textLine.Type, tokensLine.SourceText)) tokensLine.ScanState.InsideRemarksDirective = true;
            else if (textLine.Type == CobolTextLineType.Source) tokensLine.ScanState.InsideRemarksDirective = false;
            // Try to scan REMARKS compiler directive parameters inside the comment or non-comment line
            if (tokensLine.ScanState.InsideRemarksDirective) {
                string remarksLine = textLine.SourceText;

                if(remarksLine != null) { 
                    int startIndexForSignificantPart = GetStartIndexOfSignificantPart(remarksLine, tokensLine.ScanState);
                    int firstPeriodIndex = remarksLine.IndexOf('.', startIndexForSignificantPart);
                    int endIndexForSignificantPart = GetEndIndexOfSignificantPart(remarksLine, tokensLine.ScanState, firstPeriodIndex);
                    string significantPart = remarksLine.Substring(startIndexForSignificantPart, endIndexForSignificantPart - startIndexForSignificantPart + 1).Trim();

        
                    if (tokensLine.ScanState.InsideRemarksDirective && (remarksLine.Contains(").") || remarksLine.Contains(")"))) {
                        tokensLine.ScanState.LeavingRemarksDirective = true; // indicates the end of the REMARKS compiler directive
                    }

                    RemarksDirective remarksDirective = CreateRemarksDirective(significantPart, tokensLine.ScanState);
                    if (remarksDirective != null && remarksDirective.CopyTextNamesVariations.Count > 0) {
                        // A non empty remarks directive will replace the comment line
                        tokensLine.AddToken(CreateCompilerDirectiveToken(remarksDirective, tokensLine, startIndex, lastIndex, copyTextNameVariations));
                        return;
                    }
                }
            }

#endif

            // Comment line => return only one token with type CommentLine
            // Debug line => treated as a comment line if debugging mode was not activated
            if (textLine.Type == CobolTextLineType.Comment ||
               (textLine.Type == CobolTextLineType.Debug && !tokensLine.InitialScanState.WithDebuggingMode))
            {
                Token commentToken = new Token(TokenType.CommentLine, startIndex, lastIndex, tokensLine);
                tokensLine.AddToken(commentToken);
                return;
            }
            // Invalid indicator, the line type is unknown => the whole line text is handled as a single invalid token
            else if (textLine.Type == CobolTextLineType.Invalid)
            {
                // Invalid indicator => register an error
                tokensLine.AddDiagnostic(MessageCode.InvalidIndicatorCharacter, textLine.Indicator.StartIndex, textLine.Indicator.EndIndex, textLine.Indicator);

                Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lastIndex, tokensLine);
                tokensLine.AddToken(invalidToken);
                return;
            }
            // Empty line => return immediately an empty list of tokens
            // Blank line => return only one token with type SpaceSeparator
            if(textLine.Type == CobolTextLineType.Blank)
            {
                if(!String.IsNullOrEmpty(line))
                {
                    Token whitespaceToken = new Token(TokenType.SpaceSeparator, startIndex, lastIndex, tokensLine);
                    tokensLine.AddToken(whitespaceToken);
                }
                return;
            }            
            
            // Create a stateful line scanner, and iterate over the tokens
            Scanner scanner = new Scanner(line, startIndex, lastIndex, tokensLine, compilerOptions);
            Token nextToken = null;
            while((nextToken = scanner.GetNextToken()) != null)
            {                
                tokensLine.AddToken(nextToken);
            }    
        }

#if EUROINFO_LEGACY_REPLACING_SYNTAX
		private static bool IsInsideRemarks(CobolTextLineType type, string line) {
			if (type != CobolTextLineType.Comment || line == null) return false;
			return line.StartsWith("REMARKS.", StringComparison.InvariantCultureIgnoreCase);
		}
        private static int GetStartIndexOfSignificantPart([NotNull] string line, MultilineScanState state) {
            int start = Math.Max(line.IndexOf(' ') +1, line.IndexOf('=') +1);
            if (!state.InsideRemarksParentheses) {
                int firstLParenIndex = line.IndexOf('(');
                state.InsideRemarksParentheses = (firstLParenIndex >= 0);
                start = Math.Max(start, firstLParenIndex +1);
            }
            return start;
        }
        private static int GetEndIndexOfSignificantPart(string line, MultilineScanState state, int firstPeriodIndex) {
            int end = line.Length -1;
            if (state.InsideRemarksParentheses) {
                int firstRParenIndex = line.IndexOf(')');
                if (firstRParenIndex >= 0) {
                    end = firstRParenIndex -1;
                    state.InsideRemarksParentheses = false;
                }
                if (firstPeriodIndex >= 0 && firstPeriodIndex < firstRParenIndex)
                    end = firstPeriodIndex - 1;
            }
            return end;
        }
        private static RemarksDirective CreateRemarksDirective(string significantPart, MultilineScanState state) {
            if (significantPart.Length < 1) return null;
            var remarksDirective = new RemarksDirective();
            foreach (string candidateName in significantPart.Split(' ')) {
                if (candidateName.Length >= 7) {
                    RemarksDirective.TextNameVariation textName = new RemarksDirective.TextNameVariation(candidateName);
                    remarksDirective.CopyTextNamesVariations.Add(textName);
                }
                else if (!String.IsNullOrWhiteSpace(candidateName) && Regex.IsMatch(candidateName, @"^([a-zA-Z0-9]+)$")) {
                    // A string which is not a text name is an error : stop scanning here
                    remarksDirective = null;
                    state.LeavingRemarksDirective = true;
                    break;
                }
            }
            return remarksDirective;
        }
        private static Token CreateCompilerDirectiveToken(RemarksDirective remarksDirective, TokensLine tokensLine, int start, int end, List<RemarksDirective.TextNameVariation> copyTextNameVariations) {

            copyTextNameVariations.AddRange(remarksDirective.CopyTextNamesVariations);
            IList<Token> originalTokens = new List<Token>(1);
            originalTokens.Add(new Token(TokenType.CommentLine, start,end, tokensLine));
            return new CompilerDirectiveToken(remarksDirective, originalTokens, false);
        }
#endif

        /// <summary>
        /// Scan a group of continuation lines when no previous scan state object is available
        /// </summary>
        public static void ScanFirstLineContinuationGroup(IList<TokensLine> continuationLinesGroup, bool insideDataDivision, bool decimalPointIsComma, bool withDebuggingMode, Encoding encodingForAlphanumericLiterals, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {
            MultilineScanState initialScanState = new MultilineScanState(insideDataDivision, decimalPointIsComma, withDebuggingMode, encodingForAlphanumericLiterals);
            ScanTokensLineContinuationGroup(continuationLinesGroup, initialScanState, compilerOptions, copyTextNameVariations);
        }

        /// <summary>
        /// Scan a group of continuation lines
        /// </summary>
        public static void ScanTokensLineContinuationGroup(IList<TokensLine> continuationLinesGroup, MultilineScanState initialScanState, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {
            // p54: Continuation lines
            // Any sentence, entry, clause, or phrase that requires more than one line can be
            // continued in Area B of the next line that is neither a comment line nor a blank line.
            // The line being continued is a continued line; the succeeding lines are continuation
            // lines. 

            // Track the length of text contributed to the continuation text by each individual line
            TextArea[] textAreasForOriginalLinesInConcatenatedLine = new TextArea[continuationLinesGroup.Count];
            int[] startIndexForTextAreasInOriginalLines = new int[continuationLinesGroup.Count];
            int[] offsetForLiteralContinuationInOriginalLines = new int[continuationLinesGroup.Count];

            // Initialize the continuation text with the complete source text of the first line
            TokensLine firstLine = continuationLinesGroup[0];
            string concatenatedLine = null;
            if (firstLine.Type == CobolTextLineType.Source || (firstLine.Type == CobolTextLineType.Debug && initialScanState.WithDebuggingMode))
            {
                concatenatedLine = firstLine.SourceText;
            }
            else
            {
                concatenatedLine = String.Empty;
                Scanner.ScanTokensLine(firstLine, initialScanState, compilerOptions, copyTextNameVariations);
            }
            textAreasForOriginalLinesInConcatenatedLine[0] = new TextArea(TextAreaType.Source, 0, concatenatedLine.Length -1);
            startIndexForTextAreasInOriginalLines[0] = firstLine.Source.StartIndex;
            offsetForLiteralContinuationInOriginalLines[0] = 0;

            // All the following lines are continuation lines
            // => build a character string representing the complete continuation text along the way
            for (int i = 1; i < continuationLinesGroup.Count; i++)
            {
                TokensLine continuationLine = continuationLinesGroup[i];
                int startIndex = continuationLine.Source.StartIndex;
                int lastIndex = continuationLine.Source.EndIndex;
                string line = continuationLine.Text;

                // 1. Match and remove all blank characters at the beginning of the continuation line
                int startOfContinuationIndex = startIndex;
                for (; startOfContinuationIndex <= lastIndex && line[startOfContinuationIndex] == ' '; startOfContinuationIndex++) { }
                if (startOfContinuationIndex > startIndex)
                {
                    Token whitespaceToken = new Token(TokenType.SpaceSeparator, startIndex, startOfContinuationIndex - 1, continuationLine);
                    continuationLine.SourceTokens.Add(whitespaceToken);
                    startIndex = startOfContinuationIndex;
                }
                if (startOfContinuationIndex <= lastIndex)
                {
                    if (startOfContinuationIndex < 4)
                    {
                        continuationLine.AddDiagnostic(MessageCode.AreaAOfContinuationLineMustBeBlank, startOfContinuationIndex, startOfContinuationIndex);
                    }
                }
                else
                {
                    // Nothing but spaces on the continuation line
                    continue;
                }

                // p55: Continuation of alphanumeric and national literals
                // Alphanumeric and national literals can be continued only when there are no DBCS
                // characters in the content of the literal.
                // The following rules apply to alphanumeric and national literals that do not contain
                // DBCS characters:
                // - If the continued line contains an alphanumeric or national literal without a
                //   closing quotation mark, all spaces at the end of the continued line (through
                //   column 72) are considered to be part of the literal. The continuation line must
                //   contain a hyphen in the indicator area, and the first nonblank character must be
                //   a quotation mark. The continuation of the literal begins with the character
                //   immediately following the quotation mark.
                // - If an alphanumeric or national literal that is to be continued on the next line has
                //   as its last character a quotation mark in column 72, the continuation line must
                //   start with two consecutive quotation marks. This will result in a single quotation
                //   mark as part of the value of the literal.
                // - If the last character on the continued line of an alphanumeric or national literal
                //   is a single quotation mark in Area B, the continuation line can start with a single
                //   quotation mark. This will result in two consecutive literals instead of one
                //   continued literal.
                // The rules are the same when an apostrophe is used instead of a quotation mark in
                // delimiters.                
                // ... p55 -> p56: examples of continuations and expected behavior ...

                int offsetForLiteralContinuation = 0;
                if (concatenatedLine.Length > 0)
                {
                    // Scan the continuation text, and get its last token so far
                    TokensLine temporaryTokensLine = TokensLine.CreateVirtualLineForInsertedToken(firstLine.InitialLineIndex, concatenatedLine);
                    Scanner.ScanTokensLine(temporaryTokensLine, initialScanState, compilerOptions, copyTextNameVariations);
                    Token lastTokenOfConcatenatedLineSoFar = temporaryTokensLine.SourceTokens[temporaryTokensLine.SourceTokens.Count - 1];

                    // Check if the last token so far is an alphanumeric or national literal
                    if (lastTokenOfConcatenatedLineSoFar.TokenFamily == TokenFamily.AlphanumericLiteral)
                    {
                        // The continuation line must contain a hyphen in the indicator area, and the first nonblank character must be a quotation mark
                        if (line[startOfContinuationIndex] != lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter)
                        {
                            continuationLine.AddDiagnostic(MessageCode.InvalidFirstCharForContinuationLine, startOfContinuationIndex, startOfContinuationIndex, lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter);
                        }
                        // The continuation of the literal begins with the character immediately following the quotation mark.
                        else
                        {
                            offsetForLiteralContinuation = 1;

                            // If an alphanumeric literal that is to be continued on the next line has as its last character a quotation mark in column 72, 
                            // the continuation line must start with two consecutive quotation marks.
                            if (lastTokenOfConcatenatedLineSoFar.HasClosingDelimiter)
                            {
                                if ((startOfContinuationIndex + 1) > lastIndex || line[startOfContinuationIndex + 1] != lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter)
                                {
                                    continuationLine.AddDiagnostic(MessageCode.InvalidFirstTwoCharsForContinuationLine, startOfContinuationIndex, startOfContinuationIndex + 1, lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter);
                                    // Use the first quotation mark to avoid a complete mess while scanning the rest of the line
                                    offsetForLiteralContinuation = 0;
                                }
                            }
                        }
                    }
                    // Check if the last token so far is a floating comment 
                    else if (lastTokenOfConcatenatedLineSoFar.TokenType == TokenType.FloatingComment)
                    {
                        // => remove the floating comment from the text of the continuation
                        concatenatedLine = concatenatedLine.Substring(0, concatenatedLine.Length - lastTokenOfConcatenatedLineSoFar.Length);
                        textAreasForOriginalLinesInConcatenatedLine[i - 1] = new TextArea(TextAreaType.Source, textAreasForOriginalLinesInConcatenatedLine[i - 1].StartIndex, textAreasForOriginalLinesInConcatenatedLine[i - 1].EndIndex - lastTokenOfConcatenatedLineSoFar.Length);
                        TokensLine lineWithFloatingComment = continuationLinesGroup[i - 1];
                        Token floatingCommentToken = new Token(TokenType.FloatingComment, lineWithFloatingComment.Length - lastTokenOfConcatenatedLineSoFar.Length, lineWithFloatingComment.Length - 1, lineWithFloatingComment);
                        lineWithFloatingComment.SourceTokens.Add(floatingCommentToken);
                    }
                    // Check if the last token so far is a comment entry
                    else if (lastTokenOfConcatenatedLineSoFar.TokenType == TokenType.CommentEntry)
                    {
                        // p105: A hyphen in the indicator area (column 7) is not permitted in comment - entries.
                        // => impossible to ignore the continuation indicator here, it is too late 
                        //    (we can not know there is a comment entry before scanning the continuation lines groups)
                        // => register an error message
                        continuationLine.AddDiagnostic(MessageCode.HyphenIndicatorNotPermittedInCommenEntries, continuationLine.Indicator.StartIndex + 1, continuationLine.Indicator.EndIndex + 1);
                    }
                }

                // p54: If there is no hyphen (-) in the indicator area (column 7) of a line, the last character
                // of the preceding line is assumed to be followed by a space.
                // If there is a hyphen in the indicator area of a line, the first nonblank character of
                // the continuation line immediately follows the last nonblank character of the
                // continued line without an intervening space.

                // Concatenate the continuation text so far with the text of the current continuation line
                int startIndexOfContinuationStringInContinuationLine = startOfContinuationIndex + offsetForLiteralContinuation;
                int lengthOfContinuationStringInContinuationLine = lastIndex - startIndexOfContinuationStringInContinuationLine + 1;                
                
                textAreasForOriginalLinesInConcatenatedLine[i] = new TextArea(TextAreaType.Source, concatenatedLine.Length, concatenatedLine.Length + lengthOfContinuationStringInContinuationLine - 1);
                startIndexForTextAreasInOriginalLines[i] = startIndexOfContinuationStringInContinuationLine;
                offsetForLiteralContinuationInOriginalLines[i] = offsetForLiteralContinuation;

                concatenatedLine += line.Substring(startIndexOfContinuationStringInContinuationLine, lengthOfContinuationStringInContinuationLine);
            }

            // Scan the complete continuation text as a whole
            TokensLine virtualContinuationTokensLine = TokensLine.CreateVirtualLineForInsertedToken(firstLine.InitialLineIndex, concatenatedLine);
            Scanner.ScanTokensLine(virtualContinuationTokensLine, initialScanState, compilerOptions, copyTextNameVariations);

            // Then attribute each token and diagnostic to its corresponding tokens line
            MultilineScanState scanState = initialScanState;
            for (int i = 0; i < continuationLinesGroup.Count; i++)
            {
                TokensLine originalLine = continuationLinesGroup[i];
                originalLine.InitializeScanState(scanState);

                TextArea textAreaForOriginalLine = textAreasForOriginalLinesInConcatenatedLine[i];
                int concatenatedLineToOriginalLineOffset = startIndexForTextAreasInOriginalLines[i] - textAreaForOriginalLine.StartIndex;
                
                foreach (Token token in virtualContinuationTokensLine.SourceTokens)
                {
                    // Token located after the current line
                    if(token.StartIndex > textAreaForOriginalLine.EndIndex)
                    {
                        break;
                    }
                    // Token located before the current line
                    else if(token.StopIndex < textAreaForOriginalLine.StartIndex)
                    {
                        continue;
                    }
                    // Token completely completely included inside the current line
                    else if(token.StartIndex >= textAreaForOriginalLine.StartIndex && token.StopIndex <= textAreaForOriginalLine.EndIndex)
                    {                        
                        int startIndexInOriginalLine = token.StartIndex + concatenatedLineToOriginalLineOffset;
                        int stopIndexInOriginalLine = token.StopIndex + concatenatedLineToOriginalLineOffset;

                        token.CorrectTokensLine(originalLine, startIndexInOriginalLine, stopIndexInOriginalLine);
                        originalLine.AddToken(token);

                        foreach(Diagnostic diag in virtualContinuationTokensLine.GetDiagnosticsForToken(token))
                        {
                            originalLine.AddDiagnostic((MessageCode)diag.Info.Code, token, diag.MessageArgs);
                        }
                    }
                    // Multiline continuation token only partially located on this line
                    else
                    {
                        bool isContinuationFromPreviousLine = token.StartIndex < textAreaForOriginalLine.StartIndex;
                        bool isContinuedOnNextLine = token.StopIndex > textAreaForOriginalLine.EndIndex;

                        int startIndexInOriginalLine = 0; 
                        if (isContinuationFromPreviousLine)
                        {
                            startIndexInOriginalLine = startIndexForTextAreasInOriginalLines[i] - offsetForLiteralContinuationInOriginalLines[i];
                        }
                        else
                        {
                            startIndexInOriginalLine = token.StartIndex + concatenatedLineToOriginalLineOffset;
                        }
                        int stopIndexInOriginalLine = 0;
                        if (isContinuedOnNextLine)
                        {
                            stopIndexInOriginalLine = originalLine.Source.EndIndex;
                            // If a continued line ends with a floating comment, the continued token ends just before the floating comment
                            if (originalLine.SourceTokens.Count > 0 && originalLine.SourceTokens[originalLine.SourceTokens.Count - 1].TokenType == TokenType.FloatingComment)
                            {
                                stopIndexInOriginalLine -= originalLine.SourceTokens[originalLine.SourceTokens.Count - 1].Length;
                            }
                        }
                        else
                        {
                            stopIndexInOriginalLine = token.StopIndex + concatenatedLineToOriginalLineOffset;
                        }

                        ContinuationToken continuationToken = new ContinuationToken(token, startIndexInOriginalLine, stopIndexInOriginalLine, 
                            originalLine, isContinuationFromPreviousLine, isContinuedOnNextLine);
                        originalLine.AddToken(continuationToken);                        

                        // Copy diagnostics on the first line only
                        if(!isContinuationFromPreviousLine)
                        {
                            foreach (Diagnostic diag in virtualContinuationTokensLine.GetDiagnosticsForToken(token))
                            {
                                originalLine.AddDiagnostic((MessageCode)diag.Info.Code, token, diag.MessageArgs);
                            }
                        }
                    }
                }

                scanState = originalLine.ScanState;
            }
        }

        /// <summary>
        /// Scan an isolated token in the following "default" context :
        /// - insideDataDivision = true
        /// - decimalPointIsComma = false
        /// - withDebuggingMode = false
        /// - encodingForAlphanumericLiterals = IBM 1147
        /// - default compiler options
        /// </summary>
        public static Token ScanIsolatedTokenInDefaultContext(string tokenText, out Diagnostic error)
        {
            TokensLine tempTokensLine = TokensLine.CreateVirtualLineForInsertedToken(0, tokenText);
            tempTokensLine.InitializeScanState(new MultilineScanState(true, false, false, IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147)));

            Scanner tempScanner = new Scanner(tokenText, 0, tokenText.Length - 1, tempTokensLine, new TypeCobolOptions(), false);
            Token candidateToken = tempScanner.GetNextToken();

            if(tempTokensLine.ScannerDiagnostics.Count > 0)
            {
                error = tempTokensLine.ScannerDiagnostics[0];
            }
            else
            {
                error = null;
            }
            return candidateToken;
        }

        // --- State machine ---

        private TokensLine tokensLine;
        private string line;
        private int currentIndex;
        private int lastIndex;

        private TypeCobolOptions compilerOptions;

        private Scanner(string line, int startIndex, int lastIndex, TokensLine tokensLine, TypeCobolOptions compilerOptions, bool beSmartWithLevelNumber = true)
        {
            this.tokensLine = tokensLine;
            this.line = line;
            this.currentIndex = startIndex;
            this.lastIndex = lastIndex;

            this.compilerOptions = compilerOptions;

            this.BeSmartWithLevelNumber = beSmartWithLevelNumber;
        }

        private Token GetNextToken()
        {
            // Cannot read past end of line
            if(currentIndex > lastIndex)
            {
                return null;
            }

            // Start scanning at the current index
            int startIndex = currentIndex;
            MultilineScanState currentState = tokensLine.ScanState;

            //  -- Special case 1 : Comment Entries in the IDENTIFICATION DIVISION --

            bool tryScanCommentEntry = false;
            // First token after a comment entry keyword
            if (currentState.AfterCommentEntryKeyword)
            {
                switch (line[startIndex])
                {
                    case ' ':
                        return ScanWhitespace(startIndex);
                    case '.':
                        return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PeriodSeparator, MessageCode.InvalidCharAfterPeriod);
                    default:
                        tryScanCommentEntry = true;
                        break;
                }
            }
            // First token after a comment entry keyword and one period separator
            else if (currentState.AfterCommentEntryKeywordPeriod)
            {
                switch (line[startIndex])
                {
                    case ' ':
                        return ScanWhitespace(startIndex);
                    default:
                        tryScanCommentEntry = true;
                        break;
                }
            }
            // New token after a previous comment entry
            else if (currentState.AfterCommentEntry)
            {
                tryScanCommentEntry = true;
            }
            // Previous state tests show that we should try to scan a comment entry at this point
            if (tryScanCommentEntry)
            {
                // p105 : The comment-entry in any of the optional paragraphs can be any combination of
                // characters from the character set of the computer. The comment-entry is written in
                // Area B on one or more lines.
                // ==> a comment entry is delimited only by characters in area A on the next line

                // Find first non whitespace char
                int firstCharIndex = startIndex;
                for (; firstCharIndex <= lastIndex && line[firstCharIndex] == ' '; firstCharIndex++) { }
                // Check if it starts in area A
                bool nextTokensStartsInAreaA = line[firstCharIndex] != ' ' && firstCharIndex < (tokensLine.Source.StartIndex + 4);

                // A comment entry can't start in area A
                if(!nextTokensStartsInAreaA)
                {
                    return ScanCommentEntry(startIndex);
                }
                // => if it is not a comment entry, continue below
            }

            // -- Special case 2 : Exec Statement and ExecStatementText --

            bool tryScanExecStatementText = false;
            // First token after EXEC or EXECUTE
            if(currentState.AfterExec)
            {
                switch (line[startIndex])
                {
                    case ' ':
                        return ScanWhitespace(startIndex);
                    default:
                        return ScanExecTranslatorName(startIndex);
                }
            }
            // First token after ExecTranslatorName
            else if (currentState.AfterExecTranslatorName)
            {
                switch (line[startIndex])
                {
                    case ' ':
                        return ScanWhitespace(startIndex);
                    default:
                        tryScanExecStatementText = true;
                        break;
                }
            }
            // New token after a previous ExecStatementText
            else if (currentState.AfterExecStatementText)
            {
                tryScanExecStatementText = true;
            }
            // Previous state tests show that we should try to scan an exec statement text at this point
            if (tryScanExecStatementText)
            {
                return ScanExecStatementTextOrExecSqlInclude(startIndex);
            }

            // -- Special case 3 : PictureCharacterString --

            bool tryScanPictureCharacterString = false;
            // First token after PIC or PICTURE keyword IS?
            if (currentState.AfterPicture)
            {
                if (line[startIndex] == ' ')
                {
                    return ScanWhitespace(startIndex);
                }
                else
                {
                    tryScanPictureCharacterString = true;
                }
            }
            // Previous state tests show that we should try to scan a picture character string at this point
            if (tryScanPictureCharacterString)
            {
                return ScanPictureCharacterStringOrISOrSYMBOL(startIndex);
            }

            // --- Main switch ---

            switch (line[startIndex])
            {
                case ' ':
                    //SpaceSeparator=1,
                    // p45: Anywhere a space is used as a separator or as part of a
                    // separator, more than one space can be used.
                    return ScanWhitespace(startIndex);
                case ',':
                    //CommaSeparator=2,
                    // p46: A separator comma is composed of a comma followed by a space. 
                    if (tokensLine.ScanState.DecimalPointIsComma)
                    {
                        //IntegerLiteral = 27,
                        //DecimalLiteral = 28,
                        //FloatingPointLiteral = 29,
                        return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.CommaSeparator, MessageCode.InvalidCharAfterComma, false); 
                    }
                    else
                    {
                        return ScanOneCharFollowedBySpace(startIndex, TokenType.CommaSeparator, MessageCode.InvalidCharAfterComma, false);
                    }
                case ';':
                    //SemicolonSeparator=3,
                    // p46: A separator semicolon is composed of a semicolon followed by a space.
                    return ScanOneCharFollowedBySpace(startIndex, TokenType.SemicolonSeparator, MessageCode.SemicolonShouldBeFollowedBySpace);                    
                case '*':
                    //MultiplyOperator=14,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    if (currentIndex == lastIndex)
                    {
                        // consume the * char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.MultiplyOperator, startIndex, startIndex, true, tokensLine);
                    }
                    else if(line[currentIndex + 1] == ' ')
                    {
                        // consume the * char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.MultiplyOperator, startIndex, startIndex + 1, tokensLine);
                    }
                    //PowerOperator=15,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    else if (line[currentIndex + 1] == '*')
                    {
                        // consume the first * char
                        currentIndex++;
                        // scan the second * char and a space
                        return ScanOneCharWithPossibleSpaceAfter(startIndex, TokenType.PowerOperator);
                    }
                    //FloatingComment=5,                    
                    else if (line[currentIndex + 1] == '>')
                    {
                        return ScanFloatingComment(startIndex);
                    }
                    // ASTERISK_CBL = "*CBL"
                    else if ((line[currentIndex + 1] == 'C' || line[currentIndex + 1] == 'c') && 
                        (currentIndex + 3 <= lastIndex && line.Substring(currentIndex, 4).Equals("*CBL", StringComparison.OrdinalIgnoreCase)) &&
                        (currentIndex + 3 == lastIndex || CobolChar.IsCobolWordSeparator(line[currentIndex + 4])))
                    {
                        // match 4 chars
                        currentIndex += 4;
                        return new Token(TokenType.ASTERISK_CBL, startIndex, startIndex + 3, tokensLine);
                    }
                    // ASTERISK_CONTROL = "*CONTROL"
                    else if ((line[currentIndex + 1] == 'C' || line[currentIndex + 1] == 'c') &&
                        (currentIndex + 7 <= lastIndex && line.Substring(currentIndex, 8).Equals("*CONTROL", StringComparison.OrdinalIgnoreCase)) &&
                        (currentIndex + 7 == lastIndex || CobolChar.IsCobolWordSeparator(line[currentIndex + 8])))
                    {
                        // match 8 chars                       
                        currentIndex += 8;
                        return new Token(TokenType.ASTERISK_CONTROL, startIndex, startIndex + 7, tokensLine);
                    }
                    else
                    {
                        // consume * char and try to match it as a multiply operator
                        currentIndex++;
                        return new Token(TokenType.MultiplyOperator, startIndex, startIndex, tokensLine);
                    }
                case '.':
                    //PeriodSeparator=7,
                    // p46: A separator period is composed of a period followed by a space.
                    if(tokensLine.ScanState.DecimalPointIsComma)
                    {
                        return ScanOneCharFollowedBySpace(startIndex, TokenType.PeriodSeparator, MessageCode.InvalidCharAfterPeriod);
                    }
                    else
                    {
                        //IntegerLiteral = 27,
                        //DecimalLiteral = 28,
                        //FloatingPointLiteral = 29,
                        return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PeriodSeparator, MessageCode.InvalidCharAfterPeriod);
                    }
                case ':':
                    // -- TypeCobol specific syntax --
                    // QualifiedNameSeparator => qualifierName::qualifiedName
                    if (currentIndex < lastIndex && line[currentIndex + 1] == ':')
                    {
                        // consume two :: chars
                        AnalyticsWrapper.Telemetry.TrackEvent("[Operator-Used] ::");
                        currentIndex += 2;
                        return new Token(TokenType.QualifiedNameSeparator, startIndex, startIndex + 1, tokensLine);
                    }
                    // --
                    // The COPY statement with REPLACING phrase can be used to replace parts of words. 
                    // By inserting a dummy operand delimited by colons into the program text, the compiler will replace the dummy operand with the desired text. 
                    int patternEndIndex;
                    if (CheckForPartialCobolWordPattern(startIndex, out patternEndIndex))
                    {
                        return ScanPartialCobolWord(startIndex, patternEndIndex);
                    }
                    //ColonSeparator=8,
                    // p46: Colon { : } The colon is a separator and is required when shown in general formats.
                    // consume the : char
                    else
                    {
                        return ScanOneChar(startIndex, TokenType.ColonSeparator);
                    }
                case '(':
                    //LeftParenthesisSeparator=9,
                    // consume the ( char
                    return ScanOneChar(startIndex, TokenType.LeftParenthesisSeparator);
                case ')':
                    //RightParenthesisSeparator=10,
                    return ScanOneChar(startIndex, TokenType.RightParenthesisSeparator);
                case '+':
                    //PlusOperator=11,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    //IntegerLiteral = 27,
                    //DecimalLiteral = 28,
                    //FloatingPointLiteral = 29,
                    return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PlusOperator, MessageCode.ImplementationError, false);
                case '-':
                    //MinusOperator=12,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    //IntegerLiteral = 27,
                    //DecimalLiteral = 28,
                    //FloatingPointLiteral = 29,
                    return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.MinusOperator, MessageCode.ImplementationError, false);
                case '/':
                    //DivideOperator=13,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    return ScanOneCharWithPossibleSpaceAfter(startIndex, TokenType.DivideOperator);
                case '<':
                    //LessThanOperator=16,
                    //LessThanOrEqualOperator=18,
                    // p260: Each relational operator must be preceded and followed
                    // by a space. 
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    if (currentIndex == lastIndex)
                    {
                        // consume the < char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.LessThanOperator, startIndex, startIndex, true, tokensLine);
                    }
                    else if (line[currentIndex + 1] == ' ')
                    {
                        // consume the < char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.LessThanOperator, startIndex, startIndex + 1, tokensLine);
                    }
                    else if (line[currentIndex + 1] == '=')
                    {
                        // consume the < char
                        currentIndex++;
                        // scan the = char and a space
                        return ScanOneCharWithPossibleSpaceAfter(startIndex, TokenType.LessThanOrEqualOperator);
                    } else {
                        // consume < char and try to match it as a less than operator
                        currentIndex++;
                        return new Token(TokenType.LessThanOperator, startIndex, startIndex, tokensLine);
                    }
                case '>':
                    //GreaterThanOperator=17,
                    //GreaterThanOrEqualOperator=19,
                    // p260: Each relational operator must be preceded and followed
                    // by a space. 
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    if (currentIndex == lastIndex)
                    {
                        // consume the > char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.GreaterThanOperator, startIndex, startIndex, true, tokensLine);
                    }
                    else if (line[currentIndex + 1] == ' ')
                    {
                        // consume the > char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.GreaterThanOperator, startIndex, startIndex + 1, tokensLine);
                    }
                    else if (line[currentIndex + 1] == '=')
                    {
                        // consume the > char
                        currentIndex++;
                        // scan the = char and a space
                        return ScanOneCharWithPossibleSpaceAfter(startIndex, TokenType.GreaterThanOrEqualOperator);
                    } else {
                        // consume > char and try to match it as a greater than operator
                        currentIndex++;
                        return new Token(TokenType.GreaterThanOperator, startIndex, startIndex, tokensLine);
                    }
                case '=':
                    //EqualOperator=20,
                    // p260: Each relational operator must be preceded and followed
                    // by a space. 
                    //However IBM Z/OS only raise a warning if there is no space - Issue #430
                    if (currentIndex == lastIndex)
                    {
                        // consume the = char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.EqualOperator, startIndex, startIndex, true, tokensLine);
                    }
                    else if (line[currentIndex + 1] == ' ')
                    {
                        // consume the = char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.EqualOperator, startIndex, startIndex + 1, tokensLine);
                    }
                    //PseudoTextDelimiter = 11,                    
                    else if (line[currentIndex + 1] == '=')
                    {
                        // p47: Pseudo-text delimiters {b==} ... {==b}
                        // An opening pseudo-text delimiter must be immediately preceded by a
                        // space. A closing pseudo-text delimiter must be immediately followed by a
                        // separator space, comma, semicolon, or period. Pseudo-text delimiters must
                        // appear as balanced pairs. They delimit pseudo-text. (See “COPY statement”
                        // on page 530.)
                        
                        // consume both == chars
                        currentIndex += 2;

                        Token delimiterToken = null;
                        // Case 1. Opening delimiter
                        if (!tokensLine.ScanState.InsidePseudoText)
                        {
                            delimiterToken = new Token(TokenType.PseudoTextDelimiter, startIndex, startIndex + 1, tokensLine);
                        }
                        // Case 2. Closing delimiter
                        else
                        {
                            // get the immediately following char
                            char followingChar;
                            bool usesVirtualSpaceAtEndOfLine = false;
                            if(currentIndex > lastIndex)
                            {
                                followingChar = ' ';
                                usesVirtualSpaceAtEndOfLine = true;
                            }
                            else
                            {
                                followingChar = line[currentIndex];
                            }

                            delimiterToken = new Token(TokenType.PseudoTextDelimiter, startIndex, startIndex + 1, usesVirtualSpaceAtEndOfLine, tokensLine);
                            if (!(followingChar == ' ' || followingChar == ',' || followingChar == ';' || followingChar == '.'))
                            {
                                tokensLine.AddDiagnostic(MessageCode.InvalidCharAfterPseudoTextDelimiter, delimiterToken);
                            }
                        }
                        return delimiterToken;
                    }
                    else
                    {
                        // consume = char and try to match it as an equal operator
                        currentIndex++;
                        return new Token(TokenType.EqualOperator, startIndex, startIndex, tokensLine);
                    }
                case '"':
                case '\'':
                    //AlphanumericLiteral = 21,
                    return ScanAlphanumericLiteral(startIndex, TokenType.AlphanumericLiteral);
                case 'X':
                case 'x':
                    //HexadecimalAlphanumericLiteral = 22,
                    // p45: X" Opening delimiter for a hexadecimal format alphanumeric literal
                    // X’ Opening delimiter for a hexadecimal format alphanumeric literal
                    if (currentIndex < lastIndex && (line[currentIndex + 1] == '"' || line[currentIndex + 1] == '\''))
                    {
                        // consume X char
                        currentIndex++;
                        return ScanAlphanumericLiteral(startIndex, TokenType.HexadecimalAlphanumericLiteral);
                    }
                    else
                    {
                        return ScanCharacterString(startIndex);
                    }
                case 'Z':
                case 'z':
                    //NullTerminatedAlphanumericLiteral = 23,
                    // p46: Null-terminated literal delimiters {Z"} ... {"}, {Z’} ... {’}
                    if (currentIndex < lastIndex && (line[currentIndex + 1] == '"' || line[currentIndex + 1] == '\''))
                    {
                        // consume Z char
                        currentIndex++;
                        return ScanAlphanumericLiteral(startIndex, TokenType.NullTerminatedAlphanumericLiteral);
                    }
                    else
                    {
                        return ScanCharacterString(startIndex);
                    }
                case 'N':
                case 'n':
                    //NationalLiteral = 24,
                    //HexadecimalNationalLiteral = 25,
                    // p46: National literal delimiters {N"} ... {"}, {N’} ... {’}, {NX"} ... {"}, {NX’} ... {’}
                    if (currentIndex < lastIndex && (line[currentIndex + 1] == '"' || line[currentIndex + 1] == '\''))
                    {
                        // consume N char
                        currentIndex++;
                        return ScanAlphanumericLiteral(startIndex, TokenType.NationalLiteral);
                    }
                    else if (currentIndex < lastIndex     && (line[currentIndex + 1] == 'X' || line[currentIndex + 1] == 'x') &&
                             currentIndex < (lastIndex+1) && (line[currentIndex + 2] == '"' || line[currentIndex + 2] == '\''))
                    {
                        // consume N and X chars
                        currentIndex += 2;
                        return ScanAlphanumericLiteral(startIndex, TokenType.HexadecimalNationalLiteral);
                    }
                    else
                    {
                        return ScanCharacterString(startIndex);
                    }
                case 'G':
                case 'g':
                    //DBCSLiteral = 26,
                    // p46: DBCS literal delimiters {G"} ... {"}, {G’} ... {’}
                    if (currentIndex < lastIndex && (line[currentIndex + 1] == '"' || line[currentIndex + 1] == '\''))
                    {
                        // consume G char
                        currentIndex++;
                        return ScanAlphanumericLiteral(startIndex, TokenType.DBCSLiteral);
                    }
                    else
                    {
                        return ScanCharacterString(startIndex);
                    }

                // p9: COBOL words with single-byte characters
                // A COBOL word is a character-string that forms a user-defined word, a system-name, or a reserved word. 
                // Each character of a COBOL word is selected from the following set: 
                // - Latin uppercase letters A through Z, latin lowercase letters a through z 
                // - digits 0 through 9 
                // - (hyphen) - , (underscore) _
                // The hyphen cannot appear as the first or last character in such words.
                // The underscore cannot appear as the first character in such words.
                // Most user - defined words(all except section - names, paragraph - names) must contain at least one alphabetic character.

                // PROBLEMS : 
                // 123 is a valid user defined word (for section & paragraph names) AND a valid numeric literal.
                // 123E-4 is a valid user defined word (for any type of name) AND a valid numeric literal (floating point format).
                // 123-456 is a valid user defined word (fol section & paragraph names), AND it could be interpreted as two numeric literals,
                //   NB: it is NOT a valid subtraction (minus must be preceded and followedf by space) but we would like to display a nice error 
                //   message informing the user that these spaces are mandatory, because a subtraction was most likey intended in this case.
                // 000010-000050 should be interpreted as a range of numbers (indicated by separating the two bounding numbers of the range 
                //   by a hyphen) in sequence-number-fields of compiler directive statements (ex: DELETE).

                // CURRENT behavior of method ScanNumericLiteral :
                // This method matches chars as long as they are characters allowed in a numeric literal.
                // Then, it checks the format of the matched string, and returns either a NumericLiteral or Invalid token.
                // If we write 123ABC, ScanNumericLiteral will match only 123, return a perfectly valid numeric literal,
                // and place the currentIndex to match the next token on the char A.

                // PROPOSED SOLUTION :
                // * in the Scanner :
                // If a token is starting with a digit, we first try to scan it as a numeric literal (most common case).
                // Then we check if the character directly following the numeric literal is a valid character for a user defined word.
                // We also check as a special case if this character is not '-', followed by a digit or an invalid char, because we need 
                // to interpret 123-456 as two numeric literals without separator (notably for range of numbers)and 123- is not valid.
                // If it is not valid (space, separator ...), we simply return the numeric literal token.
                // If it is valid, we reset the state of the scanner and try to scan this word as a character string 
                // (keyword, user defined word ...).
                // * in the Grammar :
                // We must allow numeric literal tokens (in addition to user defind words) in section and paragraph name rules.

                // => additional PROBLEM after test : 
                // 123. is already matched by the grammar in the reference documentation as a valid dataDescriptionEntry.
                // But according to the same spec, 123. is also a valid paragraphHeader.
                // TO DO : check which one of the two alternatives must be favored in the real world ?
                // In the meantime, nothing was changed in the grammar file : purely numeric paragraph identifiers are not supported.

                // LIMITATIONS :
                // User defined words of the form 123E-4 or 123-4X are valid according to the spec but will not be supported 
                // by this compiler. 
                // These cases are considered highly improbable, but we will have to check on a large body of existing programs.
                // Purely numeric aragraph and section names are not supported.

                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    
                    // 1. First try to scan a numeric literal
                    //IntegerLiteral = 27,
                    //DecimalLiteral = 28,
                    //FloatingPointLiteral = 29,
                    int saveCurrentIndex = currentIndex;
                    Token numericLiteralToken = ScanNumericLiteral(startIndex);

                    // 2. Then check to see if the next char would be valid inside a CobolWord
                    bool nextCharIsACobolWordChar = (currentIndex <= lastIndex) && CobolChar.IsCobolWordChar(line[currentIndex]);
                    if(nextCharIsACobolWordChar && line[currentIndex] == '-')
                    {
                        nextCharIsACobolWordChar = nextCharIsACobolWordChar && currentIndex < lastIndex
                            && CobolChar.IsCobolWordChar(line[currentIndex + 1])
                            && !Char.IsDigit(line[currentIndex + 1]);
                    }

                    // 3.1. Return a numeric literal token
                    if (!nextCharIsACobolWordChar)
                    {
                        return numericLiteralToken;
                    }
                    else
                    {
                        // Reset scanner state
                        currentIndex = saveCurrentIndex;
                        if (numericLiteralToken.TokenType == TokenType.InvalidToken)
                        {
                            tokensLine.RemoveDiagnosticsForToken(numericLiteralToken);
                        }

                        // 3.2 Try to scan a Cobol character string
                        //UserDefinedWord = 36,
                        return ScanCharacterString(startIndex);
                    }
                default:
                    //UserDefinedWord = 36,
                    return ScanCharacterString(startIndex);
            }
        }

        // --- Implementation note : Context-sensistive scanner operations ---

        // (PICTURE | PIC) -> SYMBOL -> (nothing)
        //                 -> IS? -> pictureCharacterString

        // (AUTHOR | INSTALLATION | DATE_WRITTEN | DATE_COMPILED | SECURITY) -> PeriodSeparator? -> CommentEntry*

        // FUNCTION -> TCFunctionName

        // (EXEC | EXECUTE) -> ExecTranslatorName -> ExecStatementText -> END_EXEC
        //                  -> (SQL | SQLIMS)     -> INCLUDE =rw=> EXEC_SQL_INCLUDE

        // DELETE -> UserDefinedWord =ok=> DELETE
        //        -> IntegerLiteral  =rw=> DELETE_CD

        // DATA -> DIVISION -> ... DISPLAY =rw=> DISPLAY_ARG
        //      -> (RECORD | RECORDS) -> (nothing)
        // PROCEDURE -> DIVISION -> ... DISPLAY =ok=> DISPLAY
        //           -> POINTER -> (nothing)

        // TO   -> ENTRY =rw=> ENTRY_ARG
        // SAME -> SORT  =rw=> SORT_ARG

        // SYMBOLIC CHARACTERS? (SymbolicCharacter+ (ARE|IS)? IntegerLiteral+)+ (IN alphabetName)?
        // ... -> UserDefinedWord (environmentNameClause)
        // ... -> ALPHABET | CLASS | CURRENCY | DECIMAL_POINT | XML_SCHEMA | PeriodSeparator | REPOSITORY | INPUT_OUTPUT | DATA
        
        // ---

        private Token ScanWhitespace(int startIndex)
        {
            // consume all whitespace chars available
            for (; currentIndex <= lastIndex && line[currentIndex] == ' '; currentIndex++) { }
            int endIndex = currentIndex - 1;
            return new Token(TokenType.SpaceSeparator, startIndex, endIndex, tokensLine);
        }

        private Token ScanOneChar(int startIndex, TokenType tokenType)
        {
            // consume one char
            currentIndex++;
            return new Token(tokenType, startIndex, startIndex, tokensLine);
        }

        private Token ScanOneCharWithPossibleSpaceAfter(int startIndex, TokenType tokenType) {
            //Use MessageCode.ImplementationError because ScanOneCharFollowedBySpace must not create an error
            return ScanOneCharFollowedBySpace(startIndex, tokenType, MessageCode.ImplementationError, false);
        }

    private Token ScanOneCharFollowedBySpace(int startIndex, TokenType tokenType, MessageCode messageCode, bool spaceAfterisMandatory =true)
        {
            if (currentIndex == lastIndex)
            {
                // consume one char and use the virtual space at end of line
                currentIndex++;
                return new Token(tokenType, startIndex, currentIndex - 1, true, tokensLine);
            }
            else if (line[currentIndex + 1] == ' ')
            {
                // consume one char and consume the space char
                currentIndex += 2;
                return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
            }
            else
            {
                // consume one char and register an error because the following char is missing
                // even if the space is missing, try to match the expected tokenType
                currentIndex++;
                if (spaceAfterisMandatory) {
                    Token invalidToken = new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
                    tokensLine.AddDiagnostic(messageCode, invalidToken);
                    return invalidToken;
                }
                return new Token(tokenType, startIndex, currentIndex-1, tokensLine);
            }
        }

        private Token ScanOneCharFollowedBySpaceOrNumericLiteral(int startIndex, TokenType tokenType, MessageCode messageCode, bool spaceAfterIsMandatory = true)
        {
            if (currentIndex == lastIndex)
            {
                // consume one char and use the virtual space at end of line
                currentIndex++;
                return new Token(tokenType, startIndex, currentIndex - 1, true, tokensLine);
            }
            else if (line[currentIndex + 1] == ' ')
            {
                // consume one char and consume the space char
                currentIndex += 2;
                return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
            }
            else if (Char.IsDigit(line[currentIndex + 1]))
            {
                return ScanNumericLiteral(startIndex);
            }
            else if((tokenType == TokenType.PlusOperator || tokenType == TokenType.MinusOperator) && 
                    line[currentIndex + 1] == (tokensLine.ScanState.DecimalPointIsComma?',':'.'))
            {
                return ScanNumericLiteral(startIndex);
            }
            else
            {
                // consume one char and register an error because the following char is missing
                // even if the space is missing, try to match the expected tokenType
                currentIndex++;
                if (spaceAfterIsMandatory) {
                    Token invalidToken = new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
                    tokensLine.AddDiagnostic(messageCode, invalidToken);
                    return invalidToken;
                }
                return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
            }
        }

        private Token ScanFloatingComment(int startIndex)
        {
            // p57 : Floating comment indicators (*>)
            // In addition to the fixed indicators that can only be specified in the indicator area of
            // the source reference format, a floating comment indicator (*>) can be specified
            // anywhere in the program-text area to indicate a comment line or an inline
            // comment.
            // A floating comment indicator indicates a comment line if it is the first character
            // string in the program-text area (Area A plus Area B), or indicates an inline
            // comment if it is after one or more character strings in the program-text area.
            // These are the rules for floating comment indicators:
            // - Both characters (* and >) that form the multiple-character floating indicator must
            //   be contiguous and on the same line.
            // - The floating comment indicator for an inline comment must be preceded by a
            //   separator space, and can be specified wherever a separator space can be
            //   specified.
            // - All characters following the floating comment indicator up to the end of Area B
            //   are comment text.

            // consume all chars until the end of line
            currentIndex = lastIndex + 1;
            return new Token(TokenType.FloatingComment, startIndex, lastIndex, tokensLine, true, true, ' ');
        }

        private Token ScanNumericLiteral(int startIndex)
        {
            // p37: Numeric literals
            // A numeric literal is a character-string whose characters are selected from the digits 0
            // through 9, a sign character (+ or -), and the decimal point.
            // If the literal contains no decimal point, it is an integer. (In this documentation, the
            // word integer appearing in a format represents a numeric literal of nonzero value
            // that contains no sign and no decimal point, except when other rules are included
            // with the description of the format.) The following rules apply:
            // - If the ARITH(COMPAT) compiler option is in effect, one through 18 digits are
            //   allowed. If the ARITH(EXTEND) compiler option is in effect, one through 31
            //   digits are allowed.
            // - Only one sign character is allowed. If included, it must be the leftmost character
            //   of the literal. If the literal is unsigned, it is a positive value.
            // - Only one decimal point is allowed. If a decimal point is included, it is treated as
            //   an assumed decimal point (that is, as not taking up a character position in the
            //   literal). The decimal point can appear anywhere within the literal except as the
            //   rightmost character.
            // p38: Rules for floating-point literal values
            // The format and rules for floating-point literals are listed below.
            // Format : ('+' | '-')? mantissa 'E' ('+' | '-')? exponent
            // - The sign is optional before the mantissa and the exponent; if you omit the sign,
            //   the compiler assumes a positive number.
            // - The mantissa can contain between one and 16 digits. A decimal point must be
            //   included in the mantissa.
            // - The exponent is represented by an E followed by an optional sign and one or
            //   two digits.
            // - The magnitude of a floating-point literal value must fall between 0.54E-78 and
            //   0.72E+76. For values outside of this range, an E-level diagnostic message is
            //   produced and the value is replaced by either 0 or 0.72E+76, respectively.
      
            // Handle DECIMAL-POINT IS COMMA clause
            char decimalPoint = '.';
            if(tokensLine.ScanState.DecimalPointIsComma)
            {
                decimalPoint = ',';
            }

            //IntegerLiteral = 27,
            // Fast path for the most common type of numeric literal : simple integer literals like the level numbers (no sign, no decimal point)
            char firstChar = line[startIndex];
            if(Char.IsDigit(firstChar))
            {
                // consume all the following digits
                int fstCurrentIndex = startIndex + 1;
                for (; fstCurrentIndex <= lastIndex && Char.IsDigit(line[fstCurrentIndex]); fstCurrentIndex++) { }
                // check to see if the following char could be part of a numeric literal
                if( fstCurrentIndex > lastIndex || 
                    (line[fstCurrentIndex] != decimalPoint && line[fstCurrentIndex] != 'e' && line[fstCurrentIndex] != 'E') ||
                    (line[fstCurrentIndex] == decimalPoint && (fstCurrentIndex == lastIndex || line[fstCurrentIndex+1] == ' ')))
                {
                    // if it is not the case, assume this is the end of a simple integer literal
                    currentIndex = fstCurrentIndex;
                    int endIndex = fstCurrentIndex - 1;
                    Token token = new Token(TokenType.IntegerLiteral, startIndex, endIndex, tokensLine);
                    token.LiteralValue = new IntegerLiteralTokenValue(null, line.Substring(startIndex, fstCurrentIndex - startIndex));


                    if (BeSmartWithLevelNumber) { 
                        // Distinguish the special case of a LevelNumber
                        if (tokensLine.ScanState.InsideDataDivision && tokensLine.ScanState.AtBeginningOfSentence) {
                            token.CorrectType(TokenType.LevelNumber);
                        }
                    }

                    return token;
                }
            }
            
            // If the fast path attempt was not successful, try to match each one
            // of the two other numeric literal formats, with regular expressions 

            int lookupEndIndex = startIndex;

            // consume the first char if it was +/- (to simplify the count of chars below)
            if(line[startIndex] == '+' || line[startIndex] == '-')
            {
                lookupEndIndex++;
            }
            // then consume the following chars : digits many times, + -  e E . only once             
            bool currentCharStillInLiteral = false;
            bool plusMinusFound = false;
            bool periodFound = false;
            bool eEFound = false;
            do
            {
                char c = line[lookupEndIndex];
                currentCharStillInLiteral = Char.IsDigit(c) || (!periodFound && c == decimalPoint) || (!plusMinusFound && (c == '+' || c == '-')) || (!eEFound && (c == 'e' || c == 'E'));
                if (currentCharStillInLiteral)
                {
                    if (!plusMinusFound && (c == '+' || c == '-')) plusMinusFound = true;
                    if (!periodFound && c == decimalPoint) periodFound = true;
                    if (!eEFound && (c == 'e' || c == 'E')) eEFound = true;
                    lookupEndIndex++;
                }
            }
            while (currentCharStillInLiteral && lookupEndIndex <= lastIndex);
            lookupEndIndex = (lookupEndIndex > lastIndex) ? lastIndex : lookupEndIndex - 1;
            // we may have consumed one additonal character after the end of the literal
            if ((line[lookupEndIndex] == decimalPoint || line[lookupEndIndex] == '+' || line[lookupEndIndex] == '-') && lookupEndIndex > startIndex)
            {
                lookupEndIndex--;
            }
            // then try to predict the intended the number format
            string numberString = line.Substring(startIndex, lookupEndIndex - startIndex + 1);
            if (numberString.Contains('E') || numberString.Contains('e'))
            {
                //FloatingPointLiteral = 29,
                Match fpMatch = floatingPointLiteralRegex.Match(line, startIndex, lastIndex - startIndex + 1);
                if (fpMatch.Success && fpMatch.Index == startIndex)
                {
                    currentIndex += fpMatch.Length;
                    int endIndex = startIndex + fpMatch.Length - 1;
                    Token token = new Token(TokenType.FloatingPointLiteral, startIndex, endIndex, tokensLine);
                    string mantissaDecimalPart = fpMatch.Groups[3].Value;
                    if (string.IsNullOrEmpty(mantissaDecimalPart))
                    {
                        tokensLine.AddDiagnostic(MessageCode.InvalidMantissaInFloatingPointLiteral, token);
                    }
                    string exponent = fpMatch.Groups[5].Value;
                    if (exponent.Length > 2)
                    {
                        tokensLine.AddDiagnostic(MessageCode.InvalidExponentInFloatingPointLiteral, token);
                    }
                    token.LiteralValue = new FloatingPointLiteralTokenValue(fpMatch.Groups[1].Value, fpMatch.Groups[2].Value, mantissaDecimalPart, fpMatch.Groups[4].Value, exponent);
                    return token;
                }
                else
                {
                    // consume all lookup chars
                    currentIndex = lookupEndIndex + 1;
                    Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lookupEndIndex, tokensLine);
                    tokensLine.AddDiagnostic(MessageCode.InvalidNumericLiteralFormat, invalidToken);
                    return invalidToken;
                }
            }
            else
            {
                //DecimalLiteral = 28,
                Match decMatch = decimalLiteralRegex.Match(line, startIndex, lastIndex - startIndex + 1);
                if (decMatch.Success && decMatch.Index == startIndex)
                {
                    currentIndex += decMatch.Length;
                    int endIndex = startIndex + decMatch.Length - 1;
                    TokenType type;
                    LiteralTokenValue value;
                    if(decMatch.Groups[3].Value.Length > 0) {
                        type = TokenType.DecimalLiteral;
                        value = new DecimalLiteralTokenValue(decMatch.Groups[1].Value, decMatch.Groups[2].Value, decMatch.Groups[3].Value);
                    } else {
                        type = TokenType.IntegerLiteral;
                        value = new IntegerLiteralTokenValue(decMatch.Groups[1].Value, decMatch.Groups[2].Value);
                    }
                    Token token = new Token(type, startIndex, endIndex, tokensLine);
                    token.LiteralValue = value;
                    return token;
                }
                else
                {
                    // consume all lookup chars
                    currentIndex = lookupEndIndex + 1;
                    Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lookupEndIndex, tokensLine);
                    tokensLine.AddDiagnostic(MessageCode.InvalidNumericLiteralFormat, invalidToken);
                    return invalidToken;
                }
            }   
        }

        private static Regex decimalLiteralRegex = new Regex("([-+]?)([0-9]*)(?:[\\.,]([0-9]+))?", RegexOptions.Compiled);
        private static Regex floatingPointLiteralRegex = new Regex("([-+]?)([0-9]*)(?:[\\.,]([0-9]+))?[eE]([-+]?)([0-9]+)", RegexOptions.Compiled);

        private Token ScanAlphanumericLiteral(int startIndex, TokenType tokenType)
        {
            // p46: Alphanumeric Literals 
            //   Quotation marks {"} ... {"}
            //   Apostrophes {’} ... {’}
            // Delimiters must appear as balanced pairs.
            // An opening quotation mark must be immediately preceded by a space or a
            // left parenthesis. A closing quotation mark must be immediately followed
            // by a separator space, comma, semicolon, period, right parenthesis, or
            // pseudo-text delimiter.

            // p34: Basic alphanumeric literals
            // Basic alphanumeric literals can contain any character in a single-byte EBCDIC
            // character set.
            // The following format is for a basic alphanumeric literal:
            // Format 1: Basic alphanumeric literals
            // "single-byte-characters"
            //’ single-byte-characters’
            // The enclosing quotation marks or apostrophes are excluded from the literal when
            // the program is compiled.
            // An embedded quotation mark or apostrophe must be represented by a pair of
            // quotation marks ("") or a pair of apostrophes (’’), respectively, when it is the
            // character used as the opening delimiter. For example:
            // "THIS ISN""T WRONG"
            //’ THIS ISN’’T WRONG’
            // The delimiter character used as the opening delimiter for a literal must be used as
            // the closing delimiter for that literal. For example:
            // ’THIS IS RIGHT’
            // "THIS IS RIGHT"
            // ’THIS IS WRONG"
            // You can use apostrophes or quotation marks as the literal delimiters independent
            // of the APOST/QUOTE compiler option.
            // Any punctuation characters included within an alphanumeric literal are part of the
            // value of the literal.
            // The maximum length of an alphanumeric literal is 160 bytes. The minimum length
            // is 1 byte.

            // consume opening delimiter
            char delimiter = line[currentIndex];
            currentIndex++;
            
            // consume all chars until we encounter one occurence (and only one) of the delimiter 
            bool closingDelimiterFound = false;
            bool usingVirtualSpaceAtEndOfLine = false;
            StringBuilder sbValue = new StringBuilder();
            do
            {
                for (; currentIndex <= lastIndex && line[currentIndex] != delimiter; currentIndex++)
                {
                    char currentChar = line[currentIndex];
                    sbValue.Append(currentChar);
                }
                // delimiter found before the last character of the line
                if (currentIndex < lastIndex)
                {
                    // continue in case of a double delimiter
                    if (line[currentIndex + 1] == delimiter)
                    {
                        // consume the two delimiters
                        currentIndex += 2;
                        // append one delimiter to the literal value
                        sbValue.Append(delimiter);
                    }
                    // stop in case of a simple delimiter
                    else
                    {
                        // consume closing delimiter
                        currentIndex++;
                        closingDelimiterFound = true;
                        usingVirtualSpaceAtEndOfLine = false;
                    }
                }
                // delimiter found on the last character of the line
                else if (currentIndex == lastIndex)
                {
                    // consume closing delimiter
                    currentIndex++;
                    closingDelimiterFound = true;
                    usingVirtualSpaceAtEndOfLine = true;
                }
            } while (currentIndex <= lastIndex && !closingDelimiterFound);

            // create an alphanumeric literal token
            int endIndex = (currentIndex > lastIndex) ? lastIndex : currentIndex - 1;
            Token token = new Token(tokenType, startIndex, endIndex, usingVirtualSpaceAtEndOfLine, tokensLine, true, closingDelimiterFound, delimiter);
            
            // compute the value of the literal, depending on the exact literal type            
            AlphanumericLiteralTokenValue value = null;
            if (tokenType != TokenType.HexadecimalAlphanumericLiteral && tokenType != TokenType.HexadecimalNationalLiteral)
            {
                value = new AlphanumericLiteralTokenValue(sbValue.ToString());
            }
            else if (tokenType == TokenType.HexadecimalAlphanumericLiteral)
            {
                // p36: Hexadecimal notation for alphanumeric literals
                // Hexadecimal digits are characters in the range '0' to '9', 'a' to 'f', and 'A' to 'F',
                // inclusive. 
                // An even number of hexadecimal digits must be specified.
                // Two hexadecimal digits represent one character in a single-byte character
                // set (EBCDIC or ASCII). Four hexadecimal digits represent one character in a DBCS
                // character set. A string of EBCDIC DBCS characters represented in hexadecimal
                // notation must be preceded by the hexadecimal representation of a shift-out control
                // character (X'0E') and followed by the hexadecimal representation of a shift-in
                // control character (X'0F'). 
                // The maximum length of a hexadecimal literal is 320 hexadecimal digits.

                string hexadecimalChars = sbValue.ToString();
                if (hexadecimalChars.Length % 2 != 0)
                {
                    tokensLine.AddDiagnostic(MessageCode.InvalidNumberOfCharsInHexaAlphaLiteral, token);
                }
                value = new AlphanumericLiteralTokenValue(hexadecimalChars, tokensLine.ScanState.EncodingForAlphanumericLiterals);
            }
            else if (tokenType == TokenType.HexadecimalNationalLiteral)
            {
                // p41: Hexadecimal notation for national literals
                // The number of hexadecimal digits must be a multiple of four.
                // Each group of four hexadecimal digits represents a single national
                // character and must represent a valid code point in UTF-16. 

                string hexadecimalChars = sbValue.ToString();
                if (hexadecimalChars.Length % 4 != 0)
                {
                    tokensLine.AddDiagnostic(MessageCode.InvalidNumberOfCharsInHexaNationalLiteral, token);
                }
                value = new AlphanumericLiteralTokenValue(hexadecimalChars, Encoding.Unicode);
            }
            token.LiteralValue = value;

            return token;
        }

        private Token ScanCharacterString(int startIndex)
        {
            // p9: A character-string is a character or a sequence of contiguous characters that forms a
            // COBOL word, a literal, a PICTURE character-string, or a comment-entry. A
            // character-string is delimited by separators.

            // p9: Except for arithmetic operators and relation characters, each character of a COBOL
            // word is selected from the following set:
            // - Latin uppercase letters A through Z
            // - Latin lowercase letters a through z
            // - digits 0 through 9
            // - - (hyphen)
            // - _ (underscore)
            // The hyphen cannot appear as the first or last character in such words. The
            // underscore cannot appear as the first character in such words. Most user-defined
            // words (all except section-names, paragraph-names, priority-numbers, and
            // level-numbers) must contain at least one alphabetic character. 

            // All the following special cases are handled before we call this method, at the beginning of GetNextToken()
            // - ScanCommentEntry(startIndex);
            // - ScanExecTranslatorName(startIndex);
            // - ScanExecStatementTextOrExecSqlInclude(startIndex);
            // - ScanPictureCharacterString(startIndex);

            // The only possibility remaining is a keyword or a user defined word
            return ScanKeywordOrUserDefinedWord(startIndex);
        }

        private Token ScanPictureCharacterStringOrISOrSYMBOL(int startIndex)
        {
            //PictureCharacterString = 30,
            // p42: A PICTURE character-string is composed of the currency symbol and certain
            // combinations of characters in the COBOL character set. PICTURE character-strings
            // are delimited only by the separator space, separator comma, separator semicolon,
            // or separator period.
            // A chart of PICTURE clause symbols appears in Table 12 on page 199.
            // -> LP : the following separators are not delimiters for picture strings :
            // Left parenthesis, Right parenthesis, Colon
            // p199: character-string
            // character-string is made up of certain COBOL characters used as picture
            // symbols. The allowable combinations determine the category of the
            // elementary data item.
            // character-string can contain a maximum of 50 characters.
            // Symbols used in the PICTURE clause
            // Any punctuation character that appears within the PICTURE character-string is not
            // considered a punctuation character, but rather is a PICTURE character-string
            // symbol.
            // When specified in the SPECIAL-NAMES paragraph, DECIMAL-POINT IS
            // COMMA exchanges the functions of the period and the comma in PICTURE
            // character-strings and in numeric literals.
            // The lowercase letters that correspond to the uppercase letters that represent the
            // following PICTURE symbols are equivalent to their uppercase representations in a
            // PICTURE character-string:
            // A, B, E, G, N, P, S, V, X, Z, CR, DB
            // All other lowercase letters are not equivalent to their corresponding uppercase
            // representations.
            // ... p202 -> p204 : more rules to check that a picture string is valid ...

            
            // consume any char until a separator space, separator comma (+space), separator semicolon (+space), or separator period (+space) is encountered
            for (; currentIndex < lastIndex && line[currentIndex] != ' ' ; currentIndex++) { }
            int endIndex = currentIndex;
            if(line[endIndex] == ' ')
            {
                endIndex--;
            }
            else
            {
                currentIndex++;
            }
            char lastNonSpaceChar = line[endIndex];
            if(lastNonSpaceChar == ',' || lastNonSpaceChar == ';'  || lastNonSpaceChar == '.')
            {
                endIndex--;
                currentIndex--;
            }   

            // ! could be keywords SYMBOL or IS 
            string value = line.Substring(startIndex, endIndex - startIndex + 1);
            if(value.Equals("SYMBOL", StringComparison.InvariantCultureIgnoreCase))
            {
                // Return a keyword
                return new Token(TokenType.SYMBOL, startIndex, endIndex, tokensLine);
            }
            else if (value.Equals("IS", StringComparison.InvariantCultureIgnoreCase))
            {
                // Return a keyword
                return new Token(TokenType.IS, startIndex, endIndex, tokensLine);
            }
            else
            {
                // Return a picture character string
                return new Token(TokenType.PictureCharacterString, startIndex, endIndex, tokensLine);
            }
        }

        private Token ScanCommentEntry(int startIndex)
        {
            //CommentEntry = 31,
            // identificationDivisionContent :
            //                      (AUTHOR PeriodSeparator? CommentEntry*)?
            //                      (INSTALLATION PeriodSeparator? CommentEntry*)?
            //                      (DATE_WRITTEN PeriodSeparator? CommentEntry*)?
            //                      (DATE_COMPILED PeriodSeparator? CommentEntry*)?
            //                      (SECURITY PeriodSeparator? CommentEntry*)?;
            
            // Consume the entire line as a comment entry
            currentIndex = lastIndex + 1;
            return new Token(TokenType.CommentEntry, startIndex, lastIndex, tokensLine);
        }

        private Token ScanExecTranslatorName(int startIndex)
        {
            //ExecTranslatorName = 35,
            // p424: Delimit SQL statements with EXEC SQL and END-EXEC. The EXEC SQL and END-EXEC
            // delimiters must each be complete on one line. You cannot continue them across
            // multiple lines. Do not code COBOL statements within EXEC SQL statements.
            // Restriction: You cannot use SQL statements in object-oriented classes or methods.

            // Scan until a first whitespace char       
            for (; currentIndex < lastIndex && line[currentIndex] != ' '; currentIndex++) { }
            int endIndex = lastIndex;
            if(line[currentIndex] == ' ')
            {
                endIndex = currentIndex - 1;
            }
            else
            {
                currentIndex++;
            }
            
            return new Token(TokenType.ExecTranslatorName, startIndex, endIndex, tokensLine);
        }

        private Token ScanExecStatementTextOrExecSqlInclude(int startIndex)
        {
            // --- Special treatment for EXEC SQL(IMS) INCLUDE ---

            // Check if previous tokens were EXEC SQL or EXEC SQLIMS
            if(tokensLine.ScanState.AfterExecSql)
            {
                // Check if the text immediately following is INCLUDE
                if (lastIndex - startIndex >= 6 &&
                   line.Substring(startIndex, 7).Equals("INCLUDE", StringComparison.InvariantCultureIgnoreCase))
                {
                    // Consume 7 chars
                    currentIndex = startIndex + 7;
                    return new Token(TokenType.EXEC_SQL_INCLUDE, startIndex, startIndex + 6, tokensLine);
                }
            }

            // ---

            //ExecStatementText = 32,
            // p424: Delimit SQL statements with EXEC SQL and END-EXEC. The EXEC SQL and END-EXEC
            // delimiters must each be complete on one line. You cannot continue them across
            // multiple lines. Do not code COBOL statements within EXEC SQL statements.
            // Restriction: You cannot use SQL statements in object-oriented classes or methods.

            // Try to find "END-EXEC" on the line
            int endIndex = lastIndex;
            int endExecIndex = line.IndexOf("END-EXEC", startIndex, StringComparison.InvariantCultureIgnoreCase);
            // ExecStatementText is not empty
            if (endExecIndex > startIndex)
            {
                endIndex = endExecIndex - 1;
                // Remove all witespace just before END-EXEC
                for (; endIndex > startIndex && line[endIndex] == ' '; endIndex--) { }

                // If only whitespace just before END-EXEC, return a whitespace token
                if(endIndex == startIndex && line[endIndex] == ' ')
                {
                    return ScanWhitespace(startIndex);  
                }
            }
            // ExecStatementText is empty
            else if (endExecIndex == startIndex)
            {
                // Directly scan END-EXEC keyword
                return ScanKeywordOrUserDefinedWord(startIndex);
            }

            // Consume all chars
            currentIndex = endIndex + 1;

            return new Token(TokenType.ExecStatementText, startIndex, endIndex, tokensLine);
        }

        private Token ScanKeywordOrUserDefinedWord(int startIndex)
        {
            // p9: A COBOL word is a character-string that forms a user-defined word, a
            // system-name, or a reserved word.

            // p45: Table 4. Separators

            // consume any char until a separator char is encountered
            for (; currentIndex <= lastIndex && !CobolChar.IsCobolWordSeparator(line[currentIndex]); currentIndex++) { }
            int endIndex = (currentIndex == lastIndex && !CobolChar.IsCobolWordSeparator(line[currentIndex])) ? lastIndex : currentIndex - 1;
            
            // The COPY statement with REPLACING phrase can be used to replace parts of words. 
            // By inserting a dummy operand delimited by colons into the program text, the compiler will replace the dummy operand with the desired text. 
            if(endIndex + 3 <= lastIndex && line[endIndex + 1] == ':')
            {
                int patternEndIndex;
                if(CheckForPartialCobolWordPattern(endIndex + 1, out patternEndIndex))
                {
                    return ScanPartialCobolWord(startIndex, patternEndIndex);
                }
            }

            // Compute token type : keyword, intrinsic fonction name, symbolic character or user defined word ?
            TokenType tokenType = TokenType.UserDefinedWord;
            string tokenText = line.Substring(startIndex, endIndex - startIndex + 1);

            //IntrinsicFunctionName = 34,
            // p477: function-name-1 must be one of the intrinsic function names.
            // ACOS | ANNUITY | ASIN | ATAN | CHAR | COS | CURRENT_DATE | DATE_OF_INTEGER | DATE_TO_YYYYMMDD |
            // DAY_OF_INTEGER | DAY_TO_YYYYDDD | DISPLAY_OF | FACTORIAL | INTEGER | INTEGER_OF_DATE | INTEGER_OF_DAY |
            // INTEGER_PART | LENGTH | LOG | LOG10 | LOWER_CASE | MAX | MEAN | MEDIAN | MIDRANGE | MIN | MOD |
            // NATIONAL_OF | NUMVAL | NUMVAL_C | ORD | ORD_MAX | ORD_MIN | PRESENT_VALUE | RANDOM | RANGE | REM |
            // REVERSE | SIN | SQRT | STANDARD_DEVIATION | SUM | TAN | ULENGTH | UPOS | UPPER_CASE | USUBSTR |
            // USUPPLEMENTARY | UVALID | UWIDTH | VARIANCE | WHEN_COMPILED | YEAR_TO_YYYY
            if (tokensLine.ScanState.AfterFUNCTION && TokenUtils.COBOL_INTRINSIC_FUNCTIONS.IsMatch(tokenText))
            {
                tokenType = TokenType.IntrinsicFunctionName;
            }
            else
            {
                // p12: A reserved word is a character-string with a predefined meaning in a COBOL source
                // unit.

                // p13: Keywords
                // Keywords are reserved words that are required within a given clause,
                // entry, or statement. Within each format, such words appear in uppercase
                // on the main path.

                // p9: In COBOL words (but not in the content of alphanumeric, DBCS, and national
                // literals), each lowercase single-byte alphabetic letter is considered to be equivalent
                // to its corresponding single-byte uppercase alphabetic letter.

                // p9: The following rules apply for all COBOL words:
                // - A reserved word cannot be used as a user-defined word or as a system-name.
                // - The same COBOL word, however, can be used as both a user-defined word and
                //   as a system-name.

                // Try to match keyword text
                tokenType = TokenUtils.GetTokenTypeFromTokenString(tokenText);

                // Special cases of user defined words : 
                // - symbolic characters
                // - section and paragraph names
                if (tokenType == TokenType.UserDefinedWord)
                {
                    // p117: SYMBOLIC CHARACTERS clause
                    // symbolic-character-1 is a user-defined word and must contain at least one alphabetic character.
                    // The same symbolic-character can appear only once in a SYMBOLIC CHARACTERS clause.
                    // The symbolic character can be a DBCS user-defined word.
                    if (tokensLine.ScanState.InsideSymbolicCharacterDefinitions)
                    {
                        // Symbolic character definition
                        tokenType = TokenType.SymbolicCharacter;
                        tokensLine.ScanState.AddSymbolicCharacter(tokenText);
                    }
                    else if (tokensLine.ScanState.SymbolicCharacters != null)
                    {
                        // Try to match a previously defined SymbolicCharacter
                        if (tokensLine.ScanState.SymbolicCharacters.Contains(tokenText))
                        {
                            tokenType = TokenType.SymbolicCharacter;
                        }
                    }

                    // Section and paragraph names
                    if (tokensLine.ScanState.InsideProcedureDivision && tokensLine.ScanState.AtBeginningOfSentence)
                    {
                        tokenType = TokenType.SectionParagraphName;
                    }
                }
            }

            // Special case : CBL/PROCESS compiler directives
            // This compiler directive sets compiler options which can have an impact
            // on the way the Scanner interprets the next line => we must analyse
            // the syntax of this compiler directive right here, without waiting
            // for the preprocessing phase => scan a CompilerDirectiveToken 
            if(tokenType == TokenType.CBL || tokenType == TokenType.PROCESS)
            {
                return ScanCblProcessCompilerDirective(startIndex, tokenType);
            }
            
            // p11: The maximum length of a user-defined word is 30 bytes, except for level-numbers
            // and priority-numbers. Level-numbers and priority numbers must each be a
            // one-digit or two-digit integer.

            // Return a keyword or user defined word
            return new Token(tokenType, startIndex, endIndex, tokensLine);
        }

        /// <summary>
        /// p 528 : CBL (PROCESS) statement
        /// With the CBL (PROCESS) statement, you can specify compiler options to be used in the compilation of the program. 
        /// The CBL (PROCESS) statement is placed before the IDENTIFICATION DIVISION header of an outermost program.
        /// </summary>
        private Token ScanCblProcessCompilerDirective(int startIndex, TokenType tokenType)
        {
            // CBL or PROCESS keyword has been matched before
            // in ScanKeywordOrUserDefinedWord()
            CblProcessDirective compilerDirective = new CblProcessDirective(tokenType == TokenType.CBL ? CompilerDirectiveType.CBL : CompilerDirectiveType.PROCESS);

            bool lineSyntaxIsValid = true;

            // The CBL (PROCESS) statement must end before or at column 72, 
            // and options cannot be continued across multiple CBL (PROCESS) statements. 
            while (currentIndex <= lastIndex)
            {
                // options-list
                // A series of one or more compiler options, each one separated by a comma or a space.
                // For more information about compiler options, see Compiler options in the Enterprise COBOL Programming Guide.

                // Ignore spaces and commas between compiler options
                while (currentIndex <= lastIndex && (line[currentIndex] == ' ' || line[currentIndex] == ','))
                {
                    currentIndex++;
                }
                if (currentIndex > lastIndex) break;

                // Cobol Programming Guide p299 : Compiler options samples
                //
                // NOADATA AFP(VOLATILE) ARCH(6) CODEPAGE(1140) BUFSIZE(1K)
                // CICS(’string2’) CICS("string3") CURRENCY(AlphanumericLiteral | HexadecimalAlphanumericLiteral)
                // EXIT( INEXIT([’str1’,]mod1) LIBEXIT([’str2’,]mod2) )
                // FLAG(I,I) FLAGSTD(x[yy][,0])
                // 
                // compilerOption: name=UserDefinedWord (LeftParenthesisSeparator parameters+=(~RightParenthesisSeparator)* RightParenthesisSeparator)?;

                // Scan option name
                int nameStartIndex = currentIndex;                
                if (CobolChar.IsCobolWordSeparator(line[nameStartIndex]))
                {
                    lineSyntaxIsValid = false;
                    break;
                }
                else
                {
                    for (; currentIndex <= lastIndex && !CobolChar.IsCobolWordSeparator(line[currentIndex]); currentIndex++) { }
                    int nameEndIndex = (currentIndex == lastIndex && !CobolChar.IsCobolWordSeparator(line[currentIndex])) ? lastIndex : currentIndex - 1;
                    string optionWord = line.Substring(nameStartIndex, nameEndIndex - nameStartIndex + 1);

                    // Try to scan option parameters
                    string optionParameters = null;
                    if (currentIndex <= lastIndex && !(line[currentIndex] == ' ' || line[currentIndex] == ','))
                    {
                        if (currentIndex < lastIndex && line[currentIndex] == '(')
                        {
                            // Consume '('
                            currentIndex++;

                            // Scan option parameters
                            int paramsStartIndex = currentIndex;
                            int nestedParens = 0;
                            for (; currentIndex <= lastIndex && !(line[currentIndex] == ')' && nestedParens == 0); currentIndex++)
                            {
                                if (line[currentIndex] == '(') nestedParens++;
                                if (line[currentIndex] == ')') nestedParens--;
                            }
                            if (currentIndex <= lastIndex && line[currentIndex] == ')')
                            {
                                int paramsEndIndex = currentIndex - 1;
                                optionParameters = line.Substring(paramsStartIndex, paramsEndIndex - paramsStartIndex + 1);

                                // Store compiler option in compiler directive
                                compilerDirective.OptionsList.Add(
                                    new CblProcessDirective.OptionText() { Word = optionWord, Parameters = optionParameters });

                                // Register compiler options in options container
                                if (!compilerOptions.TrySetIBMOptionStatusAndValue(optionWord, optionParameters))
                                {
                                    lineSyntaxIsValid = false;
                                }

                                // Consume ')'
                                currentIndex++;
                            }
                            else
                            {
                                lineSyntaxIsValid = false;
                            }
                        }
                        else
                        {
                            lineSyntaxIsValid = false;
                        }
                    }
                    else
                    {
                        // No parameters => Store compiler option activation in compiler directive
                        compilerDirective.OptionsList.Add(
                            new CblProcessDirective.OptionText() { Word = optionWord, Parameters = null });

                        // Register compiler options in options container
                        if (!compilerOptions.TrySetIBMOptionStatusAndValue(optionWord, null))
                        {
                            lineSyntaxIsValid = false;
                        }
                    }
                }
            }

            // Match all chars until the end of the line in case of syntax error
            // (CLB/PROCESS compiler directive statement must stay alone on its line)
            if(!lineSyntaxIsValid)
            {
                currentIndex = lastIndex + 1;
            }

            // Create a single token including all the chars participating in the CBL/PROCESS directive 
            int stopIndex = currentIndex - 1;
            Token sourceToken = new Token(TokenType.CompilerDirective, startIndex, stopIndex, tokensLine);
            
            // Wrap the source token in a CompilerDirectiveToken (which is a GroupToken by defaut)
            IList<Token> originalTokens = new List<Token>();
            originalTokens.Add(sourceToken);
            CompilerDirectiveToken compilerDirectiveToken = new CompilerDirectiveToken(compilerDirective, originalTokens, !lineSyntaxIsValid);
            if (!lineSyntaxIsValid)
            {            
                tokensLine.AddDiagnostic(MessageCode.InvalidCblProcessCompilerDirective, compilerDirectiveToken);
            }
            return compilerDirectiveToken;
        }

        /// <summary>
        /// Look for pattern ':' (cobol word chars)+ ':'
        /// </summary>
        private bool CheckForPartialCobolWordPattern(int startIndex, out int patternEndIndex)
        {
            patternEndIndex = -1;

            // minimum length
            if(startIndex + 2 > lastIndex) return false;

            // match all legal cobol word chars
            int index = startIndex + 1;
            for (; index <= lastIndex && CobolChar.IsCobolWordChar(line[index]); index++) 
            { }

            // no legal cobol word chars found 
            if (index == startIndex + 1 && !CobolChar.IsCobolWordChar(line[index])) return false;

            // next character must be ':'
            if(line.Length > index && line[index] == ':')
            {
                patternEndIndex = index;
                return true;
            }
            else
            {
                return false;
            }
        }

        // :PREFIX:-NAME or NAME-:SUFFIX: or :TAG:
        private Token ScanPartialCobolWord(int startIndex, int patternEndIndex)
        {
            int endIndex = patternEndIndex;
            while (endIndex < lastIndex)
            {
                int searchIndex = endIndex + 1;

                // consume any char until a separator char is encountered
                for (; searchIndex <= lastIndex && !CobolChar.IsCobolWordSeparator(line[searchIndex]); searchIndex++) { }
                searchIndex = (searchIndex == lastIndex && !CobolChar.IsCobolWordSeparator(line[searchIndex])) ? lastIndex : searchIndex - 1;
                if (!CobolChar.IsCobolWordSeparator(line[searchIndex]))
                {
                    endIndex = searchIndex;
                }

                // Check for another pattern to replace
                if (searchIndex + 3 <= lastIndex && line[searchIndex + 1] == ':')
                {
                    int otherPatternEndIndex;
                    if (CheckForPartialCobolWordPattern(searchIndex + 1, out otherPatternEndIndex))
                    {
                        endIndex = otherPatternEndIndex;
                    }
                    else { break; }
                } else { break; }
            }

            // Consume the entire the partial Cobol word
            currentIndex = endIndex + 1;

            return new Token(TokenType.PartialCobolWord, startIndex, endIndex, tokensLine);
        }        
    }
}
