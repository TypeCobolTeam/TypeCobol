using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text.RegularExpressions;
using JetBrains.Annotations;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Sql.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Divides a line of text into a list of tokens
    /// </summary>
    public class Scanner : AbstractScanner
    {
        /// <summary>
        /// Issue #428, quick fix for this issue.
        /// Method ScanIsolatedTokenInDefaultContext need the scanState of the previous token in order to parser the new token
        /// correctly. But the caller of this method doesn't have the scanState. It only has the scanState at the beginning of the line.
        /// A solution would be to rescan all the line.
        /// </summary>
        public bool BeSmartWithLevelNumber { get; }

        /// <summary>
        /// Scan a line of a document
        /// </summary>
        /// <param name="tokensLine"></param>
        /// <param name="initialScanState"></param>
        /// <param name="compilerOptions"></param>
        /// <param name="copyTextNameVariations"></param>
        /// <param name="multiStringConcatBitPosition">Bit array of Multi String concatenation positions</param>
        public static void ScanTokensLine(TokensLine tokensLine, MultilineScanState initialScanState, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations,
            BitArray multiStringConcatBitPosition = null)
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

#if EUROINFO_RULES
            if (compilerOptions.UseEuroInformationLegacyReplacingSyntax && !compilerOptions.IsCobolLanguage)
            {
                if (tokensLine.ScanState.LeavingRemarksDirective) //If last scanned line was the end of a remarksDirective then mark scanstate as outside of remarksDirective for this new line
                    tokensLine.ScanState.InsideRemarksDirective = tokensLine.ScanState.LeavingRemarksDirective = false;

                if (IsInsideRemarks(textLine.Type, tokensLine.SourceText)) tokensLine.ScanState.InsideRemarksDirective = true;
                else if (textLine.Type == CobolTextLineType.Source) tokensLine.ScanState.InsideRemarksDirective = false;
                // Try to scan REMARKS compiler directive parameters inside the comment or non-comment line
                if (tokensLine.ScanState.InsideRemarksDirective)
                {
                    string remarksLine = textLine.SourceText?.TrimEnd();

                    if (!string.IsNullOrEmpty(remarksLine))
                    {
                        int startIndexForSignificantPart = GetStartIndexOfSignificantPart(remarksLine, tokensLine.ScanState);
                        int firstPeriodIndex = remarksLine.IndexOf('.', startIndexForSignificantPart);
                        int endIndexForSignificantPart = GetEndIndexOfSignificantPart(remarksLine, tokensLine.ScanState, firstPeriodIndex);
                        string significantPart = remarksLine.Substring(startIndexForSignificantPart, endIndexForSignificantPart - startIndexForSignificantPart + 1).Trim();


                        if (tokensLine.ScanState.InsideRemarksDirective && (remarksLine.Contains(").") || remarksLine.Contains(")")))
                        {
                            tokensLine.ScanState.LeavingRemarksDirective = true; // indicates the end of the REMARKS compiler directive
                        }

                        RemarksDirective remarksDirective = CreateRemarksDirective(significantPart, tokensLine.ScanState);
                        if (remarksDirective != null && remarksDirective.CopyTextNamesVariations.Count > 0)
                        {
                            // A non empty remarks directive will replace the comment line
                            tokensLine.AddToken(CreateCompilerDirectiveToken(remarksDirective, tokensLine, startIndex, lastIndex, copyTextNameVariations));
                            return;
                        }
                    }
                }
            }

#endif


            // Comment line => return only one token with type CommentLine
            // Debug line => treated as a comment line if debugging mode was not activated
            if (textLine.Type == CobolTextLineType.Comment || (tokensLine.Type == CobolTextLineType.Debug && !IsDebuggingModeActive(tokensLine.ScanState, tokensLine.SourceText)))
            {
                if (tokensLine.ColumnsLayout == ColumnsLayout.CobolReferenceFormat && tokensLine.Text.Length > 80)
                {
                    tokensLine.AddDiagnostic(MessageCode.Warning,
                        tokensLine.Indicator.StartIndex, tokensLine.Indicator.EndIndex, "Line exceed 80 chars");
                }

                Token commentToken = new Token(TokenType.CommentLine, startIndex, lastIndex, tokensLine);
                tokensLine.AddToken(commentToken);
                return;
            }
            else if (textLine.Type == CobolTextLineType.MultiFormalizedComment)
            {
                //If a '%' is spotted that isn't a multiline/formalized comment token or if a stop token is spotted without an associated start
                if (tokensLine.SourceText == null || !tokensLine.SourceText.StartsWith("<<") && !tokensLine.SourceText.StartsWith(">>") || 
                    !tokensLine.ScanState.InsideMultilineComments && !tokensLine.ScanState.InsideFormalizedComment && tokensLine.SourceText.StartsWith(">>"))
                {
                    tokensLine.AddDiagnostic(MessageCode.MultiFormalizedCommentIndicatorMisused, textLine.Indicator.StartIndex, textLine.Indicator.EndIndex, textLine.Indicator);
                    Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lastIndex, tokensLine);
                    tokensLine.AddToken(invalidToken);
                    return;
                }
                else
                {
                    if (tokensLine.ScannerDiagnostics.Count > 0)
                        tokensLine.ClearAllDiagnostics();
                }

            }
            // Invalid indicator, the line type is unknown => the whole line text is handled as a single invalid token
            else if (textLine.Type == CobolTextLineType.Invalid)
            {
                
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

            if (tokensLine.ColumnsLayout == ColumnsLayout.CobolReferenceFormat && tokensLine.Text.Length > 80)
            {
                tokensLine.AddDiagnostic(MessageCode.SyntaxErrorInParser,
                    tokensLine.Indicator.StartIndex, tokensLine.Indicator.EndIndex, "Line exceed 80 chars");
            }

            // Create a stateful line scanner, and iterate over the tokens
            Scanner scanner = new Scanner(line, startIndex, lastIndex, tokensLine, compilerOptions, true, multiStringConcatBitPosition);
            Token nextToken = null;
            while((nextToken = scanner.GetNextToken()) != null)
            {
                if (nextToken.TokenType == TokenType.AlphanumericLiteral && (!nextToken.HasOpeningDelimiter || !nextToken.HasClosingDelimiter))
                {
                    tokensLine.AddDiagnostic(MessageCode.SyntaxErrorInParser,
                        tokensLine.Indicator.StartIndex, tokensLine.Indicator.EndIndex, "Literal is not correctly delimited.");
                }
                tokensLine.AddToken(nextToken);
            }    
        }

#if EUROINFO_RULES
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
            foreach (string candidateName in significantPart.Split(' ', ',')) {
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

        private static bool IsDebuggingModeActive(MultilineScanState lineScanState, string lineSourceText)
        {
            System.Diagnostics.Debug.Assert(lineScanState != null);
            System.Diagnostics.Debug.Assert(lineSourceText != null);

            if (!lineScanState.WithDebuggingMode)
            {
                /*
                 * DebuggingMode is inactive but REPLACE directives are a special case.
                 * As replacing should happen before scanning, debug indicators are irrelevant
                 * and REPLACE directives are active no matter what.
                 *
                 * So an inactive debug line has to be parsed as regular source if it participates
                 * in a REPLACE directive.
                 */
                return lineScanState.InsideReplaceDirective || StartsWithReplace();
            }

            //DebuggingMode is active, debug line is considered as regular source line.
            return true;

            bool StartsWithReplace()
            {
                string replaceKeyword = TokenUtils.GetTokenStringFromTokenType(TokenType.REPLACE);
                return lineSourceText.StartsWith(replaceKeyword, StringComparison.OrdinalIgnoreCase);
            }
        }

        /// <summary>
        /// Scan a group of continuation lines
        /// </summary>
        public static void ScanTokensLineContinuationGroup(IList<TokensLine> continuationLinesGroup, MultilineScanState initialScanState, ColumnsLayout format, TypeCobolOptions compilerOptions, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
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

            // Scan the first lines (until Source is encountered)
            var scanState = initialScanState;
            int i = 0;
            bool hasSource = false;
            for (; i < continuationLinesGroup.Count; i++)
            {
                TokensLine line = continuationLinesGroup[i];

                // Line's scan state is set by Scanner.ScanTokensLine, so use local variable scanState instead of line's property
                if (line.Type == CobolTextLineType.Source || (line.Type == CobolTextLineType.Debug && IsDebuggingModeActive(scanState, line.SourceText)))
                {
                    hasSource = true;
                    break;
                }

                Scanner.ScanTokensLine(line, scanState, compilerOptions, copyTextNameVariations);
                scanState = line.ScanState;
            }

            // No source line found, it was not a continuation group...
            if (!hasSource) return;

            // Initialize the continuation text with the complete source text of the first source line
            int firstSourceLineIndex = i;
            TokensLine firstSourceLine = continuationLinesGroup[firstSourceLineIndex];
            string concatenatedLine = firstSourceLine.SourceText;
            textAreasForOriginalLinesInConcatenatedLine[firstSourceLineIndex] = new TextArea(TextAreaType.Source, 0, concatenatedLine.Length -1);
            startIndexForTextAreasInOriginalLines[firstSourceLineIndex] = firstSourceLine.Source.StartIndex;
            offsetForLiteralContinuationInOriginalLines[firstSourceLineIndex] = 0;

            // Find the index of the last ContinuationLine in the group
            int lastContinuationLineIndex;
            for (lastContinuationLineIndex = continuationLinesGroup.Count - 1; lastContinuationLineIndex > firstSourceLineIndex; lastContinuationLineIndex--)
            {
                if (continuationLinesGroup[lastContinuationLineIndex].Type == CobolTextLineType.Continuation)
                {
                    break;
                }
            }
            // Positions in the concatenatedLine string where multi strings must be applied.
            List<int> multiStringConcatPositions = new List<int>(lastContinuationLineIndex - firstSourceLineIndex);

            // Iterate over the continuation lines (some blank lines or comment lines may come in-between)
            // => build a character string representing the complete continuation text along the way
            int previousLastIndex = firstSourceLine.Source.EndIndex;
            for (i = firstSourceLineIndex + 1; i <= lastContinuationLineIndex; i++)
            {
                bool isLastLine = i == lastContinuationLineIndex;
                TokensLine continuationLine = continuationLinesGroup[i];
                int startIndex = continuationLine.Source.StartIndex;
                int lastIndex = continuationLine.Source.EndIndex;
                string line = continuationLine.Text;

                // Not a continuation line
                if (continuationLine.Type != CobolTextLineType.Continuation)
                {
                    Scanner.ScanTokensLine(continuationLine, scanState, compilerOptions, copyTextNameVariations);
                    scanState = continuationLine.ScanState;
                    continue;
                }

                // 1. Match and remove all blank characters at the beginning of the continuation line
                int startOfContinuationIndex = startIndex;
                for (; startOfContinuationIndex <= lastIndex && line[startOfContinuationIndex] == ' '; startOfContinuationIndex++) { }
                if (startOfContinuationIndex > startIndex)
                {
                    Token whitespaceToken = new Token(TokenType.SpaceSeparator, startIndex, startOfContinuationIndex - 1, continuationLine);
                    continuationLine.SourceTokens.Add(whitespaceToken);
                }
                if (startOfContinuationIndex < 4)
                {
                    continuationLine.AddDiagnostic(MessageCode.AreaAOfContinuationLineMustBeBlank, startOfContinuationIndex, startOfContinuationIndex);
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
                    TokensLine temporaryTokensLine = TokensLine.CreateVirtualLineForInsertedToken(firstSourceLine.LineIndex, concatenatedLine, ColumnsLayout.FreeTextFormat);
                    Scanner.ScanTokensLine(temporaryTokensLine, initialScanState, compilerOptions, copyTextNameVariations);
                    Token lastTokenOfConcatenatedLineSoFar = temporaryTokensLine.SourceTokens[temporaryTokensLine.SourceTokens.Count - 1];

                    // Check if the last token so far is an alphanumeric or national literal
                    if (lastTokenOfConcatenatedLineSoFar.TokenFamily == TokenFamily.AlphanumericLiteral)
                    {
                        if (!lastTokenOfConcatenatedLineSoFar.HasClosingDelimiter)
                        {
                            // In CobolReferenceFormat, add remaining spaces at the end of the original line into the AlphanumericLiteral
                            // Does not apply to FreeTextFormat as lines do not have maximum length
                            if (format == ColumnsLayout.CobolReferenceFormat)
                            {
                                string padding = new string(' ', 71 - previousLastIndex);
                                concatenatedLine += padding;
                            }

                            // check delimiters
                            const char QUOTE = '\'';
                            const char DOUBLE_QUOTE = '"';
                            char startDelimiter = line[startOfContinuationIndex];
                            bool isBadStartDelimiter = startDelimiter != lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter;
                            char endDelimiter = QUOTE;  // default value
                            bool isBadEndDelimiter = false;
                            if (isLastLine)
                            {
                                // check closing delimiter of the last continuation line
                                string lastLine = continuationLine.SourceText.TrimEnd();
                                int pos = lastLine.LastIndexOf(startDelimiter);
                                if (pos > lastLine.IndexOf(startDelimiter))
                                {
                                    // delimiter is also present near the end
                                    lastLine = lastLine.Substring(0, pos + 1);
                                }
                                else
                                {
                                    // . is present at the end
                                    pos = lastLine.Length - 1;
                                    if (lastLine[pos] == '.')
                                    {
                                        lastLine = lastLine.Substring(0, pos).TrimEnd();
                                    }
                                }

                                endDelimiter = lastLine[lastLine.Length - 1];
                                isBadEndDelimiter = endDelimiter != lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter;
                            }

                            if (isBadStartDelimiter || isBadEndDelimiter)
                            {
                                if (startDelimiter != QUOTE && startDelimiter != DOUBLE_QUOTE)
                                {
                                    // no valid starting delimiter
                                    continuationLine.AddDiagnostic(MessageCode.SyntaxErrorInParser,
                                        startOfContinuationIndex, startOfContinuationIndex + 1, 
                                        "Starting delimiter of the continuation line is missing.");
                                    offsetForLiteralContinuation = 0;
                                }
                                else if (endDelimiter != QUOTE && endDelimiter != DOUBLE_QUOTE)
                                {
                                    // no valid closing delimiter
                                    continuationLine.AddDiagnostic(MessageCode.SyntaxErrorInParser,
                                        startOfContinuationIndex, startOfContinuationIndex + 1,
                                        "Closing delimiter of the continuation line is missing.");
                                    offsetForLiteralContinuation = 0;

                                }
                                else
                                {
                                    // different delimiters between start and end delimiters
                                    continuationLine.AddDiagnostic(MessageCode.InvalidDelimiterForContinuationLine,
                                        startOfContinuationIndex, startOfContinuationIndex + 1,
                                        lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter);
                                    // Use the first quotation mark to avoid a complete mess while scanning the rest of the line
                                    offsetForLiteralContinuation = 0;
                                }
                            }
                        }

                        //// The continuation of the literal begins with the character immediately following the quotation mark.
                        if (line[startOfContinuationIndex] == lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter)
                        {
                            offsetForLiteralContinuation = 1;

                            // If an alphanumeric literal that is to be continued on the next line has as its last character a quotation mark in column 72, 
                            // the continuation line must start with two consecutive quotation marks.
                            // In ColumnsLayout.CobolReferenceFormat we just check that the quotation mark is the last character of the line.
                            if (lastTokenOfConcatenatedLineSoFar.HasClosingDelimiter)
                            {
                                bool continuationStartsWithTwoDelimiters = ((startOfContinuationIndex + 1) <= lastIndex && line[startOfContinuationIndex + 1] == lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter);
                                bool previousDelimiterIsNotAtEnd = (lastTokenOfConcatenatedLineSoFar.EndColumn + CobolFormatAreas.Indicator) != CobolFormatAreas.End_B;

                                if (format == ColumnsLayout.CobolReferenceFormat ? continuationStartsWithTwoDelimiters && previousDelimiterIsNotAtEnd : !continuationStartsWithTwoDelimiters)
                                {
                                    continuationLine.AddDiagnostic(MessageCode.InvalidFirstTwoCharsForContinuationLine, startOfContinuationIndex, startOfContinuationIndex + 1, lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter,
                                        format == ColumnsLayout.CobolReferenceFormat
                                        ? "in column 72"
                                        : "at the last column");
                                    // Use the first quotation mark to avoid a complete mess while scanning the rest of the line
                                    offsetForLiteralContinuation = 0;
                                }
                                else
                                {
                                    bool isQuoteInsertedInString = continuationStartsWithTwoDelimiters && (format == ColumnsLayout.CobolReferenceFormat ? !previousDelimiterIsNotAtEnd : true);
                                    if (!isQuoteInsertedInString)
                                    { // This is a multi string concatenation, so remember concatenation position in the whole string
                                        multiStringConcatPositions.Add(concatenatedLine.Length);
                                        // Here also use the first quotation mark.
                                        offsetForLiteralContinuation = 0;
                                        // Check error cases
                                        if ((startOfContinuationIndex + 1) == (int)CobolFormatAreas.Begin_A && format == ColumnsLayout.CobolReferenceFormat)
                                        { // A blank is missing before character """ in column 8. A blank is assumed                                            
                                            continuationLine.AddDiagnostic(MessageCode.DotShouldBeFollowedBySpace,
                                                startOfContinuationIndex, startOfContinuationIndex + 1,
                                                lastTokenOfConcatenatedLineSoFar.ExpectedClosingDelimiter, (int)CobolFormatAreas.Begin_A);
                                        }
                                        if ((startOfContinuationIndex + 1) < (int)CobolFormatAreas.Begin_B && format == ColumnsLayout.CobolReferenceFormat)
                                        { // The literal must be in Area B
                                            continuationLine.AddDiagnostic(MessageCode.AreaAOfContinuationLineMustBeBlank, startOfContinuationIndex, startOfContinuationIndex + 1);
                                        }
                                    }
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
                previousLastIndex = lastIndex;
            }

            // Scan the complete continuation text as a whole
            TokensLine virtualContinuationTokensLine = TokensLine.CreateVirtualLineForInsertedToken(firstSourceLine.LineIndex, concatenatedLine, ColumnsLayout.FreeTextFormat);
            // Create a BitArray of Multi String Positions based on the length of the concatenated line.
            BitArray multiStringConcatBitPosition = null;
            if (multiStringConcatPositions.Count > 0)
            {
                multiStringConcatBitPosition = new BitArray(concatenatedLine.Length);
                foreach (int pos in multiStringConcatPositions)
                {
                    multiStringConcatBitPosition.Set(pos, true);
                }
            }
            Scanner.ScanTokensLine(virtualContinuationTokensLine, initialScanState, compilerOptions, copyTextNameVariations, multiStringConcatBitPosition);

            // Then attribute each token and diagnostic to its corresponding tokens line
            i = firstSourceLineIndex;
            TokensLine originalLine = null;
            TextArea textAreaForOriginalLine;
            int concatenatedLineToOriginalLineOffset;
            InitLine();
            foreach (var token in virtualContinuationTokensLine.SourceTokens)
            {
                TryAddTokenToCurrentLine();

                void TryAddTokenToCurrentLine()
                {
                    System.Diagnostics.Debug.Assert(token.StartIndex >= textAreaForOriginalLine.StartIndex);
                    if (token.StopIndex <= textAreaForOriginalLine.EndIndex)
                    {
                        // The token is fully included in the current line
                        AddTokenToCurrentLine();
                    }
                    else if (token.StartIndex <= textAreaForOriginalLine.EndIndex)
                    {
                        // The token starts on the current line but ends after the end of it, split it into multiple ContinuationTokens !
                        SplitToken();
                    }
                    else
                    {
                        //The token starts on the next line
                        AdvanceToNextContinuationLine();
                        TryAddTokenToCurrentLine();
                    }

                    void AddTokenToCurrentLine()
                    {
                        int startIndexInOriginalLine = token.StartIndex + concatenatedLineToOriginalLineOffset;
                        int stopIndexInOriginalLine = token.StopIndex + concatenatedLineToOriginalLineOffset;

                        token.CorrectTokensLine(originalLine, startIndexInOriginalLine, stopIndexInOriginalLine);
                        originalLine.AddToken(token);
                        virtualContinuationTokensLine.CopyDiagnosticsForToken(token, originalLine);
                    }

                    void SplitToken()
                    {
                        // First, create a continued token on the current line
                        bool isContinuedOnNextLine = true;
                        CreateAndAddFirstContinuationToken();

                        // The token may be split across 2 or more lines
                        // So keep creating continuation tokens until the end of the token is reached
                        do
                        {
                            AdvanceToNextContinuationLine();
                            isContinuedOnNextLine = token.StopIndex > textAreaForOriginalLine.EndIndex;
                            CreateAndAddFollowingContinuationToken();
                        }
                        while (isContinuedOnNextLine);

                        void CreateAndAddFirstContinuationToken()
                        {
                            CreateAndAddContinuationToken(false);

                            // Copy diagnostics on the first line only
                            virtualContinuationTokensLine.CopyDiagnosticsForToken(token, originalLine);
                        }

                        void CreateAndAddFollowingContinuationToken() => CreateAndAddContinuationToken(true);

                        void CreateAndAddContinuationToken(bool isContinuationFromPreviousLine)
                        {
                            int startIndexInOriginalLine;
                            if (isContinuationFromPreviousLine)
                            {
                                startIndexInOriginalLine = startIndexForTextAreasInOriginalLines[i] - offsetForLiteralContinuationInOriginalLines[i];
                            }
                            else
                            {
                                startIndexInOriginalLine = token.StartIndex + concatenatedLineToOriginalLineOffset;
                            }
                            int stopIndexInOriginalLine;
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
                        }
                    }

                    void AdvanceToNextContinuationLine()
                    {
                        // Advance index until a new ContinuationLine is found
                        do { i++; } while (continuationLinesGroup[i].Type != CobolTextLineType.Continuation);
                        InitLine();
                    }
                }
            }

            void InitLine()
            {
                // Update current scanState before jumping onto new line
                scanState = originalLine?.ScanState ?? initialScanState; // On first call, originalLine is not set and scanState is reset to initialScanState.

                // Update loop variables for new current line
                originalLine = continuationLinesGroup[i];
                originalLine.InitializeScanState(scanState);
                textAreaForOriginalLine = textAreasForOriginalLinesInConcatenatedLine[i];
                concatenatedLineToOriginalLineOffset = startIndexForTextAreasInOriginalLines[i] - textAreaForOriginalLine.StartIndex;
            }

            // Finally, scan remaining lines of the group
            scanState = virtualContinuationTokensLine.ScanState;
            for (i = lastContinuationLineIndex + 1; i < continuationLinesGroup.Count; i++)
            {
                var line = continuationLinesGroup[i];
                Scanner.ScanTokensLine(line, scanState, compilerOptions, copyTextNameVariations);
                scanState = line.ScanState;
            }
        }

        /// <summary>
        /// Scan an isolated token in the given context.
        /// </summary>
        public static Token ScanIsolatedToken(string tokenText, [NotNull] MultilineScanState scanContext, TypeCobolOptions scanOptions, ColumnsLayout layout, out Diagnostic error)
        {
            TokensLine tempTokensLine = TokensLine.CreateVirtualLineForInsertedToken(0, tokenText, layout);
            tempTokensLine.InitializeScanState(scanContext);

            Token candidateToken;
            if (tokenText.Length > 0)
            {
                Scanner tempScanner = new Scanner(tokenText, 0, tokenText.Length - 1, tempTokensLine, scanOptions, false);
                candidateToken = tempScanner.GetNextToken();
            }
            else
            {
                //Create an empty SpaceSeparator token.
                candidateToken = new Token(TokenType.SpaceSeparator, 0, -1, tempTokensLine);
            }

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

        private readonly CobolLanguageLevel _targetLanguageLevel;
        
        /// <summary>
        /// Bit array of Multi String concatenation positions if any.
        /// </summary>
        private readonly BitArray _multiStringConcatBitPosition;

        private SqlScanner _sqlScanner;

        private bool InterpretDoubleColonAsQualifiedNameSeparator
        {
            get
            {
                var currentState = tokensLine.ScanState;
                return !compilerOptions.IsCobolLanguage  //No QualifiedNameSeparator allowed in pure Cobol
                       && !currentState.InsidePseudoText //In TypeCobol, no QualifiedNameSeparator allowed in pseudoText
                       && !currentState.InsideCopy;      //In TypeCobol, no QualifiedNameSeparator allowed in copies 
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="line"></param>
        /// <param name="startIndex"></param>
        /// <param name="lastIndex"></param>
        /// <param name="tokensLine"></param>
        /// <param name="compilerOptions"></param>
        /// <param name="beSmartWithLevelNumber"></param>
        /// <param name="multiStringConcatBitPosition">Bit array of Multi String concatenation positions</param>
        public Scanner(string line, int startIndex, int lastIndex, TokensLine tokensLine, TypeCobolOptions compilerOptions, bool beSmartWithLevelNumber = true, BitArray multiStringConcatBitPosition = null)
            : base(line, startIndex, lastIndex, tokensLine, compilerOptions)
        {
            this._targetLanguageLevel = compilerOptions.IsCobolLanguage ? CobolLanguageLevel.Cobol85 : CobolLanguageLevel.TypeCobol;
            this._multiStringConcatBitPosition = multiStringConcatBitPosition;
            this.BeSmartWithLevelNumber = beSmartWithLevelNumber;
        }

        public override Token GetTokenStartingFrom(int startIndex)
        {
            // Cannot read past end of line or before its beginning
            if (startIndex < 0 || startIndex > lastIndex)
            {
                return null;
            }

            // Start scanning at the given index
            currentIndex = startIndex;
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
                        return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PeriodSeparator, MessageCode.DotShouldBeFollowedBySpace, false);
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
            else if (currentState.AfterExecStatementText || currentState.InsideSql)
            {
                tryScanExecStatementText = true;
            }
            // Previous state tests show that we should try to scan an exec statement text at this point
            if (tryScanExecStatementText)
            {
                return ScanSqlCodeOrExecStatementTextOrExecSqlInclude(startIndex);
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


            if (currentState.InsideMultilineComments)
            {
                // We are inside a Multiline Comments
                // if there is no Multiline Comments end marckup "*>>" then create a new Comment Token until the "*>>"
                if (line.Length > currentIndex + 2 && line[currentIndex] == '%' && line[currentIndex + 1] == '>' && line[currentIndex + 2] == '>')
                {
                    // We are in the case of a Formalize Comment stop with the '*' on column other than 7 wich is forbidden
                    tokensLine.AddDiagnostic(MessageCode.WrongMultilineCommentMarckupPosition,
                        startIndex,
                        startIndex + 2);
                    // consume the * char and the three < chars
                    currentIndex += 3;
                    return new Token(TokenType.MULTILINES_COMMENTS_STOP, startIndex, startIndex + 2, tokensLine);
                }
                else if (line[currentIndex] == '>' && line[currentIndex - 1] == '%' && line.Length > currentIndex + 1 && line[currentIndex + 1] == '>')
                {
                    currentIndex += 2;
                    return new Token(TokenType.MULTILINES_COMMENTS_STOP, startIndex-1, startIndex + 1, tokensLine);
                }
                else
                {
                    return ScanUntilDelimiter(startIndex, TokenType.CommentLine, "%>>");
                }
            }

            // --- switch dedicated to formalized Comments as they are not compatible with TypeCobol grammar ---
            if (currentState.InsideFormalizedComment)
            {
                switch (line[startIndex])
                {
                    case ' ':
                        //SpaceSeparator=1,
                        return ScanWhitespace(startIndex);
                    case '-':
                        currentIndex++;
                        return new Token(TokenType.MinusOperator, startIndex, currentIndex - 1, tokensLine);
                    case '@':
                        currentIndex++;
                        return new Token(TokenType.AT_SIGN, startIndex, currentIndex - 1, tokensLine); 
                    case ':':
                        currentIndex++;
                        return new Token(TokenType.ColonSeparator, startIndex, currentIndex - 1, tokensLine);
                    case '*':
                        currentIndex ++;
                        return new Token(TokenType.MultiplyOperator, startIndex, currentIndex - 1, tokensLine);
                    case '%':
                        if ((line.Length > currentIndex + 3) && line[currentIndex + 1] == '>' && line[currentIndex + 2] == '>' && line[currentIndex + 3] == '>')
                        {
                            // We are in the case of a Formalize Comment stop with the '*' on column other than 7 wich is forbidden
                            tokensLine.AddDiagnostic(MessageCode.WrongFormalizedCommentMarckupPosition,
                                startIndex,
                                startIndex + 3);
                            // consume the * char and the three < chars
                            currentIndex += 4;
                            return new Token(TokenType.FORMALIZED_COMMENTS_STOP, startIndex, startIndex + 3, tokensLine);
                        }
                        currentIndex++;
                        return new Token(TokenType.InvalidToken, startIndex, currentIndex - 1, tokensLine);
                    case '>':
                        if ((line.Length > currentIndex + 2) && line[currentIndex - 1] == '%' && line[currentIndex + 1] == '>' && line[currentIndex + 2] == '>')
                        {
                            // We are in the case of a Formalize Comment stop with the '*' on column 7
                            // consume the three > chars
                            currentIndex += 3;
                            return new Token(TokenType.FORMALIZED_COMMENTS_STOP, startIndex-1, startIndex + 2, tokensLine);
                        }
                        currentIndex++;
                        return new Token(TokenType.GreaterThanOperator, startIndex, currentIndex - 1, tokensLine);
                    default:
                        // If the previous significant Token is an At Sign then the following word have to be a field keyword
                        // If the previous significant Token is a Minus Operator then the following have to be a key in case of Params field
                        if (tokensLine.ScanState.LastSignificantToken.TokenType == TokenType.AT_SIGN ||
                            (tokensLine.ScanState.LastSignificantToken.TokenType == TokenType.MinusOperator &&
                             tokensLine.ScanState.InsideParamsField))
                        {
                            Token token = ScanCharacterString(startIndex);
                            if (tokensLine.ScanState.LastSignificantToken.TokenType == TokenType.AT_SIGN
                                && token.TokenFamily != TokenFamily.FormalizedCommentsFamily)
                            {
                                tokensLine.AddDiagnostic(MessageCode.WrongFormalizedCommentKeyword, token);
                            }
                            else if (tokensLine.ScanState.LastSignificantToken.TokenType == TokenType.MinusOperator
                                && token.TokenFamily == TokenFamily.FormalizedCommentsFamily)
                            {
                                token.CorrectType(TokenType.UserDefinedWord);
                            }
                            return token;
                        }
                        return ScanUntilDelimiter(startIndex, TokenType.FORMALIZED_COMMENTS_VALUE, "%>>>");
                }
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
                    if (tokensLine.ScanState.SpecialNames.DecimalPointIsComma)
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
                case '?':
                    //QUESTION_MARK=460,
                    //TypeCobol
                    return ScanOneCharFollowedBySpace(startIndex, TokenType.QUESTION_MARK, MessageCode.QuestionMarkShouldBeFollowedBySpace);         
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
                case '%':
                    if (line.Length >= currentIndex + 2 && line[currentIndex + 1] == '>' && line[currentIndex + 2] == '>')
                    {
                        if (line.Length > currentIndex + 3 && line[currentIndex + 3] == '>')
                        {
                            // It is a Formalized Comment start that is not well positionned
                            tokensLine.AddDiagnostic(MessageCode.WrongFormalizedCommentMarckupPosition,
                                startIndex,
                                startIndex + 3);
                        }
                        else
                        {
                            // It is a Multilines Comment start that is not well positionned
                            tokensLine.AddDiagnostic(MessageCode.WrongMultilineCommentMarckupPosition,
                                startIndex,
                                startIndex + 2);
                        }
                    }
                    // Multiline Comments or Formalized Comments start should begin on column 7
                    if (line.Length > currentIndex + 2 && line[currentIndex + 1] == '<' && line[currentIndex + 2] == '<')
                    {
                        if (line.Length > currentIndex + 3 && line[currentIndex + 3] == '<')
                        {
                            // It is a Formalized Comment start that is not well positioned
                            tokensLine.AddDiagnostic(MessageCode.WrongFormalizedCommentMarckupPosition,
                                startIndex,
                                startIndex + 3);
                            // consume the * char and the three < chars
                            currentIndex += 4;
                            return new Token(TokenType.FORMALIZED_COMMENTS_START, startIndex, startIndex + 3, tokensLine);
                        }
                        else
                        {
                            // It is a Multiline Comment start that is not well positioned
                            tokensLine.AddDiagnostic(MessageCode.WrongMultilineCommentMarckupPosition,
                                startIndex,
                                startIndex + 2);
                            // We are in the case of a Multiline Comment start
                            // consume the * char and the two < chars
                            currentIndex += 3;
                            return new Token(TokenType.MULTILINES_COMMENTS_START, startIndex, startIndex + 2, tokensLine);
                        }
                    }

                    currentIndex += 1;
                    return new Token(TokenType.InvalidToken, startIndex, startIndex, tokensLine);
                case '.':
                    //PeriodSeparator=7,
                    // p46: A separator period is composed of a period followed by a space.
                    if(tokensLine.ScanState.SpecialNames.DecimalPointIsComma)
                    {
                        return ScanOneCharFollowedBySpace(startIndex, TokenType.PeriodSeparator, MessageCode.DotShouldBeFollowedBySpace);
                    }
                    else
                    {
                        //IntegerLiteral = 27,
                        //DecimalLiteral = 28,
                        //FloatingPointLiteral = 29,
                        return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PeriodSeparator, MessageCode.DotShouldBeFollowedBySpace);
                    }
                case ':':
                    // -- TypeCobol specific syntax --
                    // QualifiedNameSeparator => qualifierName::qualifiedName
                    if (currentIndex < lastIndex && line[currentIndex + 1] == ':' && InterpretDoubleColonAsQualifiedNameSeparator)
                    {
                        // consume two :: chars
                        currentIndex += 2;
                        return new Token(TokenType.QualifiedNameSeparator, startIndex, startIndex + 1, tokensLine);
                    }
                    // --
                    // The COPY statement with REPLACING phrase can be used to replace parts of words. 
                    // By inserting a dummy operand delimited by colons into the program text, the compiler will replace the dummy operand with the desired text. 
                    int patternEndIndex;
                    if (ScannerUtils.CheckForPartialCobolWordPattern(line, startIndex, lastIndex, InterpretDoubleColonAsQualifiedNameSeparator, out patternEndIndex))
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
                    }
                    else if (line[currentIndex - 1] == '%' && line[currentIndex + 1] == '<')
                    {
                        if (line.Length > currentIndex + 2 && line[currentIndex + 2] == '<')
                        {
                            // We are in the case of a Formalize Comment start
                            // consume the three < chars
                            currentIndex += 3;
                            return new Token(TokenType.FORMALIZED_COMMENTS_START, startIndex-1, startIndex + 2, tokensLine);
                        }
                        else
                        {
                            // We are in the case of a Multiline comments start
                            currentIndex += 2;
                            return new Token(TokenType.MULTILINES_COMMENTS_START, startIndex-1, startIndex + 1, tokensLine);
                        }
                    }
                    else
                    {
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
                    }
                    else {
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
                    return ScanAlphanumericLiteral(startIndex, TokenType.AlphanumericLiteral, _multiStringConcatBitPosition);
                case 'X':
                case 'x':
                    //HexadecimalAlphanumericLiteral = 22,
                    // p45: X" Opening delimiter for a hexadecimal format alphanumeric literal
                    // X’ Opening delimiter for a hexadecimal format alphanumeric literal
                    if (currentIndex < lastIndex && (line[currentIndex + 1] == '"' || line[currentIndex + 1] == '\''))
                    {
                        // consume X char
                        currentIndex++;
                        return ScanAlphanumericLiteral(startIndex, TokenType.HexadecimalAlphanumericLiteral, _multiStringConcatBitPosition);
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
                        return ScanAlphanumericLiteral(startIndex, TokenType.NullTerminatedAlphanumericLiteral, _multiStringConcatBitPosition);
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
                        return ScanAlphanumericLiteral(startIndex, TokenType.NationalLiteral, _multiStringConcatBitPosition);
                    }
                    else if (currentIndex < lastIndex     && (line[currentIndex + 1] == 'X' || line[currentIndex + 1] == 'x') &&
                             currentIndex < (lastIndex+1) && (line[currentIndex + 2] == '"' || line[currentIndex + 2] == '\''))
                    {
                        // consume N and X chars
                        currentIndex += 2;
                        return ScanAlphanumericLiteral(startIndex, TokenType.HexadecimalNationalLiteral, _multiStringConcatBitPosition);
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
                        return ScanAlphanumericLiteral(startIndex, TokenType.DBCSLiteral, _multiStringConcatBitPosition);
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
                // 000010-000050 is interpreted as a UserDefinedWord
                //Depending on the context it could be
                // - a range of numbers (for DELETE_CD)
                //     - See sequence-number-fields of compiler directive statements
                // - a paragraph name
                //The scanner will then always match 000010-000050 as a UserDefinedWord.
                //The check to be sure that a variable name contains at least alphabetic char
                //is done at CodeElement level.
                
                // CURRENT behavior of method ScanNumericLiteral :
                // This method matches chars as long as they are characters allowed in a numeric literal.
                // Then, it checks the format of the matched string, and returns either a NumericLiteral or Invalid token.
                // If we write 123ABC, ScanNumericLiteral will match 123ABC.

                // SOLUTION:
                // If a token is starting with a digit, we first try to scan it as a numeric literal (most common case).
                // Then we check if the character directly following the numeric literal is a valid character for a user defined word.
                // If it is not valid (space, separator ...), we simply return the numeric literal token.
                // If it is valid, we reset the state of the scanner and try to scan this word as a character string 
                // (keyword, user defined word ...).
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
                    Token numericLiteralToken = ScanCobolNumericLiteral(startIndex);


                    // 2. a FloatingPointLiteral contains a character '.' or ',' which is invalid
                    // in a UserDefinedWord or a keyword, so we can return this token directly.
                    if (numericLiteralToken.TokenType == TokenType.FloatingPointLiteral)
                    {
                        return numericLiteralToken;
                    }

                    //TODO : Paragraph and section with name like "123" are currently not handled 

                    if (numericLiteralToken.TokenType != TokenType.InvalidToken 
                        && !((currentIndex <= lastIndex) && CobolChar.IsCobolWordChar(line[currentIndex])))
                    {
                        // 3. Return a numeric literal token because there is no valid Cobol char that follows this token
                        return numericLiteralToken;
                    }
                    else
                    {
                        //4. Handle UserDefinedWord like  123-456, 123X or 123-X

                        //ScanNumericLiteral can return InvalidToken, in this case it's better to rescan it as a UserDefinedWord
                        // Reset scanner state
                        currentIndex = saveCurrentIndex;
                        if (numericLiteralToken.TokenType == TokenType.InvalidToken)
                        {
                            tokensLine.ClearDiagnosticsForToken(numericLiteralToken);
                        }

                        //Try to scan a Cobol character string: UserDefinedWord or PartialCobolWord
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
                    tokensLine.AddDiagnostic(messageCode, invalidToken, line[currentIndex], currentIndex + 1);
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
                return ScanCobolNumericLiteral(startIndex);
            }
            else if((tokenType == TokenType.PlusOperator || tokenType == TokenType.MinusOperator) && 
                    line[currentIndex + 1] == (tokensLine.ScanState.SpecialNames.DecimalPointIsComma ? ',' : '.'))
            {
                return ScanCobolNumericLiteral(startIndex);
            }
            else
            {
                // consume one char and register an error because the following char is missing
                // even if the space is missing, try to match the expected tokenType
                currentIndex++;
                if (spaceAfterIsMandatory) {
                    Token invalidToken = new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
                    tokensLine.AddDiagnostic(messageCode, invalidToken, line[currentIndex], currentIndex+1);
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

        private Token ScanCobolNumericLiteral(int startIndex)
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
            if (tokensLine.ScanState.SpecialNames.DecimalPointIsComma)
            {
                decimalPoint = ',';
            }

            Token token = ScanNumericLiteral(startIndex, decimalPoint);
            switch (token.TokenType)
            {
                case TokenType.IntegerLiteral:
                    CheckIntegerLiteral();
                    break;
                case TokenType.FloatingPointLiteral:
                    CheckFloatingPointLiteral();
                    break;
            }

            return token;

            void CheckIntegerLiteral()
            {
                Debug.Assert(token.LiteralValue is IntegerLiteralTokenValue);
                var literalValue = (IntegerLiteralTokenValue)token.LiteralValue;

                //Check IntegerLiteral range
                if (literalValue.Number < long.MinValue || literalValue.Number > long.MaxValue)
                {
                    //Out of range
                    this.tokensLine.AddDiagnostic(MessageCode.SyntaxErrorInParser, token, "Number is too big : " + literalValue.Number);
                }

                //Disambiguate IntegerLiteral from LevelNumber
                if (!literalValue.HasSign && BeSmartWithLevelNumber && tokensLine.ScanState.InsideDataDivision)
                {
                    if (tokensLine.ScanState.AtBeginningOfSentence || GuessIfCurrentTokenIsLevelNumber())
                    {
                        token.CorrectType(TokenType.LevelNumber);
                    }

                    //This method is here to help recognize LevelNumbers when PeriodSeparator has been forgotten at the end of previous data definition.
                    bool GuessIfCurrentTokenIsLevelNumber()
                    {
                        var lastSignificantToken = tokensLine.ScanState.LastSignificantToken;
                        var beforeLastSignificantToken = tokensLine.ScanState.BeforeLastSignificantToken;

                        bool currentTokenIsAtBeginningOfNewLine = token.Line > lastSignificantToken.Line;
                        bool currentTokenIsBeforeAreaB = token.Column < 12;

                        //Either a continuation line or we are still on the same line --> not a LevelNumber
                        if (!currentTokenIsAtBeginningOfNewLine || tokensLine.HasTokenContinuationFromPreviousLine)
                            return false;

                        //Literals can't be written outside of AreaB so it must be a LevelNumber
                        if (currentTokenIsBeforeAreaB)
                            return true;

                        //Try to guess if it is a LevelNumber or Literal depending on previous tokens
                        bool currentTokenIsExpectedToBeALiteral = false;
                        switch (lastSignificantToken.TokenType)
                        {
                            case TokenType.OCCURS:
                            case TokenType.VALUE:
                            case TokenType.VALUES:
                            case TokenType.THROUGH:
                            case TokenType.THRU:
                                currentTokenIsExpectedToBeALiteral = true;
                                break;
                            case TokenType.IS:
                            case TokenType.ARE:
                                currentTokenIsExpectedToBeALiteral =
                                    beforeLastSignificantToken.TokenType == TokenType.VALUE ||
                                    beforeLastSignificantToken.TokenType == TokenType.VALUES;
                                break;
                        }

                        if (!currentTokenIsExpectedToBeALiteral)
                        {
                            /*
                             * Here we still have an ambiguity between multiple consecutive IntegerLiteral and LevelNumber like in this kind of declarations :
                             *    01 integers PIC 99.
                             *       88 odd  VALUES 01 03 05
                             *                      07 09.
                             *       88 even VALUES 02 04 06
                             *                      08.
                             * 07 and 08 are literals but we actually can't distinguish between a following literal and a LevelNumber. We assume the code
                             * is syntactically correct more often than not so we choose in that case to consider the token as a Literal.
                             *
                             * 'ZERO', 'ZEROS' and 'ZEROES' figurative constants can also be used among values so they are considered too.
                             */
                            return lastSignificantToken.TokenFamily != TokenFamily.NumericLiteral &&
                                   lastSignificantToken.TokenType != TokenType.ZERO &&
                                   lastSignificantToken.TokenType != TokenType.ZEROS &&
                                   lastSignificantToken.TokenType != TokenType.ZEROES;
                        }

                        return false;
                    }
                }
            }

            void CheckFloatingPointLiteral()
            {
                Debug.Assert(token.LiteralValue is FloatingPointLiteralTokenValue);
                var literalValue = (FloatingPointLiteralTokenValue)token.LiteralValue;

                //Check exponent length
                var exponent = literalValue.Exponent.Number;
                int exponentLength = exponent.ToString().Length - (exponent.Sign == -1 ? 1 : 0); //Check digits only, so remove '-' at beginning of negative numbers
                if (exponentLength > 2)
                {
                    tokensLine.AddDiagnostic(MessageCode.InvalidExponentInFloatingPointLiteral, token);
                    token.CorrectType(TokenType.InvalidToken);
                }
            }
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
                var patternEndIndex = endIndex;
                var replaceStartIndex = line.Substring(startIndex).IndexOf(":", StringComparison.Ordinal) + startIndex;
                if (replaceStartIndex > startIndex && (patternEndIndex + 1) > replaceStartIndex && 
                    ScannerUtils.CheckForPartialCobolWordPattern(line, replaceStartIndex, lastIndex, InterpretDoubleColonAsQualifiedNameSeparator, out patternEndIndex)) 
                { //Check if there is cobol partial word inside the picture declaration. 
                    //Match the whole PictureCharecterString token as a partial cobol word. 
                    var picToken = new Token(TokenType.PartialCobolWord, startIndex, endIndex, tokensLine);
                    picToken.PreviousTokenType = TokenType.PictureCharacterString; //Save that the token was previously a picture character string token
                    return picToken;
                }
                else
                {
                    // Return a picture character string
                    return new Token(TokenType.PictureCharacterString, startIndex, endIndex, tokensLine);
                }
               
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

        private Token ScanSqlCodeOrExecStatementTextOrExecSqlInclude(int startIndex)
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
                    return new Token(TokenType.EXEC_SQL, startIndex, startIndex + 6, tokensLine);
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
                // Remove all whitespace just before END-EXEC
                for (; endIndex > startIndex && line[endIndex] == ' '; endIndex--) { }

                // If only whitespace just before END-EXEC, return a whitespace token
                if (endIndex == startIndex && line[endIndex] == ' ')
                {
                    return ScanWhitespace(startIndex);
                }
            }
            // ExecStatementText is empty
            else if (endExecIndex == startIndex)
            {
                // Directly scan END-EXEC keyword
                Token endExecToken = ScanKeywordOrUserDefinedWord(startIndex);
                System.Diagnostics.Debug.Assert(endExecToken.TokenType == TokenType.END_EXEC);
                tokensLine.ScanState.InsideSql = false;
                return endExecToken;
            }

            if (tokensLine.ScanState.AfterExecSql)
            {
                // Expect SQL code
                tokensLine.ScanState.InsideSql = true;
            }

            if (tokensLine.ScanState.InsideSql && compilerOptions.EnableSqlParsing)
            {
                // Use dedicated SQL scanner
                if (_sqlScanner == null)
                {
                    _sqlScanner = new SqlScanner(line, currentIndex, lastIndex, tokensLine, compilerOptions);
                }

                var sqlToken = _sqlScanner.GetTokenStartingFrom(currentIndex);
                currentIndex = _sqlScanner.CurrentIndex;
                return sqlToken;
            }

            // Not SQL code, consume all chars as ExecStatementText
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
                if(ScannerUtils.CheckForPartialCobolWordPattern(line, endIndex + 1, lastIndex, InterpretDoubleColonAsQualifiedNameSeparator, out patternEndIndex))
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
            if (tokensLine.ScanState.AfterFUNCTION && TokenUtils.CobolIntrinsicFunctions.IsMatch(tokenText))
            {
                tokenType = TokenType.IntrinsicFunctionName;
            }
            else if (tokensLine.ScanState.InsideFormalizedComment)
            {
                tokenType = TokenUtils.GetFormalComTokenTypeFromTokenString(tokenText);
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
                tokenType = TokenUtils.GetCobolKeywordTokenTypeFromTokenString(tokenText, _targetLanguageLevel);

                // Special cases of user defined words : 
                // - symbolic characters
                // - section and paragraph names
                if (tokenType == TokenType.UserDefinedWord)
                {
                    // p117: SYMBOLIC CHARACTERS clause
                    // symbolic-character-1 is a user-defined word and must contain at least one alphabetic character.
                    // The same symbolic-character can appear only once in a SYMBOLIC CHARACTERS clause.
                    // The symbolic character can be a DBCS user-defined word.
                    if (tokensLine.ScanState.SpecialNames.InsideSymbolicCharacterDefinitions)
                    {
                        // Symbolic character definition
                        tokenType = TokenType.SymbolicCharacter;
                        tokensLine.ScanState.SpecialNames.AddSymbolicCharacter(tokenText);
                    }
                    else if (tokensLine.ScanState.SpecialNames.SymbolicCharacters != null)
                    {
                        // Try to match a previously defined SymbolicCharacter
                        if (tokensLine.ScanState.SpecialNames.SymbolicCharacters.Contains(tokenText))
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
            Token sourceToken = new Token(TokenType.COMPILER_DIRECTIVE, startIndex, stopIndex, tokensLine);
            
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

        private Token ScanUntilDelimiter(int startIndex, TokenType tokenType, string delimiter)
        {
            int end = line.IndexOf(delimiter, StringComparison.Ordinal) - 1;
            if (end == -2)
                end = lastIndex;
            currentIndex = end + 1;
            return new Token(tokenType, startIndex, end, tokensLine);
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
                    if (ScannerUtils.CheckForPartialCobolWordPattern(line, searchIndex + 1, lastIndex, InterpretDoubleColonAsQualifiedNameSeparator, out otherPatternEndIndex))
                    {
                        endIndex = otherPatternEndIndex;
                    }
                    else { break; }
                } else { break; }
            }

            // Consume the entire the partial Cobol word
            currentIndex = endIndex + 1;

            Token t = new Token(TokenType.PartialCobolWord, startIndex, endIndex, tokensLine);
            return t;
        }
    }
}
