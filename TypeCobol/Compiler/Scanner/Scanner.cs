using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler.Scanner
{
    public class Scanner
    {
        // --- Entry points and fast path for trivial cases ---

        public static TokensLine ScanFirstLine(ITextLine textLine, bool insideDataDivision, bool decimalPointIsComma, bool withDebuggingMode, TextSourceInfo textSourceInfo, TypeCobolOptions compilerOptions)
        {
            TextLineMap textLineMap = new TextLineMap(textLine, textSourceInfo.ColumnsLayout);
            TokensLine tokensLine = new TokensLine(textLineMap, insideDataDivision, decimalPointIsComma, withDebuggingMode, textSourceInfo.EncodingForHexadecimalAlphanumericLiterals);
            ScanTokensLine(tokensLine, compilerOptions);
            return tokensLine;
        }

        public static TokensLine ScanTextLine(ITextLine textLine, TokensLine previousLine, TextSourceInfo textSourceInfo, TypeCobolOptions compilerOptions)
        {
            TextLineMap textLineMap = new TextLineMap(textLine, textSourceInfo.ColumnsLayout);
            TokensLine tokensLine = new TokensLine(textLineMap, previousLine);
            ScanTokensLine(tokensLine, compilerOptions);
            return tokensLine;
        }

        private static void ScanTokensLine(TokensLine tokensLine, TypeCobolOptions compilerOptions)
        {
            // Shorter aliases for tokensLine properties
            TextLineMap textLineMap = tokensLine.TextLineMap;
            
            // The source section of is line of text must be split into tokens    
            string line = textLineMap.TextLine.Text;
            int startIndex = textLineMap.Source.StartIndex;
            int lastIndex = textLineMap.Source.EndIndex;
                        
            // Comment line => return only one token with type CommentLine
            // Debug line => treated as a comment line if debugging mode was not activated
            if(textLineMap.Type == TextLineType.Comment ||
               (textLineMap.Type == TextLineType.Debug && !tokensLine.InitialScanState.WithDebuggingMode))
            {
                Token commentToken = new Token(TokenType.CommentLine, startIndex, lastIndex, textLineMap.TextLine);
                tokensLine.AddToken(commentToken);
                return;
            }
            // Invalid indicator, the line type is unknown => the whole line text is handled as a single invalid token
            else if (textLineMap.Type == TextLineType.Invalid)
            {
                // Invalid indicator => register an error
                tokensLine.AddDiagnostic(MessageCode.InvalidIndicatorCharacter, textLineMap.Indicator.StartIndex, textLineMap.Indicator.EndIndex, textLineMap.Indicator);

                Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lastIndex, textLineMap.TextLine);
                tokensLine.AddToken(invalidToken);
                return;
            }
            // Empty line => return immediately an empty list of tokens
            // Blank line => return only one token with type SpaceSeparator
            if(textLineMap.Type == TextLineType.Blank)
            {
                if(!String.IsNullOrEmpty(line))
                {
                    Token whitespaceToken = new Token(TokenType.SpaceSeparator, startIndex, lastIndex, textLineMap.TextLine);
                    tokensLine.AddToken(whitespaceToken);
                }
                return;
            }            
            // Handle continuation from the previous line
            else if (textLineMap.Type == TextLineType.Continuation && 
                        tokensLine.InitialScanState.LastToken != null && // no continuation is possible if there is no previous token
                        tokensLine.InitialScanState.LastToken.TokenType != TokenType.SpaceSeparator  && // no continuation is possible after a space separator
                        tokensLine.InitialScanState.LastToken.TokenType != TokenType.CommentEntry // no continuation is allowed after a comment-entry (p105 : A hyphen in the indicator area (column 7) is not permitted in comment-entries)
                )
            {
                Token lastTokenFromPreviousLine = tokensLine.InitialScanState.LastToken;

                // p54: Continuation lines
                // Any sentence, entry, clause, or phrase that requires more than one line can be
                // continued in Area B of the next line that is neither a comment line nor a blank line.
                // The line being continued is a continued line; the succeeding lines are continuation
                // lines. 
                
                // 1. Match all blank characters at the beginning of the line
                int startOfContinuationIndex = startIndex;
                for (; startOfContinuationIndex <= lastIndex && line[startOfContinuationIndex] == ' '; startOfContinuationIndex++) { }
                if(startOfContinuationIndex > startIndex)
                {
                    Token whitespaceToken = new Token(TokenType.SpaceSeparator, startIndex, startOfContinuationIndex - 1, textLineMap.TextLine);
                    tokensLine.AddToken(whitespaceToken);
                    startIndex = startOfContinuationIndex;
                }
                if(startOfContinuationIndex <= lastIndex)
                {
                    if (startOfContinuationIndex < 4)
                    {
                        tokensLine.AddDiagnostic(MessageCode.AreaAOfContinuationLineMustBeBlank, startOfContinuationIndex, startOfContinuationIndex);
                    }
                }
                else
                {
                    // Nothing but spaces on the continuation line
                    return;
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
                if (lastTokenFromPreviousLine.TokenFamily == TokenFamily.AlphanumericLiteral)
                {
                    // The continuation line must contain a hyphen in the indicator area, and the first nonblank character must be a quotation mark
                    if(line[startOfContinuationIndex] != lastTokenFromPreviousLine.ExpectedClosingDelimiter)
                    {
                        tokensLine.AddDiagnostic(MessageCode.InvalidFirstCharForContinuationLine, startOfContinuationIndex, startOfContinuationIndex, lastTokenFromPreviousLine.ExpectedClosingDelimiter);
                    }
                    // The continuation of the literal begins with the character immediately following the quotation mark.
                    else
                    {
                        offsetForLiteralContinuation = 1;

                        // If an alphanumeric literal that is to be continued on the next line has as its last character a quotation mark in column 72, 
                        // the continuation line must start with two consecutive quotation marks.
                        if (lastTokenFromPreviousLine.HasClosingDelimiter)
                        {
                            if ((startOfContinuationIndex + 1) > lastIndex || line[startOfContinuationIndex + 1] != lastTokenFromPreviousLine.ExpectedClosingDelimiter)
                            {
                                tokensLine.AddDiagnostic(MessageCode.InvalidFirstTwoCharsForContinuationLine, startOfContinuationIndex, startOfContinuationIndex+1, lastTokenFromPreviousLine.ExpectedClosingDelimiter);
                                // Use the first quotation mark to avoid a complete mess while scanning the rest of the line
                                offsetForLiteralContinuation = 0;
                            }
                        }
                    }                    
                }          
      
                // p54: If there is no hyphen (-) in the indicator area (column 7) of a line, the last character
                // of the preceding line is assumed to be followed by a space.
                // If there is a hyphen in the indicator area of a line, the first nonblank character of
                // the continuation line immediately follows the last nonblank character of the
                // continued line without an intervening space.
                
                // Concatenate last token from previous line with the continuation line
                string continuedTextFromPreviousLine = lastTokenFromPreviousLine is ContinuationToken ? ((ContinuationToken)lastTokenFromPreviousLine).ContinuedSourceText : lastTokenFromPreviousLine.Text;
                int startOfContinuationStringIndex = startOfContinuationIndex + offsetForLiteralContinuation;
                string concatenatedLine = continuedTextFromPreviousLine + line.Substring(startOfContinuationStringIndex, lastIndex - startOfContinuationStringIndex + 1);

                // Create a temporary scanner over the concatenated line (continued token + continuation line)
                TokensLine virtualConcatenatedTokensLine = new TokensLine(TextLineMap.Create(concatenatedLine), tokensLine.PreviousLine, true);
                Scanner tempScanner = new Scanner(concatenatedLine, 0, concatenatedLine.Length - 1, virtualConcatenatedTokensLine, compilerOptions);

                // Check if the first token of the concatenated line really is a continuation of the previous line,
                // => is it different from the last token of the previous line ?
                Token virtualConcatenatedToken = tempScanner.GetNextToken();
                if(virtualConcatenatedToken != null)
                {
                    // Yes, the first token of the concatenated line is different from the last token of the previous line
                    if(virtualConcatenatedToken.TokenType != lastTokenFromPreviousLine.TokenType ||
                       virtualConcatenatedToken.HasError != lastTokenFromPreviousLine.HasError ||
                       virtualConcatenatedToken.Text != (lastTokenFromPreviousLine is ContinuationToken ? ((ContinuationToken)lastTokenFromPreviousLine).ContinuedSourceText : lastTokenFromPreviousLine.Text))
                    {
                        // Save the type of the last token on the previous line before continuation
                        TokenType lastTokenTypeFromPreviousLine = lastTokenFromPreviousLine.TokenType;

                        // Compute end index of the continuation token on the current line
                        int endOfContinuationIndex = virtualConcatenatedToken.Length - continuedTextFromPreviousLine.Length - 1 + startOfContinuationIndex + offsetForLiteralContinuation;

                        // Create a continuation token : copy of the first token, with different line and index properties
                        ContinuationToken continuationToken = new ContinuationToken(virtualConcatenatedToken, startOfContinuationIndex, offsetForLiteralContinuation, endOfContinuationIndex, textLineMap.TextLine, lastTokenFromPreviousLine);
                        
                        // Adjust the scanner state of the previous and current line : 
                        // the effect of the last token of the previous line must be canceled because it is in fact continued on the current line
                        tokensLine.AdjustScanStatesForContinuedAndContinuationLines();
                        
                        // Copy the first token and his diagnostics in the current line
                        tokensLine.AddToken(continuationToken);
                        foreach (Diagnostic diag in virtualConcatenatedTokensLine.GetDiagnosticsForToken(virtualConcatenatedToken))
                        {
                            tokensLine.AddDiagnostic((MessageCode)diag.Info.Code, continuationToken, diag.MessageArgs);
                        }

                        // Add a warning for a very special case we choosed not to implement in this scanner
                        // (cost and complexity is too high given the chance of seeing this case in a real program)
                        // Case 1
                        // - previous line     :          123.  => IntegerLiteral{123} PeriodSeparator
                        // - continuation line :    456         => DecimalLiteral{.456}
                        // (... should be DecimalLiteral{123.456} ...)
                        // Case 2
                        // - previous line     :          12E+  => InvalidToken{12E} PlusOperator
                        // - continuation line :    45          => IntegerLiteral{+45}
                        // (... should be FloatingPointLiteral{12E+45} ...)
                        if ((lastTokenTypeFromPreviousLine == TokenType.PeriodSeparator || lastTokenTypeFromPreviousLine == TokenType.PlusOperator || lastTokenTypeFromPreviousLine == TokenType.MinusOperator) &&
                            continuationToken.TokenFamily == TokenFamily.NumericLiteral)
                        {
                            string previousLineText = tokensLine.PreviousLine.TextLineMap.SourceText;
                            if (previousLineText.Length >= 2)
                            {
                                char lastCharBeforeSeparatorOrOperator = previousLineText[previousLineText.Length - 2];
                                if ((lastTokenTypeFromPreviousLine == TokenType.PeriodSeparator && Char.IsDigit(lastCharBeforeSeparatorOrOperator)) ||
                                    ((lastTokenTypeFromPreviousLine == TokenType.PlusOperator || lastTokenTypeFromPreviousLine == TokenType.MinusOperator) && (lastCharBeforeSeparatorOrOperator == 'E' || lastCharBeforeSeparatorOrOperator == 'E')))
                                {
                                    tokensLine.AddDiagnostic(MessageCode.ContinuationInsideDecimalLiteralCouldBeWrong, continuationToken);
                                }
                            }
                        }
                        // Add a warning for a very special case we choosed not to implement in this scanner
                        // (cost and complexity is too high given the chance of seeing this case in a real program)
                        // - previous line     :    PIC    999. => PictureCharacterString{999} PeriodSeparator
                        // - continuation line :    999         => DecimalLiteral{.999}
                        // (... should be PictureCharacterString{999.999} ...)
                        if (tokensLine.PreviousLine.ScanState.LastToken != null && 
                            tokensLine.PreviousLine.ScanState.LastToken.TokenType == TokenType.PictureCharacterString &&
                            (lastTokenTypeFromPreviousLine == TokenType.PeriodSeparator || lastTokenTypeFromPreviousLine == TokenType.CommaSeparator))
                        {
                            tokensLine.AddDiagnostic(MessageCode.ContinuationInsidePictureCharacterStringCouldBeWrong, continuationToken);
                        }

                        // Set the start index for the scanner after the continuation
                        startIndex = endOfContinuationIndex + 1;
                    }
                    else
                    {
                        // Do nothing special, we can safely ignore the continuation indicator
                    }
                } 
            }
            
            // Create a stateful line scanner, and iterate over the tokens
            Scanner scanner = new Scanner(line, startIndex, lastIndex, tokensLine, compilerOptions);
            Token nextToken = null;
            while((nextToken = scanner.GetNextToken()) != null)
            {
                // Resolve DELETE ambiguity : DELETE + InterLiteral => DELETE_CD
                // Warning : DELETE and the sequence-number-field must be on the same line
                if(nextToken.TokenType == TokenType.IntegerLiteral && tokensLine.ScanState.KeywordsState == KeywordsSequenceState.After_DELETE)
                {
                    tokensLine.ScanState.LastKeywordOrSymbolToken.CorrectType(TokenType.DELETE_CD);
                }
                tokensLine.AddToken(nextToken);
            }    
        }

        /// <summary>
        /// Scan an isolated token in the following "default" context :
        /// - insideDataDivision = true
        /// - decimalPointIsComma = false
        /// - withDebuggingMode = false
        /// - encodingForHexadecimalAlphanumericLiterals = IBM 1147
        /// - default compiler options
        /// </summary>
        public static Token ScanIsolatedTokenInDefaultContext(string tokenText, out Diagnostic error)
        {
            TokensLine tempTokensLine = new TokensLine(TextLineMap.Create(tokenText), true, false, false, IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147));
            Scanner tempScanner = new Scanner(tokenText, 0, tokenText.Length - 1, tempTokensLine, new TypeCobolOptions());
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
        private ITextLine textLine;
        private string line;
        private int currentIndex;
        private int lastIndex;

        private TypeCobolOptions compilerOptions;

        private Scanner(string line, int startIndex, int lastIndex, TokensLine tokensLine, TypeCobolOptions compilerOptions)
        {
            this.tokensLine = tokensLine;
            this.textLine = tokensLine.TextLineMap.TextLine;
            this.line = line;
            this.currentIndex = startIndex;
            this.lastIndex = lastIndex;

            this.compilerOptions = compilerOptions;
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

            // Handle special scanner states
            KeywordsSequenceState currentState = tokensLine.ScanState.KeywordsState;
            if (currentState != KeywordsSequenceState.Default)
            {
                // -> PictureCharacterString
                if(currentState == KeywordsSequenceState.After_PIC_orPICTURE ||
                   currentState == KeywordsSequenceState.After_PIC_orPICTURE_IS)
                {
                   if(CobolChar.IsStartOfPictureCharacterString(line[startIndex]))
                   {
                       return ScanPictureCharacterString(startIndex);
                   }
                   else
                   {
                       // Continue to the main switch
                   }
                }
                // -> PeriodSeparator or CommentEntry
                else if (currentState == KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY)
                {
                    switch (line[startIndex])
                    {
                        case ' ':
                            return ScanWhitespace(startIndex);
                        case '.':
                            return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PeriodSeparator, MessageCode.InvalidCharAfterPeriod);
                        default:
                            return ScanCommentEntry(startIndex);
                    }
                }
                // -> CommentEntry
                else if (currentState == KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY_PeriodSeparator)
                {
                    switch (line[startIndex])
                    {
                        case ' ':
                            return ScanWhitespace(startIndex);
                        default:
                            return ScanCommentEntry(startIndex);
                    }
                }
                // -> second CommentEntry
                else if (currentState == KeywordsSequenceState.After_CommentEntry)
                {
                    return ScanCommentEntry(startIndex);
                }
                // -> ExecStatementText
                else if (tokensLine.ScanState.KeywordsState == KeywordsSequenceState.After_EXEC_orEXECUTE_ExecTranslatorName)
                {
                    switch (line[startIndex])
                    {
                        case ' ':
                            return ScanWhitespace(startIndex);
                        default:
                            return ScanExecStatementTextOrExecSqlInclude(startIndex);
                    }
                }
                // -> second ExecStatementText
                else if (currentState == KeywordsSequenceState.After_ExecStatementText)
                {
                    return ScanExecStatementTextOrExecSqlInclude(startIndex);
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
                    if (tokensLine.ScanState.DecimalPointIsComma)
                    {
                        //IntegerLiteral = 27,
                        //DecimalLiteral = 28,
                        //FloatingPointLiteral = 29,
                        return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.CommaSeparator, MessageCode.InvalidCharAfterComma); 
                    }
                    else
                    {
                        return ScanOneCharFollowedBySpace(startIndex, TokenType.CommaSeparator, MessageCode.InvalidCharAfterComma);
                    }
                case ';':
                    //SemicolonSeparator=3,
                    // p46: A separator semicolon is composed of a semicolon followed by a space.
                    return ScanOneCharFollowedBySpace(startIndex, TokenType.SemicolonSeparator, MessageCode.SemicolonShouldBeFollowedBySpace);                    
                case '*':
                    //MultiplyOperator=14,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    if(currentIndex == lastIndex)
                    {
                        // consume the * char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.MultiplyOperator, startIndex, startIndex, true, textLine);
                    }
                    else if(line[currentIndex + 1] == ' ')
                    {
                        // consume the * char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.MultiplyOperator, startIndex, startIndex + 1, textLine);
                    }
                    //PowerOperator=15,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    else if (line[currentIndex + 1] == '*')
                    {
                        // consume the first * char
                        currentIndex++;
                        // scan the second * char and a space
                        return ScanOneCharFollowedBySpace(startIndex, TokenType.PowerOperator, MessageCode.PowerOperatorShouldBeFollowedBySpace);
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
                        return new Token(TokenType.ASTERISK_CBL, startIndex, startIndex + 3, textLine);
                    }
                    // ASTERISK_CONTROL = "*CONTROL"
                    else if ((line[currentIndex + 1] == 'C' || line[currentIndex + 1] == 'c') &&
                        (currentIndex + 7 <= lastIndex && line.Substring(currentIndex, 8).Equals("*CONTROL", StringComparison.OrdinalIgnoreCase)) &&
                        (currentIndex + 7 == lastIndex || CobolChar.IsCobolWordSeparator(line[currentIndex + 8])))
                    {
                        // match 8 chars                       
                        currentIndex += 8;
                        return new Token(TokenType.ASTERISK_CONTROL, startIndex, startIndex + 7, textLine);
                    }
                    else
                    {
                        // consume * char and try to match it as a multiply operator
                        currentIndex++;
                        Token invalidToken = new Token(TokenType.MultiplyOperator, startIndex, startIndex, textLine);
                        tokensLine.AddDiagnostic(MessageCode.InvalidCharAfterAsterisk, invalidToken);
                        return invalidToken;
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
                    //IntegerLiteral = 27,
                    //DecimalLiteral = 28,
                    //FloatingPointLiteral = 29,
                    return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PlusOperator, MessageCode.InvalidCharAfterPlus);
                case '-':
                    //MinusOperator=12,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    //IntegerLiteral = 27,
                    //DecimalLiteral = 28,
                    //FloatingPointLiteral = 29,
                    return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.MinusOperator, MessageCode.InvalidCharAfterMinus);
                case '/':
                    //DivideOperator=13,
                    // p254: These operators are represented by specific characters that
                    // must be preceded and followed by a space.
                    return ScanOneCharFollowedBySpace(startIndex, TokenType.DivideOperator, MessageCode.DivideOperatorShouldBeFollowedBySpace);
                case '<':
                    //LessThanOperator=16,
                    //LessThanOrEqualOperator=18,
                    // p260: Each relational operator must be preceded and followed
                    // by a space. 
                    if (currentIndex == lastIndex)
                    {
                        // consume the < char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.LessThanOperator, startIndex, startIndex, true, textLine);
                    }
                    else if (line[currentIndex + 1] == ' ')
                    {
                        // consume the < char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.LessThanOperator, startIndex, startIndex + 1, textLine);
                    }
                    else if (line[currentIndex + 1] == '=')
                    {
                        // consume the < char
                        currentIndex++;
                        // scan the = char and a space
                        return ScanOneCharFollowedBySpace(startIndex, TokenType.LessThanOrEqualOperator, MessageCode.LessThanOrEqualOperatorShouldBeFollowedBySpace); 
                    }
                    else
                    {
                        // consume < char and try to match it as a less than operator
                        currentIndex++;
                        Token invalidToken = new Token(TokenType.LessThanOperator, startIndex, startIndex, textLine);
                        tokensLine.AddDiagnostic(MessageCode.InvalidCharAfterLessThan, invalidToken);
                        return invalidToken;
                    }
                case '>':
                    //GreaterThanOperator=17,
                    //GreaterThanOrEqualOperator=19,
                    // p260: Each relational operator must be preceded and followed
                    // by a space. 
                    if (currentIndex == lastIndex)
                    {
                        // consume the > char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.GreaterThanOperator, startIndex, startIndex, true, textLine);
                    }
                    else if (line[currentIndex + 1] == ' ')
                    {
                        // consume the > char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.GreaterThanOperator, startIndex, startIndex + 1, textLine);
                    }
                    else if (line[currentIndex + 1] == '=')
                    {
                        // consume the > char
                        currentIndex++;
                        // scan the = char and a space
                        return ScanOneCharFollowedBySpace(startIndex, TokenType.GreaterThanOrEqualOperator, MessageCode.GreaterThanOrEqualOperatorShouldBeFollowedBySpace);
                    }
                    else
                    {
                        // consume > char and try to match it as a greater than operator
                        currentIndex++;
                        Token invalidToken = new Token(TokenType.GreaterThanOperator, startIndex, startIndex, textLine);
                        tokensLine.AddDiagnostic(MessageCode.InvalidCharAfterGreaterThan, invalidToken);
                        return invalidToken;
                    }
                case '=':
                    //EqualOperator=20,
                    // p260: Each relational operator must be preceded and followed
                    // by a space. 
                    if (currentIndex == lastIndex)
                    {
                        // consume the = char
                        currentIndex++;
                        // use the virtual space at end of line
                        return new Token(TokenType.EqualOperator, startIndex, startIndex, true, textLine);
                    }
                    else if (line[currentIndex + 1] == ' ')
                    {
                        // consume the = char and the space char
                        currentIndex += 2;
                        return new Token(TokenType.EqualOperator, startIndex, startIndex + 1, textLine);
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
                        if (tokensLine.ScanState.KeywordsState != KeywordsSequenceState.InsidePseudoText)
                        {
                            delimiterToken = new Token(TokenType.PseudoTextDelimiter, startIndex, startIndex + 1, textLine);
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

                            delimiterToken = new Token(TokenType.PseudoTextDelimiter, startIndex, startIndex + 1, usesVirtualSpaceAtEndOfLine, textLine);
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
                        Token invalidToken = new Token(TokenType.EqualOperator, startIndex, startIndex, textLine);
                        tokensLine.AddDiagnostic(MessageCode.InvalidCharAfterEquals, invalidToken);
                        return invalidToken;
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
                    //IntegerLiteral = 27,
                    //DecimalLiteral = 28,
                    //FloatingPointLiteral = 29,
                    return ScanNumericLiteral(startIndex);
                default:
                    //UserDefinedWord = 36,
                    return ScanCharacterString(startIndex);
            }
        }

        // --- Implementation note : Context-sensistive scanner operations ---

        // (PICTURE | PIC) -> SYMBOL -> (nothing)
        //                 -> IS? -> pictureCharacterString

        // (AUTHOR | INSTALLATION | DATE_WRITTEN | DATE_COMPILED | SECURITY) -> PeriodSeparator? -> CommentEntry*

        // FUNCTION -> FunctionName

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
            return new Token(TokenType.SpaceSeparator, startIndex, endIndex, textLine);
        }

        private Token ScanOneChar(int startIndex, TokenType tokenType)
        {
            // consume one char
            currentIndex++;
            return new Token(tokenType, startIndex, startIndex, textLine);
        }

        private Token ScanOneCharFollowedBySpace(int startIndex, TokenType tokenType, MessageCode messageCode)
        {
            if (currentIndex == lastIndex)
            {
                // consume one char and use the virtual space at end of line
                currentIndex++;
                return new Token(tokenType, startIndex, startIndex, true, textLine);
            }
            else if (line[currentIndex + 1] == ' ')
            {
                // consume one char and consume the space char
                currentIndex += 2;
                return new Token(tokenType, startIndex, startIndex + 1, textLine);
            }
            else
            {
                // consume one char and register an error because the following char is missing
                // even if the space is missing, try to match the expected tokenType
                currentIndex++;
                int endIndex = (tokenType == TokenType.PowerOperator || tokenType == TokenType.LessThanOrEqualOperator || tokenType == TokenType.GreaterThanOrEqualOperator) ? startIndex + 1 : startIndex;
                Token invalidToken = new Token(tokenType, startIndex, endIndex, textLine);
                tokensLine.AddDiagnostic(messageCode, invalidToken);
                return invalidToken;
            }
        }

        private Token ScanOneCharFollowedBySpaceOrNumericLiteral(int startIndex, TokenType tokenType, MessageCode messageCode)
        {
            if (currentIndex == lastIndex)
            {
                // consume one char and use the virtual space at end of line
                currentIndex++;
                return new Token(tokenType, startIndex, startIndex, true, textLine);
            }
            else if (line[currentIndex + 1] == ' ')
            {
                // consume one char and consume the space char
                currentIndex += 2;
                return new Token(tokenType, startIndex, startIndex + 1, textLine);
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
                Token invalidToken = new Token(tokenType, startIndex, startIndex, textLine);
                tokensLine.AddDiagnostic(messageCode, invalidToken);
                return invalidToken;
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
            return new Token(TokenType.FloatingComment, startIndex, lastIndex, textLine, true, true, ' ');
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
                    Token token = new Token(TokenType.IntegerLiteral, startIndex, endIndex, textLine);
                    token.LiteralValue = new IntegerLiteralValue(null, line.Substring(startIndex, fstCurrentIndex - startIndex));
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
                    Token token = new Token(TokenType.FloatingPointLiteral, startIndex, endIndex, textLine);
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
                    token.LiteralValue = new FloatingPointLiteralValue(fpMatch.Groups[1].Value, fpMatch.Groups[2].Value, mantissaDecimalPart, fpMatch.Groups[4].Value, exponent);
                    return token;
                }
                else
                {
                    // consume all lookup chars
                    currentIndex = lookupEndIndex + 1;
                    Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lookupEndIndex, textLine);
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
                    Token token = new Token(TokenType.DecimalLiteral, startIndex, endIndex, textLine);
                    token.LiteralValue = new DecimalLiteralValue(decMatch.Groups[1].Value, decMatch.Groups[2].Value, decMatch.Groups[3].Value);
                    return token;
                }
                else
                {
                    // consume all lookup chars
                    currentIndex = lookupEndIndex + 1;
                    Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lookupEndIndex, textLine);
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
            Token token = new Token(tokenType, startIndex, endIndex, usingVirtualSpaceAtEndOfLine, textLine, true, closingDelimiterFound, delimiter);
            
            // compute the value of the literal, depending on the exact literal type            
            AlphanumericLiteralValue value = null;
            if (tokenType != TokenType.HexadecimalAlphanumericLiteral && tokenType != TokenType.HexadecimalNationalLiteral)
            {
                value = new AlphanumericLiteralValue(sbValue.ToString());
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
                value = new AlphanumericLiteralValue(hexadecimalChars, tokensLine.ScanState.EncodingForHexadecimalAlphanumericLiterals);
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
                value = new AlphanumericLiteralValue(hexadecimalChars, Encoding.Unicode);
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

            switch(tokensLine.ScanState.KeywordsState)
            {
                case KeywordsSequenceState.After_PIC_orPICTURE:
                case KeywordsSequenceState.After_PIC_orPICTURE_IS:
                    return ScanPictureCharacterString(startIndex);

                case KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY:
                case KeywordsSequenceState.After_AUTHOR_orINSTALLATION_orDATE_WRITTEN_orDATE_COMPILED_orSECURITY_PeriodSeparator:
                    return ScanCommentEntry(startIndex);

                case KeywordsSequenceState.After_EXEC_orEXECUTE:
                    return ScanExecTranslatorName(startIndex);

                case KeywordsSequenceState.After_EXEC_orEXECUTE_ExecTranslatorName:
                    return ScanExecStatementTextOrExecSqlInclude(startIndex);

                default:
                    return ScanKeywordOrUserDefinedWord(startIndex);
            }
        }

        private Token ScanPictureCharacterString(int startIndex)
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
                return new Token(TokenType.SYMBOL, startIndex, endIndex, textLine);
            }
            else if (value.Equals("IS", StringComparison.InvariantCultureIgnoreCase))
            {
                // Return a keyword
                return new Token(TokenType.IS, startIndex, endIndex, textLine);
            }
            else
            {
                // Return a picture character string
                return new Token(TokenType.PictureCharacterString, startIndex, endIndex, textLine);
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
            //
            // p105 : The comment-entry in any of the optional paragraphs can be any combination of
            // characters from the character set of the computer. The comment-entry is written in
            // Area B on one or more lines.
            // LP -> a comment entry is delimited only by characters in area A on the next line

            // Find first non whitespace char
            int firstCharIndex = startIndex;
            for (; firstCharIndex <= lastIndex && line[firstCharIndex] == ' '; firstCharIndex++) { }
            // Check if it is in area A
            if(line[firstCharIndex] != ' ' && firstCharIndex < (tokensLine.TextLineMap.Source.StartIndex + 4))
            {
                // Reset scanner state and retry scanning
                tokensLine.ScanState.ResetKeywordsState();
                return GetNextToken();
            }
            else
            {
                // Consume the entire line as a comment entry
                currentIndex = lastIndex + 1;
                return new Token(TokenType.CommentEntry, startIndex, lastIndex, textLine);
            }

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
            
            return new Token(TokenType.ExecTranslatorName, startIndex, endIndex, textLine);
        }

        private Token ScanExecStatementTextOrExecSqlInclude(int startIndex)
        {
            // --- Special treatment for EXEC SQL(IMS) INCLUDE ---

            // Check if previous tokens were EXEC SQL or EXEC SQLIMS
            Token lastSymbolToken = tokensLine.ScanState.LastKeywordOrSymbolToken;
            if(lastSymbolToken.TokenType == TokenType.ExecTranslatorName &&
               (lastSymbolToken.Text.Equals("SQL", StringComparison.InvariantCultureIgnoreCase) ||
                lastSymbolToken.Text.Equals("SQLIMS", StringComparison.InvariantCultureIgnoreCase)) &&
                tokensLine.ScanState.LastToken.TokenType != TokenType.ExecStatementText)
            {
                // Check if the text immediately following is INCLUDE
                if (lastIndex - startIndex >= 6 &&
                   line.Substring(startIndex, 7).Equals("INCLUDE", StringComparison.InvariantCultureIgnoreCase))
                {
                    // Consume 7 chars
                    currentIndex = startIndex + 7;
                    return new Token(TokenType.EXEC_SQL_INCLUDE, startIndex, startIndex + 6, textLine);
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

                // Reset Scan State after END-EXEC
                tokensLine.ScanState.ResetKeywordsState();
            }
            // ExecStatementText is empty
            else if (endExecIndex == startIndex)
            {
                // Reset Scan State and directly scan END-EXEC keyword
                tokensLine.ScanState.ResetKeywordsState();
                return ScanKeywordOrUserDefinedWord(startIndex);
            }

            // Consume all chars
            currentIndex = endIndex + 1;

            return new Token(TokenType.ExecStatementText, startIndex, endIndex, textLine);
        }

        private Token ScanKeywordOrUserDefinedWord(int startIndex)
        {
            // p9: A COBOL word is a character-string that forms a user-defined word, a
            // system-name, or a reserved word.

            // p45: Table 4. Separators

            // consume any char until a separator char is encountered
            for (; currentIndex <= lastIndex && line[currentIndex] < 256 && !CobolChar.IsCobolWordSeparator(line[currentIndex]); currentIndex++) { }
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
            string tokenText = line.Substring(startIndex, endIndex - startIndex + 1);
            TokenType tokenType = TokenUtils.GetTokenTypeFromTokenString(tokenText);

            // Correct token type for context-sensitive tokens
            switch(tokenType)
            {
                // Inside DATA DIVISION, the DISPLAY keyword is an argument of the PICTURE clause 
                case TokenType.DISPLAY:
                    if(tokensLine.ScanState.InsideDataDivision)
                    {
                        tokenType = TokenType.DISPLAY_ARG;
                    }
                    break;
                case TokenType.ENTRY:
                    if (tokensLine.ScanState.KeywordsState == KeywordsSequenceState.After_TO)
                    {
                        tokenType = TokenType.ENTRY_ARG;
                    }
                    break;
                case TokenType.SORT:
                    if (tokensLine.ScanState.KeywordsState == KeywordsSequenceState.After_SAME)
                    {
                        tokenType = TokenType.SORT_ARG;
                    }
                    break;
                case TokenType.UserDefinedWord:                    
                    //FunctionName = 34,
                    // p477: function-name-1 must be one of the intrinsic function names.
                    // ACOS | ANNUITY | ASIN | ATAN | CHAR | COS | CURRENT_DATE | DATE_OF_INTEGER | DATE_TO_YYYYMMDD |
                    // DAY_OF_INTEGER | DAY_TO_YYYYDDD | DISPLAY_OF | FACTORIAL | INTEGER | INTEGER_OF_DATE | INTEGER_OF_DAY |
                    // INTEGER_PART | LENGTH | LOG | LOG10 | LOWER_CASE | MAX | MEAN | MEDIAN | MIDRANGE | MIN | MOD |
                    // NATIONAL_OF | NUMVAL | NUMVAL_C | ORD | ORD_MAX | ORD_MIN | PRESENT_VALUE | RANDOM | RANGE | REM |
                    // REVERSE | SIN | SQRT | STANDARD_DEVIATION | SUM | TAN | ULENGTH | UPOS | UPPER_CASE | USUBSTR | 
                    // USUPPLEMENTARY | UVALID | UWIDTH | VARIANCE | WHEN_COMPILED | YEAR_TO_YYYY
                    if(tokensLine.ScanState.KeywordsState == KeywordsSequenceState.After_FUNCTION)
                    {
                        tokenType = TokenType.FunctionName;
                    }
                    // p117: SYMBOLIC CHARACTERS clause 
                    // symbolic-character-1 is a user-defined word and must contain at least one alphabetic character. 
                    // The same symbolic-character can appear only once in a SYMBOLIC CHARACTERS clause. 
                    // The symbolic character can be a DBCS user-defined word. 
                    else if(tokensLine.ScanState.KeywordsState == KeywordsSequenceState.After_SYMBOLIC ||
                            tokensLine.ScanState.KeywordsState == KeywordsSequenceState.After_SYMBOLIC_SymbolicCharacters)
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
                    break;
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
            return new Token(tokenType, startIndex, endIndex, textLine);
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
                    for (; currentIndex <= lastIndex && line[currentIndex] < 256 && !CobolChar.IsCobolWordSeparator(line[currentIndex]); currentIndex++) { }
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
            Token sourceToken = new Token(TokenType.CompilerDirective, startIndex, stopIndex, textLine);
            
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
            if(line[index] == ':')
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
                for (; searchIndex <= lastIndex && line[searchIndex] < 256 && !CobolChar.IsCobolWordSeparator(line[searchIndex]); searchIndex++) { }
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

            return new Token(TokenType.PartialCobolWord, startIndex, endIndex, textLine);
        }        
    }
}
