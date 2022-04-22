using System.Linq;
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Scanner
{
    /// <summary>
    /// Base class for Cobol Scanner and SQL Scanner
    /// </summary>
    public abstract class AbstractScanner
    {
        private static readonly Regex _DecimalLiteralRegex = new Regex("([-+]?)([0-9]*)(?:[\\.,]([0-9]+))?", RegexOptions.Compiled);
        private static readonly Regex _FloatingPointLiteralRegex = new Regex("([-+]?)([0-9]*)(?:[\\.,]([0-9]+))?[eE]([-+]?)([0-9]+)", RegexOptions.Compiled);

        protected readonly string line;
        protected int currentIndex;
        protected readonly int lastIndex;
        protected readonly TokensLine tokensLine;
        protected readonly TypeCobolOptions compilerOptions;

        protected AbstractScanner(string line, int startIndex, int lastIndex, TokensLine tokensLine, TypeCobolOptions compilerOptions)
        {
            this.line = line;
            this.currentIndex = startIndex;
            this.lastIndex = lastIndex;
            this.tokensLine = tokensLine;
            this.compilerOptions = compilerOptions;
        }

        /// <summary>
        /// Current position in the line being parsed.
        /// This is the index of the next character to be read by this scanner.
        /// </summary>
        public int CurrentIndex => currentIndex;

        /// <summary>
        /// Scan from current index and return the token found.
        /// </summary>
        /// <returns>Token instance, null if no token could be scanned.</returns>
        public Token GetNextToken() => GetTokenStartingFrom(currentIndex);

        /// <summary>
        /// Scan from given index and return the token found.
        /// </summary>
        /// <param name="startIndex">Index to start scanning from.</param>
        /// <returns>Token instance, null if no token could be scanned.</returns>
        public abstract Token GetTokenStartingFrom(int startIndex);

        protected Token ScanWhitespace(int startIndex)
        {
            // consume all whitespace chars available
            for (; currentIndex <= lastIndex && line[currentIndex] == ' '; currentIndex++) { }
            int endIndex = currentIndex - 1;

            if (tokensLine.ScanState.InsidePseudoText || !compilerOptions.OptimizeWhitespaceScanning)
            {
                // SpaceSeparator has to be created
                return new Token(TokenType.SpaceSeparator, startIndex, endIndex, tokensLine);
            }

            // jump to next token
            return GetNextToken();
        }

        protected Token ScanOneChar(int startIndex, TokenType tokenType)
        {
            // consume one char
            currentIndex++;
            return new Token(tokenType, startIndex, startIndex, tokensLine);
        }

        protected Token ScanNumericLiteral(int startIndex, char decimalPoint)
        {
            //IntegerLiteral = 27,
            // Fast path for the most common type of numeric literal : simple integer literals like the level numbers (no sign, no decimal point)
            char firstChar = line[startIndex];
            if (char.IsDigit(firstChar))
            {
                // consume all the following digits
                int fstCurrentIndex = startIndex + 1;
                for (; fstCurrentIndex <= lastIndex && char.IsDigit(line[fstCurrentIndex]); fstCurrentIndex++) { }

                // check to see if the following char could be part of a numeric literal
                if (fstCurrentIndex > lastIndex ||
                    (line[fstCurrentIndex] != decimalPoint && line[fstCurrentIndex] != 'e' && line[fstCurrentIndex] != 'E') ||
                    (line[fstCurrentIndex] == decimalPoint && (fstCurrentIndex == lastIndex || line[fstCurrentIndex + 1] == ' ')))
                {
                    // if it is not the case, assume this is the end of a simple integer literal
                    currentIndex = fstCurrentIndex;
                    int endIndex = fstCurrentIndex - 1;
                    Token token = new Token(TokenType.IntegerLiteral, startIndex, endIndex, tokensLine)
                                  {
                                      LiteralValue = new IntegerLiteralTokenValue(null, line.Substring(startIndex, fstCurrentIndex - startIndex))
                                  };
                    return token;
                }
            }

            // If the fast path attempt was not successful, try to match each one
            // of the two other numeric literal formats, with regular expressions 
            int lookupEndIndex = startIndex;

            // consume the first char if it was +/- (to simplify the count of chars below)
            if (line[startIndex] == '+' || line[startIndex] == '-')
            {
                lookupEndIndex++;
            }

            // then consume the following chars : digits many times, + -  e E . only once             
            bool currentCharStillInLiteral;
            bool plusMinusFound = false;
            bool periodFound = false;
            bool eEFound = false;
            do
            {
                char c = line[lookupEndIndex];
                currentCharStillInLiteral = char.IsDigit(c) || (!periodFound && c == decimalPoint) || (!plusMinusFound && (c == '+' || c == '-')) || (!eEFound && (c == 'e' || c == 'E'));
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

            // we may have consumed one additional character after the end of the literal
            if ((line[lookupEndIndex] == decimalPoint || line[lookupEndIndex] == '+' || line[lookupEndIndex] == '-') && lookupEndIndex > startIndex)
            {
                lookupEndIndex--;
            }

            // then try to predict the intended the number format
            string numberString = line.Substring(startIndex, lookupEndIndex - startIndex + 1);
            if (numberString.Contains('E') || numberString.Contains('e'))
            {
                //FloatingPointLiteral = 29,
                Match fpMatch = _FloatingPointLiteralRegex.Match(line, startIndex, lastIndex - startIndex + 1);
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
                    token.LiteralValue = new FloatingPointLiteralTokenValue(fpMatch.Groups[1].Value, fpMatch.Groups[2].Value, mantissaDecimalPart, fpMatch.Groups[4].Value, fpMatch.Groups[5].Value);
                    return token;
                }

                // consume all lookup chars
                currentIndex = lookupEndIndex + 1;
                Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lookupEndIndex, tokensLine);
                tokensLine.AddDiagnostic(MessageCode.InvalidNumericLiteralFormat, invalidToken);
                return invalidToken;
            }
            else
            {
                //DecimalLiteral = 28,
                Match decMatch = _DecimalLiteralRegex.Match(line, startIndex, lastIndex - startIndex + 1);
                if (decMatch.Success && decMatch.Index == startIndex)
                {
                    currentIndex += decMatch.Length;
                    int endIndex = startIndex + decMatch.Length - 1;
                    TokenType type;
                    LiteralTokenValue value;
                    if (decMatch.Groups[3].Value.Length > 0)
                    {
                        type = TokenType.DecimalLiteral;
                        value = new DecimalLiteralTokenValue(decMatch.Groups[1].Value, decMatch.Groups[2].Value, decMatch.Groups[3].Value);
                    }
                    else
                    {
                        type = TokenType.IntegerLiteral;
                        value = new IntegerLiteralTokenValue(decMatch.Groups[1].Value, decMatch.Groups[2].Value);
                    }
                    Token token = new Token(type, startIndex, endIndex, tokensLine)
                                  {
                                      LiteralValue = value
                                  };
                    return token;
                }

                // consume all lookup chars
                currentIndex = lookupEndIndex + 1;
                Token invalidToken = new Token(TokenType.InvalidToken, startIndex, lookupEndIndex, tokensLine);
                tokensLine.AddDiagnostic(MessageCode.InvalidNumericLiteralFormat, invalidToken);
                return invalidToken;
            }
        }
    }
}
