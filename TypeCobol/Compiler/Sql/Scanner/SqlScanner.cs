using System;
using System.Collections.Generic;
using System.Diagnostics;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.Sql.Scanner
{
    /// <summary>
    /// A Sql Scanner related to a Scanner
    /// </summary>
    public class SqlScanner : AbstractScanner
    {
        private static readonly Dictionary<string, DecimalFloatingPointSpecialValueType> SpecialValues =
            new Dictionary<string, DecimalFloatingPointSpecialValueType>(StringComparer.OrdinalIgnoreCase)
            {
                {"NaN", DecimalFloatingPointSpecialValueType.NaN},
                {"SNaN", DecimalFloatingPointSpecialValueType.SNaN},
                {"Infinity", DecimalFloatingPointSpecialValueType.Infinity},
                {"Inf", DecimalFloatingPointSpecialValueType.Infinity}
            };

        /// <summary>
        /// True if the given char can be part of an SQL keyword.
        /// </summary>
        /// <param name="c">The character to test</param>
        /// <returns>True if yes, no otherwise</returns>
        private static bool IsSqlKeywordPart(char c)
        {
            return c == '-' || char.IsLetter(c);
        }

        public SqlScanner(string line, int startIndex, int lastIndex, TokensLine tokensLine,
            TypeCobolOptions compilerOptions)
            : base(line, startIndex, lastIndex, tokensLine, compilerOptions)
        {

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
            if (IsSqlKeywordPart(line[currentIndex]))
            {
                //Consume all sql-keyword compatible chars
                for (; currentIndex <= lastIndex && IsSqlKeywordPart(line[currentIndex]); currentIndex++)
                {
                }

                string tokenText = line.Substring(startIndex, currentIndex - startIndex);
                //Try to match keyword text
                var tryToGetTokenType =
                    TokenUtils.TryToGetSqlKeywordTokenTypeFromTokenString(tokenText, out TokenType tokenType);
                if (tryToGetTokenType)
                {
                    return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
                }
            }

            switch (line[startIndex])
            {
                case ' ':
                    //SpaceSeparator=1,
                    return ScanWhitespace(startIndex);
                case '*':
                    return ScanOneChar(startIndex, TokenType.MultiplyOperator);
                case ',':
                    return ScanOneChar(startIndex, TokenType.SQL_CommaSeparator);
                case '.':
                    return ScanOneChar(startIndex, TokenType.PeriodSeparator);
                case '(':
                    return ScanOneChar(startIndex, TokenType.LeftParenthesisSeparator);
                case ')':
                    return ScanOneChar(startIndex, TokenType.RightParenthesisSeparator);
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
                    return ScanSqlNumericLiteral(startIndex, false);
                case '+':
                    return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PlusOperator,
                        MessageCode.ImplementationError, false);
                case '-':
                    return ScanOneCharFollowedBySpaceOrNumericLiteral(startIndex, TokenType.MinusOperator,
                        MessageCode.ImplementationError, false);

                case 'I':
                case 'N':
                case 'S':
                case 'i':
                case 'n':
                case 's':
                    if (TryScanDecimalFloatingPointSpecialValue(startIndex, false,
                            out var tokenValue))
                    {
                        Token decimalFloatingPointSpecialValueToken = new Token(
                            TokenType.SQL_DecimalFloatingPointLiteral,
                            startIndex, currentIndex - 1, tokensLine)
                        {
                            LiteralValue = tokenValue
                        };
                        return decimalFloatingPointSpecialValueToken;
                    }

                    return ScanKeywordOrUserDefinedWord(startIndex);
                case '\'':
                case '"':
                    return ScanAlphanumericLiteral(startIndex, TokenType.AlphanumericLiteral, null);
                case 'X':
                case 'x':
                    if (line[currentIndex + 1] == '\'' || line[currentIndex + 1] == '"')
                    {
                        return ScanAlphanumericLiteral(startIndex, TokenType.AlphanumericLiteral, null);
                    }
                    else
                    {
                        return ScanKeywordOrUserDefinedWord(startIndex);
                    }
                case 'B':
                case 'b':
                    if ((startIndex < lastIndex - 2) && (line[startIndex + 1] == 'x' || line[startIndex + 1] == 'X') &&
                        (line[startIndex + 2] == '\'' || line[startIndex + 2] == '"'))
                    {
                        return ScanAlphanumericLiteral(startIndex, TokenType.SQL_BinaryStringLiteral, null);
                    }
                    else
                    {
                        return ScanKeywordOrUserDefinedWord(startIndex);
                    }
                case 'U':
                case 'u':
                case 'G':
                case 'g':
                    if ((startIndex < lastIndex - 2) && (line[startIndex + 1] == 'x' || line[startIndex + 1] == 'X') &&
                        (line[startIndex + 2] == '\'' || line[startIndex + 2] == '"'))
                    {
                        return ScanAlphanumericLiteral(startIndex, TokenType.SQL_GraphicStringLiteral, null);
                    }
                    else
                    {
                        return ScanKeywordOrUserDefinedWord(startIndex);
                    }
                default:
                    return ScanKeywordOrUserDefinedWord(startIndex);

            }
        }

        private Token ScanSqlNumericLiteral(int startIndex, bool isNegative)
        {
            var tryScanDecimalFloatingPointSpecialValue =
                TryScanDecimalFloatingPointSpecialValue(startIndex, isNegative,
                    out var decimalFloatingPointLiteralTokenValue);
            if (tryScanDecimalFloatingPointSpecialValue == true)
            {
                Token decimalFloatingPointSpecialValueToken = new Token(TokenType.SQL_DecimalFloatingPointLiteral,
                    startIndex, currentIndex - 1, tokensLine)
                {
                    LiteralValue = decimalFloatingPointLiteralTokenValue
                };
                return decimalFloatingPointSpecialValueToken;
            }

            var token = ScanNumericLiteral(startIndex, '.');
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
                int literalValueLength = literalValue.Number.ToString().Length -
                                         (literalValue.HasSign == true ? 1 : 0);
                ;
                //Check IntegerLiteral range
                if (literalValueLength > 19)
                {
                    //Out of range
                    this.tokensLine.AddDiagnostic(MessageCode.SyntaxErrorInParser, token,
                        "Number is too big : " + literalValue.Number);
                }
            }

            void CheckFloatingPointLiteral()
            {
                Debug.Assert(token.LiteralValue is FloatingPointLiteralTokenValue);
                var literalValue = (FloatingPointLiteralTokenValue)token.LiteralValue;

                //Check exponent length
                var exponent = literalValue.Exponent.Number;
                int exponentLength =
                    exponent.ToString().Length -
                    (exponent.Sign == -1 ? 1 : 0); //Check digits only, so remove '-' at beginning of negative numbers
                var mantissa = literalValue.Mantissa.Number;
                int mantissaLength =
                    mantissa.ToString().Length;
                if (exponentLength > 2 || mantissaLength > 17)
                {
                    token = new Token(TokenType.SQL_DecimalFloatingPointLiteral, startIndex, currentIndex - 1,
                        tokensLine)
                    {
                        LiteralValue =
                            new Sql.Scanner.DecimalFloatingPointLiteralTokenValue(LiteralTokenValueType.FloatingPoint,
                                literalValue)
                    };

                    CheckDecimalFloatingPointLiteral();
                }
            }

            void CheckDecimalFloatingPointLiteral()
            {
                Debug.Assert(token.LiteralValue is DecimalFloatingPointLiteralTokenValue);
                var literalValue = (DecimalFloatingPointLiteralTokenValue)token.LiteralValue;

                //Check exponent length
                var exponent = literalValue.NumericConstant.Exponent.Number;
                var mantissa = literalValue.NumericConstant.Mantissa.Number;
                int mantissaLength =
                    mantissa.ToString().Length;
                int exponentLength =
                    exponent.ToString().Length -
                    (exponent.Sign == -1 ? 1 : 0); //Check digits only, so remove '-' at beginning of negative numbers
                if (exponentLength > 4 || mantissaLength > 34)
                {
                    tokensLine.AddDiagnostic(MessageCode.InvalidExponentInFloatingPointLiteral, token);
                }
            }

        }

        public bool TryScanDecimalFloatingPointSpecialValue(int startIndex, bool isNegative,
            out DecimalFloatingPointLiteralTokenValue decimalFloatingPointLiteralTokenValue)

        {
            decimalFloatingPointLiteralTokenValue =
                new DecimalFloatingPointLiteralTokenValue(LiteralTokenValueType.DecimalFloatingPoint, null);

            for (; currentIndex <= lastIndex && IsSqlKeywordPart(line[currentIndex]); currentIndex++)
            {
            }

            string text = null;
            text = isNegative ? line.Substring(startIndex + 1, currentIndex - startIndex - 1) : line.Substring(startIndex, currentIndex - startIndex);
            var tryToGetValue = SpecialValues.TryGetValue(text, out var outPut);
            if (tryToGetValue != true) return false;
            //negative
            decimalFloatingPointLiteralTokenValue.SpecialValue =
                new DecimalFloatingPointSpecialValue(isNegative, outPut);
            return true;

        }

        private Token ScanOneCharFollowedBySpaceOrNumericLiteral(int startIndex, TokenType tokenType,
            MessageCode messageCode, bool spaceAfterIsMandatory = true)
        {
            if (currentIndex == lastIndex)
            {
                // consume one char and use the virtual space at end of line
                currentIndex++;
                return new Token(tokenType, startIndex, currentIndex - 1, true, tokensLine);
            }
            else if ((currentIndex + 1) < lastIndex && line[currentIndex + 1] == ' ')
            {
                // consume one char and consume the space char
                currentIndex += 2;
                return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
            }
            else if ((currentIndex + 1) < lastIndex && Char.IsDigit(line[currentIndex + 1]))
            {
                return ScanSqlNumericLiteral(startIndex, false);
            }
            else if ((tokenType == TokenType.PlusOperator || tokenType == TokenType.MinusOperator))
            {
                return ScanSqlNumericLiteral(startIndex, tokenType == TokenType.MinusOperator);
            }
            else
            {
                // consume one char and register an error because the following char is missing
                // even if the space is missing, try to match the expected tokenType
                currentIndex++;
                if (spaceAfterIsMandatory)
                {
                    Token invalidToken = new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
                    tokensLine.AddDiagnostic(messageCode, invalidToken, line[currentIndex], currentIndex + 1);
                    return invalidToken;
                }

                return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
            }
        }

        private Token ScanKeywordOrUserDefinedWord(int startIndex)
        {
            for (; currentIndex <= lastIndex && IsSqlKeywordPart(line[currentIndex]); currentIndex++)
            {
            }

            return new Token(TokenType.UserDefinedWord, startIndex, currentIndex - 1, tokensLine);
        }

    }

}
