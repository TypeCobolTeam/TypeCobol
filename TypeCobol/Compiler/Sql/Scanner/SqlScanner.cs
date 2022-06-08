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
        private enum Sign
        {
            Plus,
            Minus
        }

        private const char SQL_DECIMAL_POINT = '.';
        private const int SQL_MAX_INTEGER_LENGTH = 19;
        private const int SQL_MAX_DECIMAL_LENGTH = 31;
        private const int SQL_MAX_FLOATING_POINT_EXPONENT_LENGTH = 2;
        private const int SQL_MAX_FLOATING_POINT_MANTISSA_LENGTH = 17;
        private const int SQL_MAX_DECIMAL_FLOATING_POINT_EXPONENT_LENGTH = 4;
        private const int SQL_MAX_DECIMAL_FLOATING_POINT_MANTISSA_LENGTH = 34;

        private static readonly Dictionary<string, DecimalFloatingPointSpecialValueType> _SpecialValues =
            new Dictionary<string, DecimalFloatingPointSpecialValueType>(StringComparer.OrdinalIgnoreCase)
            {
                { "NaN", DecimalFloatingPointSpecialValueType.NaN },
                { "SNaN", DecimalFloatingPointSpecialValueType.SNaN },
                { "Infinity", DecimalFloatingPointSpecialValueType.Infinity },
                { "Inf", DecimalFloatingPointSpecialValueType.Infinity }
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
                case ':':
                    return ScanOneChar(startIndex, TokenType.ColonSeparator);
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
                    return ScanSqlNumericLiteral(startIndex, null);
                case '+':
                    return ScanOperatorFollowedBySpaceOrNumericLiteral(startIndex, TokenType.PlusOperator);
                case '-':
                    return ScanOperatorFollowedBySpaceOrNumericLiteral(startIndex, TokenType.MinusOperator);
                case 'I':
                case 'N':
                case 'S':
                case 'i':
                case 'n':
                case 's':
                    if (TryScanDecimalFloatingPointSpecialValue(startIndex, null, out var tokenValue))
                    {
                        Token decimalFloatingPointSpecialValueToken =
                            new Token(TokenType.SQL_DecimalFloatingPointLiteral, startIndex, currentIndex - 1,
                                tokensLine)
                            {
                                LiteralValue = tokenValue
                            };
                        return decimalFloatingPointSpecialValueToken;
                    }

                    return ScanKeyWordOrUserDefinedWord(startIndex);
                case '\'':
                case '"':
                    return ScanAlphanumericLiteral(startIndex, TokenType.AlphanumericLiteral, null);
                case 'X':
                case 'x':
                    if (startIndex < lastIndex - 1 && (line[startIndex + 1] == '\'' || line[startIndex + 1] == '"'))
                    {
                        currentIndex++;
                        return ScanAlphanumericLiteral(startIndex, TokenType.HexadecimalAlphanumericLiteral, null);
                    }

                    return ScanKeyWordOrUserDefinedWord(startIndex);
                case 'B':
                case 'b':
                    if (startIndex + 2 <= lastIndex && (line[startIndex + 1] == 'x' || line[startIndex + 1] == 'X') &&
                        (line[startIndex + 2] == '\'' || line[startIndex + 2] == '"'))
                    {
                        currentIndex += 2;
                        return ScanAlphanumericLiteral(startIndex, TokenType.SQL_BinaryStringLiteral, null);
                    }

                    return ScanKeyWordOrUserDefinedWord(startIndex);
                case 'U':
                case 'u':
                case 'G':
                case 'g':
                    if (startIndex + 2 <= lastIndex && (line[startIndex + 1] == 'x' || line[startIndex + 1] == 'X') &&
                        (line[startIndex + 2] == '\'' || line[startIndex + 2] == '"'))
                    {
                        currentIndex += 2;
                        return ScanAlphanumericLiteral(startIndex, TokenType.SQL_GraphicStringLiteral, null);
                    }

                    return ScanKeyWordOrUserDefinedWord(startIndex);
                default:
                    return ScanKeyWordOrUserDefinedWord(startIndex);
            }
        }

        private Token ScanSqlNumericLiteral(int startIndex, Sign? sign)
        {
            if (TryScanDecimalFloatingPointSpecialValue(startIndex, sign, out var decimalFloatingPointLiteralTokenValue))
            {
                return new Token(TokenType.SQL_DecimalFloatingPointLiteral, startIndex, currentIndex - 1, tokensLine)
                       {
                           LiteralValue = decimalFloatingPointLiteralTokenValue
                       };
            }

            var token = ScanNumericLiteral(startIndex, SQL_DECIMAL_POINT);
            switch (token.TokenType)
            {
                case TokenType.IntegerLiteral:
                    CheckIntegerLiteral();
                    break;
                case TokenType.DecimalLiteral:
                    CheckDecimalLiteral();
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
                int literalValueLength = literalValue.Number.ToString().Length - (literalValue.HasSign ? 1 : 0);

                //Check IntegerLiteral range
                if (literalValueLength > SQL_MAX_INTEGER_LENGTH)
                {
                    //Too many digits for IntegerLiteral, try DecimalLiteral
                    token.CorrectType(TokenType.DecimalLiteral);
                    token.LiteralValue = new DecimalLiteralTokenValue(literalValue);
                    CheckDecimalLiteral();
                }
            }

            void CheckDecimalLiteral()
            {
                Debug.Assert(token.LiteralValue is DecimalLiteralTokenValue);
                var literalValue = (DecimalLiteralTokenValue)token.LiteralValue;
                int literalValueLength = literalValue.IntegerValue.ToString().Length - (literalValue.HasSign ? 1 : 0);

                //Check DecimalLiteral range
                if (literalValueLength > SQL_MAX_DECIMAL_LENGTH)
                {
                    //Out of range
                    this.tokensLine.AddDiagnostic(MessageCode.SyntaxErrorInParser, token, "Decimal is too big: " + literalValue.Number);
                }
            }

            void CheckFloatingPointLiteral()
            {
                Debug.Assert(token.LiteralValue is FloatingPointLiteralTokenValue);
                var literalValue = (FloatingPointLiteralTokenValue)token.LiteralValue;

                //Check exponent and mantissa length
                var exponent = literalValue.Exponent;
                int exponentLength = exponent.Number.ToString().Length - (exponent.HasSign ? 1 : 0);
                var mantissa = literalValue.Mantissa;
                int mantissaLength = mantissa.IntegerValue.ToString().Length - (mantissa.HasSign ? 1 : 0);
                if (exponentLength > SQL_MAX_FLOATING_POINT_EXPONENT_LENGTH || mantissaLength > SQL_MAX_FLOATING_POINT_MANTISSA_LENGTH)
                {
                    //Try DecimalFloatingPointLiteral
                    token.CorrectType(TokenType.SQL_DecimalFloatingPointLiteral);
                    token.LiteralValue = new DecimalFloatingPointLiteralTokenValue(literalValue);
                    CheckDecimalFloatingPointLiteral(exponentLength, mantissaLength);
                }
            }

            void CheckDecimalFloatingPointLiteral(int exponentLength, int mantissaLength)
            {
                Debug.Assert(token.LiteralValue is DecimalFloatingPointLiteralTokenValue);
                if (exponentLength > SQL_MAX_DECIMAL_FLOATING_POINT_EXPONENT_LENGTH)
                {
                    tokensLine.AddDiagnostic(MessageCode.InvalidExponentInDecimalFloatingPointLiteral, token);
                }

                if (mantissaLength > SQL_MAX_DECIMAL_FLOATING_POINT_MANTISSA_LENGTH)
                {
                    tokensLine.AddDiagnostic(MessageCode.InvalidMantissaInDecimalFloatingPointLiteral, token);
                }
            }
        }

        private bool TryScanDecimalFloatingPointSpecialValue(int startIndex, Sign? sign,
            out DecimalFloatingPointLiteralTokenValue decimalFloatingPointLiteralTokenValue)
        {
            // Read letters but do not consume chars yet
            int start = sign.HasValue ? startIndex + 1 : startIndex;
            int index;
            for (index = start; index <= lastIndex && char.IsLetter(line[index]); index++)
            {
            }

            string text = line.Substring(start, index - start);
            if (_SpecialValues.TryGetValue(text, out var specialValueType))
            {
                //Consume chars and create value
                currentIndex = index;
                bool isNegative = sign.HasValue && sign.Value == Sign.Minus;
                decimalFloatingPointLiteralTokenValue =
                    new DecimalFloatingPointLiteralTokenValue(
                        new DecimalFloatingPointSpecialValue(isNegative, specialValueType));
                return true;
            }

            // No match
            decimalFloatingPointLiteralTokenValue = null;
            return false;
        }

        private Token ScanOperatorFollowedBySpaceOrNumericLiteral(int startIndex, TokenType operatorType)
        {
            Debug.Assert(operatorType == TokenType.PlusOperator || operatorType == TokenType.MinusOperator);

            if (currentIndex == lastIndex)
            {
                // consume one char and use the virtual space at end of line
                currentIndex++;
                return new Token(operatorType, startIndex, currentIndex - 1, true, tokensLine);
            }

            if (currentIndex + 1 < lastIndex && line[currentIndex + 1] == ' ')
            {
                // consume one char and consume the space char
                currentIndex += 2;
                return new Token(operatorType, startIndex, currentIndex - 1, tokensLine);
            }

            Sign sign = operatorType == TokenType.PlusOperator ? Sign.Plus : Sign.Minus;
            return ScanSqlNumericLiteral(startIndex, sign);
        }

        private Token ScanKeyWordOrUserDefinedWord(int startIndex)
        {
            //Consume all sql-keyword compatible chars
            for (; currentIndex <= lastIndex && IsSqlKeywordPart(line[currentIndex]); currentIndex++)
            {
            }

            //Try to match keyword text
            string tokenText = line.Substring(startIndex, currentIndex - startIndex);
            var tokenType = TokenUtils.GetSqlKeywordTokenTypeFromTokenString(tokenText);
            return new Token(tokenType, startIndex, currentIndex - 1, tokensLine);
        }
    }
}
