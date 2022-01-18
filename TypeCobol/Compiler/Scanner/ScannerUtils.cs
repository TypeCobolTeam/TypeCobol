using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

public static class ScannerUtils
{
    /// <summary>
    /// Look for pattern ':' (cobol word chars)+ ':'
    /// </summary>
    /// <param name="line">The source line</param>
    /// <param name="startIndex">zero based start index in the source line</param>
    /// <param name="lastIndex">zero based last index (included) in the source line</param>
    /// <param name="interpretDoubleColonAsQualifiedNameSeparator">true to interpret :: as TypeCobol qualifier separator</param>
    /// <param name="patternEndIndex">Out last index of the : separator</param>
    /// <returns>true if the region in the line is a Partial Cobol Word, false otherwise</returns>
    public static bool CheckForPartialCobolWordPattern(string line, int startIndex, int lastIndex, 
        bool interpretDoubleColonAsQualifiedNameSeparator, out int patternEndIndex)
    {
        //This method is always called on a starting ':'
        System.Diagnostics.Debug.Assert(line[startIndex] == ':');
        patternEndIndex = -1;

        // match leading spaces if any
        int index = startIndex + 1;
        for (; index <= lastIndex && line[index] == ' '; index++)
        { }

        // match all legal cobol word chars
        for (; index <= lastIndex && CobolChar.IsCobolWordChar(line[index]); index++)
        { }

        // match trailing spaces if any
        for (; index <= lastIndex && line[index] == ' '; index++)
        { }

        // next character must be ':'
        if (line.Length > index && line[index] == ':')
        {
            // Empty tag, we only found '::'
            if (index == startIndex + 1 && interpretDoubleColonAsQualifiedNameSeparator)
            {
                return false;
            }

            // character after is another ':'
            if (line.Length > index + 1 && line[index + 1] == ':' && interpretDoubleColonAsQualifiedNameSeparator)
            {
                return false;
            }

            // Tag is properly closed
            patternEndIndex = index;
            return true;
        }

        return false;
    }

    /// <summary>
    /// Determines if the given token corresponds to a literal-1 or an identifier according to the following grammar:
    /// literal-1-or-identifier:
    ///     identifier 
    /// |   numericValue 
    /// |   alphanumericValue3;
    /// 
    /// numericValue: 
    ///    numericLiteralToken | numericFigurativeConstant;
    /// 
    /// numericLiteralToken:
    ///    (IntegerLiteral | DecimalLiteral | FloatingPointLiteral);
    /// 
    /// numericFigurativeConstant: (ZERO | ZEROS | ZEROES);
    /// 
    /// alphanumericValue3:
    ///    alphanumericOrNationalLiteralToken | figurativeConstant;
    /// 
    /// alphanumericOrNationalLiteralToken:
    ///    (AlphanumericLiteral | HexadecimalAlphanumericLiteral | NullTerminatedAlphanumericLiteral |
    ///         DBCSLiteral | NationalLiteral | HexadecimalNationalLiteral);
    ///         
    /// figurativeConstant: (HIGH_VALUE | HIGH_VALUES |
    /// LOW_VALUE  | LOW_VALUES |
    /// QUOTE | QUOTES |
    /// SPACE | SPACES |
    /// ZERO  | ZEROS  | ZEROES) |
    /// symbolicCharacterReference;
    /// 
    /// symbolicCharacterReference: standardCollatingSequenceReference | SymbolicCharacter;
    /// standardCollatingSequenceReference: STANDARD_1 | STANDARD_2 | NATIVE;
    /// 
    /// </summary>
    /// <param name="token"></param>
    /// <returns></returns>
    internal static bool IsIdentifierOrLiteral1(Token token)
    {
        switch (token.TokenType)
        {
            case TokenType.UserDefinedWord:
            case TokenType.IntegerLiteral:
            case TokenType.DecimalLiteral:
            case TokenType.FloatingPointLiteral:
            case TokenType.ZERO:
            case TokenType.ZEROS:
            case TokenType.ZEROES:
            case TokenType.AlphanumericLiteral:
            case TokenType.HexadecimalAlphanumericLiteral:
            case TokenType.NullTerminatedAlphanumericLiteral:
            case TokenType.DBCSLiteral:
            case TokenType.NationalLiteral:
            case TokenType.HexadecimalNationalLiteral:
            case TokenType.HIGH_VALUE:
            case TokenType.HIGH_VALUES:
            case TokenType.LOW_VALUE:
            case TokenType.LOW_VALUES:
            case TokenType.QUOTE:
            case TokenType.QUOTES:
            case TokenType.SPACE:
            case TokenType.SPACES:
            case TokenType.SymbolicCharacter:
            case TokenType.STANDARD_1:
            case TokenType.STANDARD_2:
            case TokenType.NATIVE:
                return true;
            default:
                return false;
        }
    }
}
