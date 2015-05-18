using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Diagnostics
{
    public enum MessageCode
    {
        InvalidIndicatorCharacter = 1,
        InvalidCharAfterComma = 2,
        SemicolonShouldBeFollowedBySpace = 3,
        PowerOperatorShouldBeFollowedBySpace = 4,
        InvalidCharAfterAsterisk = 5,
        DivideOperatorShouldBeFollowedBySpace = 6,
        LessThanOrEqualOperatorShouldBeFollowedBySpace = 7,
        InvalidCharAfterLessThan = 8,
        GreaterThanOrEqualOperatorShouldBeFollowedBySpace = 9,
        InvalidCharAfterGreaterThan = 10,
        InvalidCharAfterEquals = 11,
        InvalidCharAfterPeriod = 12,
        InvalidCharAfterPlus = 13,
        InvalidCharAfterMinus = 14,
        InvalidCharAfterPseudoTextDelimiter = 15,
        InvalidNumericLiteralFormat = 16,
        InvalidMantissaInFloatingPointLiteral = 17,
        InvalidExponentInFloatingPointLiteral = 18,
        InvalidNumberOfCharsInHexaAlphaLiteral = 19,
        InvalidNumberOfCharsInHexaNationalLiteral = 20,
        InvalidFirstCharForContinuationLine = 21,
        InvalidFirstTwoCharsForContinuationLine = 22,
        AreaAOfContinuationLineMustBeBlank = 23,
        ContinuationInsideDecimalLiteralCouldBeWrong = 24,
        ContinuationInsidePictureCharacterStringCouldBeWrong = 25,
        InvalidCblProcessCompilerDirective = 26,
        InvalidControlCblCompilerStatementOption = 27,
        SyntaxErrorInParser = 28,
        FailedToLoadTextDocumentReferencedByCopyDirective = 29
    }
}