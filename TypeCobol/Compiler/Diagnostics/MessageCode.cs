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
        DotShouldBeFollowedBySpace = 12,
        InvalidCharAfterPlus = 13,
        InvalidCharAfterMinus = 14,
        InvalidCharAfterPseudoTextDelimiter = 15,
        InvalidNumericLiteralFormat = 16,
        InvalidMantissaInFloatingPointLiteral = 17,
        InvalidExponentInFloatingPointLiteral = 18,
        InvalidNumberOfCharsInHexaAlphaLiteral = 19,
        InvalidNumberOfCharsInHexaNationalLiteral = 20,
        InvalidDelimiterForContinuationLine = 21,
        InvalidFirstTwoCharsForContinuationLine = 22,
        AreaAOfContinuationLineMustBeBlank = 23,
        HyphenIndicatorNotPermittedInCommenEntries = 24,
        InvalidCblProcessCompilerDirective = 25,
        InvalidControlCblCompilerStatementOption = 26,
        SyntaxErrorInParser = 27,
        FailedToLoadTextDocumentReferencedByCopyDirective = 28,
        SyntaxWarningInParser = 29,
        SemanticTCErrorInParser = 30,
        ImplementationError = 31,
        TypeCobolParserLimitation = 32,
        ParserInit = 33,
        IntrinsicLoading = 34,
        DependenciesLoading = 35,
        GenerationFailled  = 36,
        Warning = 37,
        GenerationErrorLineExceed = 38,
        QuestionMarkShouldBeFollowedBySpace = 39,
        WrongFormalizedCommentKeyword = 40,
        WrongFormalizedCommentMarckupPosition = 41,
        WrongMultilineCommentMarckupPosition = 42,
        ErrorFormalizedCommentMissplaced = 43,
        MultiFormalizedCommentIndicatorMisused = 44,
        Info = 45,
        AnalyzerFailure = 46,
        //MessageCode number 47 is reserved for violations coming from a Quality analyzer, it has no corresponding DiagnosticMessage object.
        UnsupportedLanguageFeature = 48,
        //For SQL
        InvalidNumberOfCharsInBinaryStringLiteral = 49,
        InvalidNumberOfCharsInGraphicStringLiteral = 50,
        InvalidExponentInDecimalFloatingPointLiteral = 51,
        InvalidMantissaInDecimalFloatingPointLiteral = 52,
        // End for SQL
        InvalidCharBeforePseudoTextDelimiter = 53,
        ShouldBePrecededBySpace = 54,
        InvalidCharInsidePseudoText = 55,
        DirectiveSyntaxWarning = 56
    }
}