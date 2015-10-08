using System.ComponentModel.Composition;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace TypeCobol.Editor
{
    internal static class ClassificationType
    {
        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol")]
        internal static ClassificationTypeDefinition CobolClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.invalid")]
        internal static ClassificationTypeDefinition CobolInvalidClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.whitespace")]
        internal static ClassificationTypeDefinition CobolWhitespaceClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.comment")]
        internal static ClassificationTypeDefinition CobolCommentClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.separator.syntax")]
        internal static ClassificationTypeDefinition CobolSeparatorClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.operator.arithmetic")]
        internal static ClassificationTypeDefinition CobolOperatorArithmeticClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.operator.relational")]
        internal static ClassificationTypeDefinition CobolOperatorRelationalClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.literal.alphanumeric")]
        internal static ClassificationTypeDefinition CobolLiteralAlphanumericClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.literal.numeric")]
        internal static ClassificationTypeDefinition CobolLiteralNumericClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.literal.syntax")]
        internal static ClassificationTypeDefinition CobolLiteralSyntaxClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.symbol")]
        internal static ClassificationTypeDefinition CobolSymbolClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.directive.start")]
        internal static ClassificationTypeDefinition CobolKeywordDirectiveStartClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.codeelement.start")]
        internal static ClassificationTypeDefinition CobolKeywordCodeElementStartClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.statement.start")]
        internal static ClassificationTypeDefinition CobolKeywordStatementStartClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.statement.end")]
        internal static ClassificationTypeDefinition CobolKeywordStatementEndClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.specialregister")]
        internal static ClassificationTypeDefinition CobolKeywordSpecialRegisterClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.figurativeconstant")]
        internal static ClassificationTypeDefinition CobolKeywordFigurativeConstantClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.specialobjectidentifier")]
        internal static ClassificationTypeDefinition CobolKeywordSpecialObjectIdentifierClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword.syntax")]
        internal static ClassificationTypeDefinition CobolKeywordSyntaxClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.compilerdirective")]
        internal static ClassificationTypeDefinition CobolCompilerDirectiveClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.internal")]
        internal static ClassificationTypeDefinition CobolInternalClassificationDefinition = null;
    }
}
