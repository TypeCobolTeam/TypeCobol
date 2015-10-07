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
        [Name("cobol.separator")]
        internal static ClassificationTypeDefinition CobolSeparatorClassificationDefinition = null;

//        [Export(typeof(ClassificationTypeDefinition))]
//        [Name("cobol.operator")]
//        internal static ClassificationTypeDefinition CobolOperatorClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.operator.arithmetic")]
        internal static ClassificationTypeDefinition CobolOperatorArithmeticClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.operator.relational")]
        internal static ClassificationTypeDefinition CobolOperatorRelationalClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.literal")]
        internal static ClassificationTypeDefinition CobolLiteralClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.symbol")]
        internal static ClassificationTypeDefinition CobolSymbolClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.keyword")]
        internal static ClassificationTypeDefinition CobolKeywordClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.compilerdirective")]
        internal static ClassificationTypeDefinition CobolCompilerDirectiveClassificationDefinition = null;

        [Export(typeof(ClassificationTypeDefinition))]
        [Name("cobol.internal")]
        internal static ClassificationTypeDefinition CobolInternalClassificationDefinition = null;
    }
}
