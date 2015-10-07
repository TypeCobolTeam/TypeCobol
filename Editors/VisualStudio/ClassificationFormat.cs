using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;

namespace TypeCobol.Editor
{
    #region Format definition

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.invalid")]
    [Name("cobol.invalid")]
    //[UserVisible(false)] // should this be visible to the end user
    //[Order(Before = Priority.Default)] // set the priority to be after the default classifiers
    internal sealed class CobolInvalidClassificationFormat : ClassificationFormatDefinition
    {
        public CobolInvalidClassificationFormat()
        {
            //this.DisplayName = "cobol.invalid";
            this.ForegroundColor = Colors.BlueViolet;
            //this.BackgroundColor = Colors.BlueViolet;
            //this.TextDecorations = System.Windows.TextDecorations.Underline;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.whitespace")]
    [Name("cobol.whitespace")]
    internal sealed class CobolWhitespaceClassificationFormat : ClassificationFormatDefinition {
        public CobolWhitespaceClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.comment")]
    [Name("cobol.comment")]
    internal sealed class CobolCommentClassificationFormat : ClassificationFormatDefinition {
        public CobolCommentClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.separator")]
    [Name("cobol.separator")]
    internal sealed class CobolSeparatorClassificationFormat : ClassificationFormatDefinition {
        public CobolSeparatorClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.operator.arithmetic")]
    [Name("cobol.operator.arithmetic")]
    internal sealed class CobolOperatorArithmeticClassificationFormat : ClassificationFormatDefinition {
        public CobolOperatorArithmeticClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.operator.relational")]
    [Name("cobol.operator.relational")]
    internal sealed class CobolOperatorRelationalClassificationFormat : ClassificationFormatDefinition
    {
        public CobolOperatorRelationalClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.literal")]
    [Name("cobol.literal")]
    internal sealed class CobolLiteralClassificationFormat : ClassificationFormatDefinition {
        public CobolLiteralClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.symbol")]
    [Name("cobol.symbol")]
    internal sealed class CobolSymbolClassificationFormat : ClassificationFormatDefinition {
        public CobolSymbolClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword")]
    [Name("cobol.keyword")]
    internal sealed class CobolKeywordClassificationFormat : ClassificationFormatDefinition {
        public CobolKeywordClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.compilerdirective")]
    [Name("cobol.compilerdirective")]
    internal sealed class CobolCompilerDirectiveClassificationFormat : ClassificationFormatDefinition {
        public CobolCompilerDirectiveClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.internal")]
    [Name("cobol.internal")]
    internal sealed class CobolInternalClassificationFormat : ClassificationFormatDefinition {
        public CobolInternalClassificationFormat() { this.ForegroundColor = Colors.BlueViolet; }
    }

    #endregion //Format definition
}
