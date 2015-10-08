using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using TypeCobol.Editor.util;

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
            this.ForegroundColor = Colors.BlueViolet;
        }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.error")]
    [Name("cobol.error")]
    internal sealed class CobolErrorClassificationFormat : ClassificationFormatDefinition
    {
        // A red   wavy underline identifies a syntax error, such as a missing semicolon or mismatched braces, or a semantic error.
        // A green wavy underline identifies a potential compiler warning.
        // A blue wavy underline identifies compiler errors.
        public CobolErrorClassificationFormat() {
            var underline = new System.Windows.TextDecoration();
            underline.Pen = Pens.CompileError;
            underline.PenThicknessUnit = System.Windows.TextDecorationUnit.FontRecommended;
            if (this.TextDecorations == null )
                this.TextDecorations = new System.Windows.TextDecorationCollection();
            this.TextDecorations.Add(underline);
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
    [ClassificationType(ClassificationTypeNames = "cobol.literal.alphanumeric")]
    [Name("cobol.literal.alphanumeric")]
    internal sealed class CobolLiteralAlphanumericClassificationFormat : ClassificationFormatDefinition {
        public CobolLiteralAlphanumericClassificationFormat() { this.ForegroundColor = Colors.Green; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.literal.numeric")]
    [Name("cobol.literal.numeric")]
    internal sealed class CobolLiteralNumericClassificationFormat : ClassificationFormatDefinition
    {
        public CobolLiteralNumericClassificationFormat() { this.ForegroundColor = Colors.Green; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.literal.syntax")]
    [Name("cobol.literal.syntax")]
    internal sealed class CobolLiteralSyntaxClassificationFormat : ClassificationFormatDefinition
    {
        public CobolLiteralSyntaxClassificationFormat() { this.ForegroundColor = Colors.Green; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.symbol")]
    [Name("cobol.symbol")]
    internal sealed class CobolSymbolClassificationFormat : ClassificationFormatDefinition {
        public CobolSymbolClassificationFormat() { this.ForegroundColor = Colors.DarkBlue; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.directive.start")]
    [Name("cobol.keyword.directive.start")]
    internal sealed class CobolKeywordDirectiveStartClassificationFormat : ClassificationFormatDefinition
    {
        public CobolKeywordDirectiveStartClassificationFormat() { this.ForegroundColor = Colors.DarkRed; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.codeelement.start")]
    [Name("cobol.keyword.codeelement.start")]
    internal sealed class CobolKeywordCodeElementStartClassificationFormat : ClassificationFormatDefinition
    {
        public CobolKeywordCodeElementStartClassificationFormat() { this.ForegroundColor = Colors.DarkRed; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.statement.start")]
    [Name("cobol.keyword.statement.start")]
    internal sealed class CobolKeywordStatementStartClassificationFormat : ClassificationFormatDefinition {
        public CobolKeywordStatementStartClassificationFormat() { this.ForegroundColor = Colors.Crimson; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.statement.end")]
    [Name("cobol.keyword.statement.end")]
    internal sealed class CobolKeywordStatementEndClassificationFormat : ClassificationFormatDefinition
    {
        public CobolKeywordStatementEndClassificationFormat() { this.ForegroundColor = Colors.Crimson; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.specialregister")]
    [Name("cobol.keyword.specialregister")]
    internal sealed class CobolKeywordSpecialRegisterClassificationFormat : ClassificationFormatDefinition
    {
        public CobolKeywordSpecialRegisterClassificationFormat() { this.ForegroundColor = Colors.DarkRed; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.figurativeconstant")]
    [Name("cobol.keyword.figurativeconstant")]
    internal sealed class CobolKeywordFigurativeConstantClassificationFormat : ClassificationFormatDefinition
    {
        public CobolKeywordFigurativeConstantClassificationFormat() { this.ForegroundColor = Colors.DarkRed; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.specialobjectidentifier")]
    [Name("cobol.keyword.specialobjectidentifier")]
    internal sealed class CobolKeywordSpecialObjectIdentifierClassificationFormat : ClassificationFormatDefinition
    {
        public CobolKeywordSpecialObjectIdentifierClassificationFormat() { this.ForegroundColor = Colors.DarkRed; }
    }

    [Export(typeof(EditorFormatDefinition))]
    [ClassificationType(ClassificationTypeNames = "cobol.keyword.syntax")]
    [Name("cobol.keyword.syntax")]
    internal sealed class CobolKeywordSyntaxClassificationFormat : ClassificationFormatDefinition
    {
        public CobolKeywordSyntaxClassificationFormat() { this.ForegroundColor = Colors.IndianRed; }
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
