using System;
using System.Collections.Generic;
using System.ComponentModel.Composition;
using System.Windows.Media;
using Microsoft.VisualStudio.Text;
using Microsoft.VisualStudio.Text.Classification;
using Microsoft.VisualStudio.Utilities;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Editor
{

    #region Provider definition
    /// <summary>
    /// This class causes a classifier to be added to the set of classifiers. Since 
    /// the content type is set to "text", this classifier applies to all text files
    /// </summary>
    [Export(typeof(IClassifierProvider))]
    [ContentType("text")]
    internal class ClassifierProvider : IClassifierProvider
    {
        /// <summary>
        /// Import the classification registry to be used for getting a reference
        /// to the custom classification type later.
        /// </summary>
        [Import]
        internal IClassificationTypeRegistryService ClassificationRegistry = null; // Set via MEF

        public IClassifier GetClassifier(ITextBuffer buffer)
        {
            return buffer.Properties.GetOrCreateSingletonProperty<Classifier>(delegate { return new Classifier(ClassificationRegistry); });
        }
    }
    #endregion //provider def

    /// <summary>
    /// Classifier that classifies all text as an instance of the OrinaryClassifierType
    /// </summary>
    class Classifier : IClassifier
    {
        private IClassificationTypeRegistryService registry;
        private Parser parser;

        internal Classifier(IClassificationTypeRegistryService registry)
        {
            this.registry = registry;
            this.parser = new Parser("V$");
        }

        /// <summary>
        /// This method scans the given SnapshotSpan for potential matches for this classification.
        /// In this instance, it classifies everything and returns each span as a new ClassificationSpan.
        /// </summary>
        /// <param name="trackingSpan">The span currently being classified</param>
        /// <returns>A list of ClassificationSpans that represent spans identified to be of this classification</returns>
        public IList<ClassificationSpan> GetClassificationSpans(SnapshotSpan span)
        {
            List<ClassificationSpan> spans = new List<ClassificationSpan>();
            parser.Parse(new TextString(span.GetText()));
            foreach (var e in parser.CodeElements)
            {
                foreach (var token in e.ConsumedTokens)
                {
                    IClassificationType type = GetClassificationType(token.TokenFamily);
                    spans.Add(new ClassificationSpan(new SnapshotSpan(span.Start + token.StartIndex, token.Length), type));
                }
            }
            foreach (var e in parser.Errors)
            {
                IClassificationType type = registry.GetClassificationType("cobol.error");
                System.Console.WriteLine("Error on: \"" + new SnapshotSpan(span.Start+e.ColumnStart-1, span.Start+e.ColumnEnd).GetText()+"\"");
                spans.Add(new ClassificationSpan(new SnapshotSpan(span.Start+e.ColumnStart-1, span.Start+e.ColumnEnd), type));
            }
            return spans;
        }

        private IClassificationType GetClassificationType(TokenFamily family)
        {
            string type;
            if (!mapTokenFamilyToClassificationType.TryGetValue(family, out type))
                throw new ArgumentException("Unknown token family \""+family.ToString()+"\"");
            return registry.GetClassificationType(type);
        }

        private static Dictionary<TokenFamily, string> mapTokenFamilyToClassificationType = new Dictionary<TokenFamily, string>()
        {
            { TokenFamily.Invalid, "cobol.invalid" },
            { TokenFamily.Whitespace, "cobol.whitespace" },
            { TokenFamily.Comments, "cobol.comment" },
            { TokenFamily.SyntaxSeparator, "cobol.separator.syntax" },
            { TokenFamily.ArithmeticOperator, "cobol.operator.arithmetic" },
            { TokenFamily.RelationalOperator, "cobol.operator.relational" },
            { TokenFamily.AlphanumericLiteral, "cobol.literal.alphanumeric" },
            { TokenFamily.NumericLiteral, "cobol.literal.numeric" },
            { TokenFamily.SyntaxLiteral, "cobol.literal.syntax" },
            { TokenFamily.Symbol, "cobol.symbol" },
            { TokenFamily.CompilerDirectiveStartingKeyword, "cobol.keyword.directive.start" },
            { TokenFamily.CodeElementStartingKeyword, "cobol.keyword.codeelement.start" },
            { TokenFamily.StatementStartingKeyword, "cobol.keyword.statement.start" },
            { TokenFamily.StatementEndingKeyword, "cobol.keyword.statement.end" },
            { TokenFamily.SpecialRegisterKeyword, "cobol.keyword.specialregister" },
            { TokenFamily.FigurativeConstantKeyword, "cobol.keyword.figurativeconstant" },
            { TokenFamily.SpecialObjetIdentifierKeyword, "cobol.keyword.specialobjectidentifier" },
            { TokenFamily.SyntaxKeyword, "cobol.keyword.syntax" },
            { TokenFamily.CompilerDirective, "cobol.compilerdirective" },
            { TokenFamily.InternalTokenGroup, "cobol.internal" },
        };

#pragma warning disable 67
        // This event gets raised if a non-text change would affect the classification in some way,
        // for example typing /* would cause the classification to change in C# without directly
        // affecting the span.
        public event EventHandler<ClassificationChangedEventArgs> ClassificationChanged;
#pragma warning restore 67
    }
}
