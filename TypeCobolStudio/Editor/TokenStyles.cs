using ICSharpCode.AvalonEdit.Highlighting;
using System;
using System.Windows;
using System.Windows.Media;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobolStudio.Editor
{
    internal static class TokenStyles
    {
        // --- TextArea styles ---

        public static HighlightingColor SequenceNumberAreaStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0xDD, 0xDD, 0xDD))
        };

        public static HighlightingColor IndicatorAreaStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0xEE, 0xEE, 0xEE)),
            Background = new SimpleHighlightingBrush(Color.FromRgb(0x88, 0x88, 0x88))
        };

        public static HighlightingColor CommentAreaStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0xDD, 0xDD, 0xDD))
        };

        // ---

        public static HighlightingColor GetAreaStyle(TextAreaType area)
        {
            if(area == TextAreaType.SequenceNumber)
            {
                return SequenceNumberAreaStyle;
            }
            else if(area == TextAreaType.Indicator)
            {
                return IndicatorAreaStyle;
            }
            else if(area == TextAreaType.Comment)
            {
                return CommentAreaStyle;
            }
            return null;
        }

        // --- Token styles ---

        public static HighlightingColor CommentStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0x00, 0x88, 0x00))
        };

        public static HighlightingColor CobolVerbStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0x88, 0x00, 0x00)),
            FontWeight = FontWeights.Bold
        };

        public static HighlightingColor CobolKeywordStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0x88, 0x00, 0x00))
        };

        public static HighlightingColor CobolAlphanumericLiteralStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0x88, 0x00, 0x88))
        };

        public static HighlightingColor CobolNumericLiteralStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0x00, 0x00, 0x88))
        };

        public static HighlightingColor CobolUserDefinedWordStyle = new HighlightingColor()
        {
            Foreground = new SimpleHighlightingBrush(Color.FromRgb(0x00, 0x00, 0x00)),
            FontStyle = FontStyles.Italic
        };

        // ---

        public static HighlightingColor GetTokenStyle(Token token)
        {
            if (token.TokenFamily == TokenFamily.Comments)
            {
                return CommentStyle;
            }
            else if (token.TokenFamily == TokenFamily.StatementStartingKeyword || token.TokenFamily == TokenFamily.StatementEndingKeyword)
            {
                return CobolVerbStyle;
            }
            else if (token.TokenFamily == TokenFamily.SyntaxKeyword)
            {
                return CobolKeywordStyle;
            }
            else if (token.TokenFamily == TokenFamily.AlphanumericLiteral)
            {
                return CobolAlphanumericLiteralStyle;
            }
            else if (token.TokenFamily == TokenFamily.NumericLiteral)
            {
                return CobolNumericLiteralStyle;
            }
            else if (token.TokenFamily == TokenFamily.Symbol)
            {
                return CobolUserDefinedWordStyle;
            }
            return null;
        }


    }
}
