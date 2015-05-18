using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Highlighting;
using ICSharpCode.AvalonEdit.Rendering;
using System;
using System.Windows.Media;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobolStudio.Editor
{
	/// <summary>
	/// Syntax Coloring for the TypeCobol language
	/// </summary>
	internal class SyntaxHighlighter : DocumentColorizingTransformer
	{
		protected override void ColorizeLine(DocumentLine line)
		{
            TypeCobolCompilerService compilerService = CurrentContext.TextView.GetService(typeof(TypeCobolCompilerService)) as TypeCobolCompilerService;
            if (compilerService != null)
            {
                int lineIndex = line.LineNumber - 1;
                if (compilerService.CompilationUnit.TokensDocument.TokensLines.Count > lineIndex)
                {
                    TokensLine tokensLine = compilerService.CompilationUnit.TokensDocument.TokensLines[lineIndex];
                    TextLineMap lineMap = tokensLine.TextLineMap;
                    int lineStartOffset = line.Offset;

                    // Areas
                    if (!lineMap.SequenceNumber.IsEmpty)
                    {
                        ApplyTextAreaStyle(lineStartOffset, lineMap.SequenceNumber, TokenStyles.GetAreaStyle(TextAreaType.SequenceNumber));
                    }
                    if (!lineMap.Indicator.IsEmpty)
                    {
                        if (lineMap.Type == TextLineType.Comment)
                        {
                            ApplyTextAreaStyle(lineStartOffset, lineMap.Indicator, TokenStyles.GetAreaStyle(TextAreaType.Comment));
                        }
                        else if (lineMap.Type == TextLineType.Debug || lineMap.Type == TextLineType.Continuation)
                        {
                            ApplyTextAreaStyle(lineStartOffset, lineMap.Indicator, TokenStyles.GetAreaStyle(TextAreaType.Indicator));
                        }
                    }
                    if (!lineMap.Comment.IsEmpty)
                    {
                        ApplyTextAreaStyle(lineStartOffset, lineMap.Comment, TokenStyles.GetAreaStyle(TextAreaType.Comment));
                    }

                    // Tokens
                    foreach (Token token in tokensLine.SourceTokens)
                    {
                        HighlightingColor tokenStyle = TokenStyles.GetTokenStyle(token);
                        if (tokenStyle != null && token.Length > 0)
                        {
                            ApplyTokenStyle(lineStartOffset, token, tokenStyle);
                        }
                    }
                }
            }
		}
                
        private void ApplyTextAreaStyle(int lineStartOffset, TextArea textArea, HighlightingColor areaStyle)
        {
            base.ChangeLinePart(lineStartOffset + textArea.StartIndex, // startOffset
                                lineStartOffset + textArea.EndIndex + 1, // endOffset
                                visualLineElement => ApplyColorToElement(visualLineElement, areaStyle));
        }

        private void ApplyTokenStyle(int lineStartOffset, Token token, HighlightingColor tokenStyle)
        {
            base.ChangeLinePart(lineStartOffset + token.StartIndex, // startOffset
                                lineStartOffset + token.StopIndex + 1, // endOffset
                                visualLineElement => ApplyColorToElement(visualLineElement, tokenStyle));
        }

        internal void ApplyColorToElement(VisualLineElement element, HighlightingColor color)
        {
            if (color.Foreground != null)
            {
                Brush b = color.Foreground.GetBrush(CurrentContext);
                if (b != null)
                    element.TextRunProperties.SetForegroundBrush(b);
            }
            if (color.Background != null)
            {
                Brush b = color.Background.GetBrush(CurrentContext);
                if (b != null)
                    element.BackgroundBrush = b;
            }
            if (color.FontStyle != null || color.FontWeight != null)
            {
                Typeface tf = element.TextRunProperties.Typeface;
                element.TextRunProperties.SetTypeface(new Typeface(
                    tf.FontFamily,
                    color.FontStyle ?? tf.Style,
                    color.FontWeight ?? tf.Weight,
                    tf.Stretch
                ));
            }
        }
	}
}
