using ICSharpCode.AvalonEdit;
using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Rendering;
using ICSharpCode.AvalonEdit.Search;
using System;
using System.Collections.Generic;
using System.Windows.Media;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;
using TypeCobol.LanguageServices.Editor;

namespace TypeCobolStudio.Editor
{
    /// <summary>
    /// Specialized TextEditor used to write TypeCobol programs :
    /// - ITextDocument interface
    /// - syntax coloring
    /// - red squiggles and tooltips to show errors 
    /// </summary>
    public class TypeCobolEditor : TextEditor, ILanguageEditor
    {
        // Internal representation of a TextDocument
        AvalonEditTextDocument textDocument;

        /// <summary>
        /// Text document currrently displayed by the editor 
        /// </summary>
        public ITextDocument TextDocument { get { return textDocument; } }

        // Editor services
        SyntaxHighlighter syntaxHighlighter;
        ErrorMarker errorMarker;
        TooltipManager tooltipManager;

        /// <summary>
        /// Opens an empty editor
        /// </summary>
        public TypeCobolEditor() : this(null)
        { }

        /// <summary>
        /// Loads the content of a source file in the editor
        /// </summary>
        public TypeCobolEditor(CobolFile sourceFile)
        {
           TextView textView = TextArea.TextView;

            // Default text style
			FontFamily = new FontFamily("Consolas");
            FontSize = 13;

            // Show line numbers
            ShowLineNumbers = true;

            // Activate search box
            SearchPanel.Install(this);

            // Plug in TypeCobol syntax highlighting
            syntaxHighlighter = new SyntaxHighlighter();
            textView.LineTransformers.Add(syntaxHighlighter);
            
            // Plug in TypeCobol error marker
            errorMarker = new ErrorMarker(this);
            textView.BackgroundRenderers.Add(errorMarker);
            textView.LineTransformers.Add(errorMarker);

            // Tooltip management
            tooltipManager = new TooltipManager(this, errorMarker);
            textView.MouseHover += tooltipManager.MouseHover;
            textView.MouseHoverStopped += tooltipManager.MouseHoverStopped;
            textView.VisualLinesChanged += tooltipManager.VisualLinesChanged;

            // Initial load of the cobol file if necessary
            if (sourceFile != null)
            {
                Document = new ICSharpCode.AvalonEdit.Document.TextDocument(sourceFile.ReadChars());
            }

            // Wrap the AvalonEdit.Document in a ITextDocument interface
            textDocument = new AvalonEditTextDocument(Document, IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), ColumnsLayout.CobolReferenceFormat);
        }

        // --- Handle Scanner and error events ---

        public void OnNext(DocumentChangedEvent<ITokensLine> tokensLinesChangedEvent)
        {
            // Refresh the display of all the lines which were not directly modified
            // but which were rescanned because of a scanner state change on a previous or following line
            foreach (var documentChange in tokensLinesChangedEvent.DocumentChanges)
            {
                DocumentLine rescannedLine = Document.GetLineByNumber(documentChange.LineIndex);
                TextArea.TextView.Redraw(rescannedLine.Offset, rescannedLine.Length);
            }
        }

        public void OnNext(IList<CompilationError> errors)
        {
            // Reset all error markers
            errorMarker.Clear();

            // Adujst the error markers to the new errors list
            if (Document.TextLength > 0)
            {
                foreach (CompilationError error in errors)
                {
                    int startOffset = Document.GetOffset(new TextLocation(error.LineNumber, error.StartColumn));
                    int endOffset = Document.GetOffset(new TextLocation(error.LineNumber, error.EndColumn));
                    int length = endOffset - startOffset;
                    if (length < 2)
                    {
                        length = Math.Min(2, Document.TextLength - startOffset);
                    }
                    errorMarker.Create(startOffset, length, error.Message);
                }
            }
        }
        
        public void OnCompleted()
        {
            // Nothing to do
        }

        public void OnError(Exception error)
        {
            // Nothing to do
        }
        
    }
}
