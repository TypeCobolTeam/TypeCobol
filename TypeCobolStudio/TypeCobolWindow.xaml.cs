using ICSharpCode.AvalonEdit.Document;
using ICSharpCode.AvalonEdit.Rendering;
using ICSharpCode.AvalonEdit.Search;
using Microsoft.Win32;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.TypeChecker;
using TypeCobolStudio.Editor;

namespace TypeCobolStudio
{
    /// <summary>
    /// Logique d'interaction pour TypeCobolEditor.xaml
    /// </summary>
    public partial class TypeCobolWindow : Window, IObserver<IList<CompilationError>>
    {
        readonly string DEFAULT_DIRECTORY = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments);
        readonly DocumentFormat DEFAULT_DOCUMENT_FORMAT = DocumentFormat.ZOsReferenceFormat;

        public TypeCobolWindow()
        {
            InitializeComponent();
            
            // Default directory at startup
            currentProject = new CompilationProject("project",
                DEFAULT_DIRECTORY, new string[] { ".txt", "*.cbl", "*.cpy" },
                DEFAULT_DOCUMENT_FORMAT.Encoding, DEFAULT_DOCUMENT_FORMAT.EndOfLineDelimiter, DEFAULT_DOCUMENT_FORMAT.FixedLineLength, DEFAULT_DOCUMENT_FORMAT.ColumnsLayout, new TypeCobolOptions());
     
            // Default compiler options
            compilerOptions = new TypeCobolOptions();

            // Incremental TypeCobol compilation              
            TypeCobolCompilerService.AttachIncrementalCompilerToTextEditorDocument(textEditor, currentProject, null, compilerOptions, this);
        }

        #region Load / Save source file

        CompilationProject currentProject;
        TypeCobolOptions compilerOptions;

        void openFileClick(object sender, RoutedEventArgs e)
        {
            OpenFileDialog dlg = new OpenFileDialog();
            dlg.CheckFileExists = true;
            if (Directory.Exists(DEFAULT_DIRECTORY))
            {
                dlg.InitialDirectory = DEFAULT_DIRECTORY;
            }
            if (dlg.ShowDialog() ?? false)
            {
                currentProject = new CompilationProject("project",
                    Path.GetDirectoryName(dlg.FileName), new string[] { ".txt", "*.cbl", "*.cpy" },
                    DEFAULT_DOCUMENT_FORMAT.Encoding, DEFAULT_DOCUMENT_FORMAT.EndOfLineDelimiter, DEFAULT_DOCUMENT_FORMAT.FixedLineLength, DEFAULT_DOCUMENT_FORMAT.ColumnsLayout, new TypeCobolOptions());
                string textName = Path.GetFileNameWithoutExtension(dlg.FileName);
                TypeCobolCompilerService.AttachIncrementalCompilerToTextEditorDocument(textEditor, currentProject, textName, compilerOptions, this);
            }
        }

        void saveFileClick(object sender, EventArgs e)
        {
            // throw new NotImplementedException();
        }

        #endregion
                
        public void OnNext(IList<CompilationError> errors)
        {
            errorsListView.ItemsSource = errors;
        }

        public void OnCompleted()
        {
            throw new NotImplementedException();
        }

        public void OnError(Exception error)
        {
            throw new NotImplementedException();
        }

        // Jump to error line in source text if an error is clicked in the list

        private void errorsListView_MouseDoubleClick(object sender, MouseButtonEventArgs e)
        {
            CompilationError error = ((FrameworkElement)e.OriginalSource).DataContext as CompilationError;
            if (error != null)
            {
                textEditor.ScrollToLine(error.LineNumber - 1);
                DocumentLine line = textEditor.Document.Lines[error.LineNumber - 1];
                textEditor.Select(line.Offset, line.Length);
            }
        }        
    }
}
