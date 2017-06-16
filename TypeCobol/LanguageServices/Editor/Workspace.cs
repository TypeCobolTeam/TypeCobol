using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;

namespace TypeCobol.LanguageServices.Editor
{
    /// <summary>
    /// Represents a workspace in an Integrated Development Environment :
    /// - a given set of source files
    /// - a common set of configuration properties for these files
    /// - several source documents opened in interactive text editors
    /// - continuous compilation of these source documents in the background
    /// - cached compilation results
    /// - language services based on these compilation results
    /// </summary>
    public class Workspace
    {
        /// <summary>
        /// Given set of source files.
        /// Common set of configuration properties for these files.
        /// Cached compilation results.
        /// </summary>
        private CompilationProject compilationProject;

        /// <summary>
        /// Source documents opened in interactive text editors.
        /// Continuous compilation of these source documents in the background.
        /// Cached compilation results.
        /// </summary>
        public IDictionary<string, FileCompiler> OpenedFileCompilers { get; private set; }
        

        public Workspace(string workspaceName, string rootDirectory, string[] fileExtensions, 
                         Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength, ColumnsLayout columnsLayout, 
                         TypeCobolOptions compilationOptions)
        {
            compilationProject = new CompilationProject(workspaceName, rootDirectory, fileExtensions, encoding, endOfLineDelimiter, fixedLineLength, columnsLayout, compilationOptions);
            OpenedFileCompilers = new Dictionary<string, FileCompiler>(3);
        }

        /// <summary>
        /// Start continuous background compilation on a newly opened file
        /// </summary>
        public void OpenSourceFile(string fileName, string sourceText)
        {
            ITextDocument initialTextDocumentLines = new ReadOnlyTextDocument(fileName, compilationProject.Encoding, compilationProject.ColumnsLayout, sourceText);
            FileCompiler fileCompiler = new FileCompiler(initialTextDocumentLines, compilationProject.SourceFileProvider, compilationProject, compilationProject.CompilationOptions, false, compilationProject);
            //Create our own empty Symbol table.
            SymbolTable table = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
            fileCompiler.CompilationResultsForProgram.CustomSymbols = table;
            fileCompiler.CompilationResultsForProgram.UpdateTokensLines();
            lock (OpenedFileCompilers)
            {
                OpenedFileCompilers.Add(fileName, fileCompiler);
            }
            fileCompiler.CompilationResultsForProgram.SetOwnerThread(Thread.CurrentThread);
            fileCompiler.StartContinuousBackgroundCompilation(200,500,1000,3000);
        }

        /// <summary>
        /// Update the text contents of the file
        /// </summary>
        public void UpdateSourceFile(string fileName, TextChangedEvent textChangedEvent, bool bAsync)
        {
            FileCompiler fileCompilerToUpdate = null;
            if (OpenedFileCompilers.TryGetValue(fileName, out fileCompilerToUpdate))
            {
                fileCompilerToUpdate.CompilationResultsForProgram.UpdateTextLines(textChangedEvent);
                fileCompilerToUpdate.CompilationResultsForProgram.UpdateTokensLines();

                if (!bAsync)
                {//Don't wait asynchoneous snapshot refresh.
                    fileCompilerToUpdate.CompilationResultsForProgram.RefreshTokensDocumentSnapshot();
                    fileCompilerToUpdate.CompilationResultsForProgram.RefreshProcessedTokensDocumentSnapshot();
                    fileCompilerToUpdate.CompilationResultsForProgram.RefreshCodeElementsDocumentSnapshot();
                    fileCompilerToUpdate.CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot();
                }
            }
        }

        /// <summary>
        /// Stop continuous background compilation after a file has been closed
        /// </summary>
        public void CloseSourceFile(string fileName)
        {
            FileCompiler fileCompilerToClose = null;
            lock (OpenedFileCompilers)
            {
                if (OpenedFileCompilers.ContainsKey(fileName))
                {
                    fileCompilerToClose = OpenedFileCompilers[fileName];
                    OpenedFileCompilers.Remove(fileName);
                    fileCompilerToClose.StopContinuousBackgroundCompilation();
                }
            }            
        }
    }
}
