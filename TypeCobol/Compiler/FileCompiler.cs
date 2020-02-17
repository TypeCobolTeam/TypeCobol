using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Tools;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Batch compilation of one file on disk or continuous incremental compilation of text in an editor
    /// </summary>
    public class FileCompiler
    {
        /// <summary>
        /// Source text file on disk
        /// </summary>
        public CobolFile CobolFile { get; private set; }

        /// <summary>
        /// Source text buffer in memory
        /// </summary>
        public ITextDocument TextDocument { get; private set; }

        /// <summary>
        /// True if the source text buffer in memory (TextDocument) is different from the source text file (CobolFile) on disk 
        /// </summary>
        public bool HasTextChangesToSave { get; private set; }

        /// <summary>
        /// Compiler options directing the scanner, preprocessor, parser and type checker operations
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>
        /// Document representing the results of the compilation in case of copy file
        /// </summary>
        public CompilationDocument CompilationResultsForCopy { get; private set; }

        /// <summary>
        /// Document representing the results of the compilation in case of program or class file
        /// </summary>
        public CompilationUnit CompilationResultsForProgram { get; private set; }

        public CompilationProject CompilationProject { get; set; }

        /// <summary>
        /// Output text buffer in memory where the target Cobol code will be generated
        /// </summary>
        public ITextDocument GeneratedTextDocument { get; private set; }

        /// <summary>
        /// True if the generated text in memory (GeneratedTextDocument) is different from the output file (GeneratedCobolFile) on disk
        /// </summary>
        public bool HasGeneratedTextChangesToSave { get; private set; }

        /// <summary>
        /// Output text file used to save the results of the Cobol code generation
        /// </summary>
        public CobolFile GeneratedCobolFile { get; private set; }

        /// <summary>
        /// Number of milliseconds to locate the source file in the source library
        /// </summary>
        public int SourceFileSearchTime { get; private set; }

        /// <summary>
        /// Number of milliseconds to load the source file in memory
        /// </summary>
        public int SourceFileLoadTime { get; private set; }

        public EventHandler<ExecutionStepEventArgs> ExecutionStepEventHandler { get; set; }

        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public FileCompiler([NotNull] CompilationProject compilationProject, string fileName, bool isCopyFile) :
            this(null, fileName, null, compilationProject.SourceFileProvider, compilationProject, compilationProject.ColumnsLayout, null, compilationProject.CompilationOptions, null, isCopyFile, null, compilationProject, null)
        { }
        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public FileCompiler(string libraryName, string fileName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ColumnsLayout columnsLayout, TypeCobolOptions compilerOptions, CodeModel.SymbolTable customSymbols, bool isCopyFile, CompilationProject compilationProject) :
            this(libraryName, fileName, null, sourceFileProvider, documentProvider, columnsLayout, null, compilerOptions, customSymbols, isCopyFile, null, compilationProject, null)
        { }

        /// <summary>
        /// Load a Cobol source file in an pre-existing text document
        /// </summary>
        public FileCompiler(string libraryName, string fileName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ITextDocument textDocument, TypeCobolOptions compilerOptions, bool isCopyFile, CompilationProject compilationProject) :
            this(libraryName, fileName, null, sourceFileProvider, documentProvider, default(ColumnsLayout), textDocument, compilerOptions, null, isCopyFile, null, compilationProject, null)
        { }

        /// <summary>
        /// Use a pre-existing text document, not yet associated with a Cobol file
        /// </summary>
        public FileCompiler(ITextDocument textDocument, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions, bool isCopyFile, CompilationProject compilationProject) :
            this(null, null, null, sourceFileProvider, documentProvider, default(ColumnsLayout), textDocument, compilerOptions, null, isCopyFile, null, compilationProject, null)
        { }

        /// <summary>
        /// Use a pre-existing text document, not yet associated with a Cobol file + Existing SymbolTable
        /// </summary>
        public FileCompiler(ITextDocument textDocument, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions, SymbolTable customSymbols, bool isCopyFile, CompilationProject compilationProject) :
            this(null, null, null, sourceFileProvider, documentProvider, default(ColumnsLayout), textDocument, compilerOptions, customSymbols, isCopyFile, null, compilationProject, null)
        { }

        /// <summary>
        /// Use a pre-existing text document, already initialized from a Cobol file
        /// </summary>
        public FileCompiler(string libraryName, string fileName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ColumnsLayout columnsLayout, TypeCobolOptions compilerOptions, CodeModel.SymbolTable customSymbols, bool isCopyFile, MultilineScanState scanState, CompilationProject compilationProject, List<RemarksDirective.TextNameVariation> copyTextNameVariations) :
            this(libraryName, fileName, null, sourceFileProvider, documentProvider, columnsLayout, null, compilerOptions, customSymbols, isCopyFile, scanState, compilationProject, copyTextNameVariations)
        { }

        /// <summary>
        /// Common internal implementation for all constructors above
        /// </summary>
        private FileCompiler(string libraryName, string fileName, CobolFile loadedCobolFile, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ColumnsLayout columnsLayout, ITextDocument textDocument, TypeCobolOptions compilerOptions, SymbolTable customSymbols, bool isCopyFile,
            [CanBeNull] MultilineScanState scanState, CompilationProject compilationProject, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {

            var chrono = new Stopwatch();
            chrono.Start();

            // 1.a Find the Cobol source file
            CobolFile sourceFile = null;
            CompilationProject = compilationProject;

            if (fileName != null)
            {
                if (sourceFileProvider.TryGetFile(libraryName, fileName, out sourceFile))
                {
                    CobolFile = sourceFile;
                }
                else
                {
                    var message = string.IsNullOrEmpty(libraryName) ? string.Format("Cobol source file not found: {0}", fileName)
                                                                    : string.Format("Cobol source file not found: {0} in {1}", fileName, libraryName);
                    throw new Exception(message);
                }

            }
            // 1.b Register a Cobol source file which was already loaded
            else if (loadedCobolFile != null)
            {
                CobolFile = loadedCobolFile;
            }

            chrono.Stop();
            SourceFileSearchTime = (int)chrono.ElapsedMilliseconds;
            chrono.Reset();

            // 2.a Load it in a new text document in memory
            chrono.Start();
            if (textDocument == null)
            {
                TextDocument = new ReadOnlyTextDocument(sourceFile?.Name, sourceFile?.Encoding, columnsLayout, sourceFile?.ReadChars());
            }
            // 2.b Load it in an existing text document in memory
            else if (sourceFile != null)
            {
                TextDocument = textDocument;
                textDocument.LoadChars(sourceFile.ReadChars());
            }
            // 2.c Use a pre-existing text document 
            //     - not yet associated with a Cobol source file
            //     - with a Cobol source file already loaded
            else if (sourceFile == null || loadedCobolFile != null)
            {
                TextDocument = textDocument;
            }

            chrono.Stop();
            SourceFileLoadTime = (int)chrono.ElapsedMilliseconds;
            chrono.Reset();

            // 3. Prepare the data structures used by the different steps of the compiler
            if (isCopyFile)
            {
                CompilationResultsForCopy = new CompilationDocument(TextDocument.Source, TextDocument.Lines, compilerOptions, documentProvider, scanState, copyTextNameVariations);
                CompilationResultsForCopy.CustomSymbols = customSymbols;
            }
            else
            {
                CompilationResultsForProgram = new CompilationUnit(TextDocument.Source, TextDocument.Lines, compilerOptions, documentProvider, copyTextNameVariations);
                CompilationResultsForProgram.CustomSymbols = customSymbols;
            }
            CompilerOptions = compilerOptions;
        }

        /// <summary>
        /// Synchronous one-time compilation of the current file
        /// </summary>
        public void CompileOnce()
        {
            CompileOnce(CompilerOptions.ExecToStep, CompilerOptions.HaltOnMissingCopy, CompilerOptions.UseAntlrProgramParsing);
        }

        /// <summary>
        /// Perform a cmpilation based on an execution step.
        /// </summary>
        /// <param name="exec2Step">The execution step</param>
        /// <param name="haltOnMissingCopy">For preprocessing step, halt on missing copy options</param>
        /// <param name="useAntlrProgramParsing">Shall Antlr be used to parse the program</param>
        public void CompileOnce(ExecutionStep? exec2Step, bool haltOnMissingCopy, bool useAntlrProgramParsing)
        {
            if (exec2Step == null)
                exec2Step = ExecutionStep.CrossCheck;

            if (CompilationResultsForCopy != null)
            {
                CompilationResultsForCopy.UpdateTokensLines(); //Scanner

                if (!(exec2Step > ExecutionStep.Scanner)) return;

                CompilationResultsForCopy.RefreshTokensDocumentSnapshot();
                CompilationResultsForCopy.RefreshProcessedTokensDocumentSnapshot(); //Preprocessor
            }
            else
            {
                CompilationResultsForProgram.UpdateTokensLines(); //Scanner
                CompilationResultsForProgram.RefreshTokensDocumentSnapshot();
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() {ExecutionStep = ExecutionStep.Scanner});

                if (!(exec2Step > ExecutionStep.Scanner)) return;

                CompilationResultsForProgram.RefreshProcessedTokensDocumentSnapshot(); //Preprocessor
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.Preprocessor});

                if (!(exec2Step > ExecutionStep.Preprocessor)) return;
                if (haltOnMissingCopy && CompilationResultsForProgram.MissingCopies.Count > 0) return; //If the Option is set to true and there is at least one missing copy, we don't have to run the semantic phase

                CompilationResultsForProgram.RefreshCodeElementsDocumentSnapshot(); //SyntaxCheck
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.SyntaxCheck});

                if (!(exec2Step > ExecutionStep.SyntaxCheck)) return;

                CompilationResultsForProgram.ProduceTemporarySemanticDocument(); //SemanticCheck
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.SemanticCheck });

                if (!(exec2Step > ExecutionStep.SemanticCheck)) return;

                CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot(); //Cross Check step
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.CrossCheck });
            }
        }
    }

    public class ExecutionStepEventArgs : EventArgs
    {
        public ExecutionStep ExecutionStep { get; set; }
    }
}
