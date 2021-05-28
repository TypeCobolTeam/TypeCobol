using System;
using System.Collections.Generic;
using System.Diagnostics;
using JetBrains.Annotations;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Batch compilation of one file on disk or continuous incremental compilation of text in an editor
    /// </summary>
    public class FileCompiler
    {
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
        public FileCompiler([NotNull] CompilationProject compilationProject, string fileName, bool isCopyFile)
            : this(null, fileName, compilationProject.ColumnsLayout, isCopyFile,
                compilationProject.SourceFileProvider, compilationProject, compilationProject.CompilationOptions, null,
                compilationProject)
        { }

        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public FileCompiler(string libraryName, string fileName, ColumnsLayout columnsLayout, bool isCopyFile,
            SourceFileProvider sourceFileProvider, IDocumentImporter documentImporter, TypeCobolOptions compilerOptions, SymbolTable customSymbols,
            CompilationProject compilationProject)
            : this(libraryName, fileName, columnsLayout, isCopyFile,
                sourceFileProvider, documentImporter, compilerOptions, customSymbols,
                null, compilationProject, null)
        { }

        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public FileCompiler(string libraryName, string fileName, ColumnsLayout columnsLayout, bool isCopyFile,
            SourceFileProvider sourceFileProvider, IDocumentImporter documentImporter, TypeCobolOptions compilerOptions, SymbolTable customSymbols,
            MultilineScanState scanState, CompilationProject compilationProject, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
            : this(new Tuple<string, string, ColumnsLayout, bool>(libraryName, fileName, columnsLayout, isCopyFile), null,
                sourceFileProvider, documentImporter, compilerOptions, customSymbols,
                scanState, compilationProject, copyTextNameVariations)
        { }

        /// <summary>
        /// Use a pre-existing text document, not yet associated with a Cobol file
        /// </summary>
        public FileCompiler(ITextDocument textDocument,
            SourceFileProvider sourceFileProvider, IDocumentImporter documentImporter, TypeCobolOptions compilerOptions,
            CompilationProject compilationProject)
            : this(textDocument,
                sourceFileProvider, documentImporter, compilerOptions, null,
                compilationProject)
        { }

        /// <summary>
        /// Use a pre-existing text document, not yet associated with a Cobol file + Existing SymbolTable
        /// </summary>
        public FileCompiler(ITextDocument textDocument,
            SourceFileProvider sourceFileProvider, IDocumentImporter documentImporter, TypeCobolOptions compilerOptions, SymbolTable customSymbols,
            CompilationProject compilationProject)
            : this(null, textDocument,
                sourceFileProvider, documentImporter, compilerOptions, customSymbols,
                null, compilationProject, null)
        { }

        /// <summary>
        /// Common internal implementation for all constructors above
        /// </summary>
        private FileCompiler(Tuple<string, string, ColumnsLayout, bool> fileInfo, ITextDocument textDocument,
            SourceFileProvider sourceFileProvider, IDocumentImporter documentImporter, TypeCobolOptions compilerOptions, SymbolTable customSymbols,
            [CanBeNull] MultilineScanState scanState, CompilationProject compilationProject, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {
            var chrono = new Stopwatch();
            chrono.Start();

            // 1.a Find the Cobol source file
            CobolFile sourceFile = null;
            CompilationProject = compilationProject;

            if (textDocument == null)
            {
                //No textDocument provided, use fileInfo to find the file
                Debug.Assert(fileInfo != null);
                string libraryName = fileInfo.Item1;
                string fileName = fileInfo.Item2;
                if (!sourceFileProvider.TryGetFile(libraryName, fileName, out sourceFile))
                {
                    var message = string.IsNullOrEmpty(libraryName)
                        ? $"Cobol source file not found: {fileName}"
                        : $"Cobol source file not found: {fileName} in {libraryName}";
                    throw new Exception(message);
                }
            }
            else
            {
                Debug.Assert(fileInfo == null);
            }

            chrono.Stop();
            SourceFileSearchTime = (int)chrono.ElapsedMilliseconds;
            chrono.Reset();

            chrono.Start();

            if (textDocument == null)
            {
                // 2.a Load it in a new text document in memory
                Debug.Assert(sourceFile != null);
                TextDocument = new ReadOnlyTextDocument(sourceFile.Name, sourceFile.Encoding, fileInfo.Item3, fileInfo.Item4, sourceFile.ReadChars());
            }
            else
            {
                // 2.b Use existing text document in memory, assuming that source is already loaded
                TextDocument = textDocument;
            }

            chrono.Stop();
            SourceFileLoadTime = (int)chrono.ElapsedMilliseconds;
            chrono.Reset();

            // 3. Prepare the data structures used by the different steps of the compiler
            if (TextDocument.Source.IsCopy)
            {
                if (scanState != null)
                {
                    //This is an imported copy
                    Debug.Assert(scanState.InsideCopy);
                    CompilationResultsForCopy = new CompilationDocument(TextDocument.Source, true, TextDocument.Lines, compilerOptions, documentImporter, scanState, copyTextNameVariations);
                }
                else
                {
                    //Direct copy parsing, copy is assumed to be part of Data Division and using comma as the decimal point.
                    var initialScanState = new MultilineScanState(TextDocument.Source.EncodingForAlphanumericLiterals, true, true, insideCopy: true);
                    CompilationResultsForProgram = new CompilationUnit(TextDocument.Source, false, TextDocument.Lines, compilerOptions, documentImporter, initialScanState, copyTextNameVariations, CompilationProject.AnalyzerProvider);
                    CompilationResultsForCopy = CompilationResultsForProgram;
                }

                CompilationResultsForCopy.CustomSymbols = customSymbols;
            }
            else
            {
                //This is a regular program
                Debug.Assert(scanState == null);
                var initialScanState = new MultilineScanState(TextDocument.Source.EncodingForAlphanumericLiterals);
                CompilationResultsForProgram = new CompilationUnit(TextDocument.Source, false, TextDocument.Lines, compilerOptions, documentImporter, initialScanState, copyTextNameVariations, CompilationProject.AnalyzerProvider);
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
        /// Perform a compilation based on an execution step.
        /// </summary>
        /// <param name="exec2Step">The execution step</param>
        /// <param name="haltOnMissingCopy">For preprocessing step, halt on missing copy options</param>
        /// <param name="useAntlrProgramParsing">Shall Antlr be used to parse the program</param>
        public void CompileOnce(ExecutionStep? exec2Step, bool haltOnMissingCopy, bool useAntlrProgramParsing)
        {
            if (exec2Step == null)
                exec2Step = ExecutionStep.CrossCheck;

            if (CompilationResultsForProgram != null)
            {
                CompilationResultsForProgram.UpdateTokensLines(); //Scanner
                CompilationResultsForProgram.RefreshTokensDocumentSnapshot();
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.Scanner });

                if (!(exec2Step > ExecutionStep.Scanner)) return;

                CompilationResultsForProgram.RefreshProcessedTokensDocumentSnapshot(); //Preprocessor
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.Preprocessor });

                if (!(exec2Step > ExecutionStep.Preprocessor)) return;
                if (haltOnMissingCopy && CompilationResultsForProgram.MissingCopies.Count > 0) return; //If the Option is set to true and there is at least one missing copy, we don't have to run the semantic phase

                CompilationResultsForProgram.RefreshCodeElementsDocumentSnapshot(); //SyntaxCheck
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.SyntaxCheck });

                if (!(exec2Step > ExecutionStep.SyntaxCheck)) return;

                CompilationResultsForProgram.ProduceTemporarySemanticDocument(); //SemanticCheck
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.SemanticCheck });

                if (!(exec2Step > ExecutionStep.SemanticCheck)) return;

                CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot(); //Cross Check step
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.CrossCheck });

                if (!(exec2Step > ExecutionStep.CrossCheck)) return;

                CompilationResultsForProgram.RefreshCodeAnalysisDocumentSnapshot(); //QualityCheck step
                ExecutionStepEventHandler?.Invoke(this, new ExecutionStepEventArgs() { ExecutionStep = ExecutionStep.QualityCheck });
            }
            else
            {
                Debug.Assert(CompilationResultsForCopy != null);
                CompilationResultsForCopy.UpdateTokensLines(); //Scanner

                if (!(exec2Step > ExecutionStep.Scanner)) return;

                CompilationResultsForCopy.RefreshTokensDocumentSnapshot();
                CompilationResultsForCopy.RefreshProcessedTokensDocumentSnapshot(); //Preprocessor
            }
        }
    }

    public class ExecutionStepEventArgs : EventArgs
    {
        public ExecutionStep ExecutionStep { get; set; }
    }
}
