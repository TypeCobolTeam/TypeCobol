using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using Analytics;
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
        /// Common internal implementation for all 4 constructors above
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
                TextDocument = new ReadOnlyTextDocument(sourceFile.Name, sourceFile.Encoding, columnsLayout, sourceFile.ReadChars());
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

            if (TextDocument != null && TextDocument.LineCount == 0)
            {
                throw new Exception(string.Format("Source file {0} is empty", fileName));
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
            if (CompilerOptions.ExecToStep == null)
                CompilerOptions.ExecToStep = ExecutionStep.SemanticCheck;

            if (CompilationResultsForCopy != null)
            {
                CompilationResultsForCopy.UpdateTokensLines(); //Scanner

                if (!(CompilerOptions.ExecToStep > ExecutionStep.Scanner)) return;

                CompilationResultsForCopy.RefreshTokensDocumentSnapshot();
                CompilationResultsForCopy.RefreshProcessedTokensDocumentSnapshot(); //Preprocessor
            }
            else
            {
                AnalyticsWrapper.Telemetry.TrackEvent("[Phase] Scanner Step");
                CompilationResultsForProgram.UpdateTokensLines(); //Scanner

                if (!(CompilerOptions.ExecToStep > ExecutionStep.Scanner)) return;

                AnalyticsWrapper.Telemetry.TrackEvent("[Phase] Preprocessor Step");
                CompilationResultsForProgram.RefreshTokensDocumentSnapshot();
                CompilationResultsForProgram.RefreshProcessedTokensDocumentSnapshot(); //Preprocessor

                if (!(CompilerOptions.ExecToStep > ExecutionStep.Preprocessor)) return;
                if (CompilerOptions.HaltOnMissingCopy && CompilationResultsForProgram.MissingCopies.Count > 0) return; //If the Option is set to true and there is at least one missing copy, we don't have to run the semantic phase

                AnalyticsWrapper.Telemetry.TrackEvent("[Phase] Syntaxic Step");
                CompilationResultsForProgram.RefreshCodeElementsDocumentSnapshot(); //SyntaxCheck

                if (!(CompilerOptions.ExecToStep > ExecutionStep.SyntaxCheck)) return;

                AnalyticsWrapper.Telemetry.TrackEvent("[Phase] Semantic Step");
                CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot(); //SemanticCheck
            }


        }

        // Timers used for background execution of all compiler steps
        private readonly object timersSyncObject = new object();
        private Timer scannerTimer;
        private Timer preprocessorTimer;
        private Timer codeElementsParserTimer;
        private Timer programClassParserTimer;

        /// <summary>
        /// Start asynchronous continuous compilation in background threads
        /// </summary>
        public void StartContinuousBackgroundCompilation(int scannerPeriodMillisecond, int preprocessorPeriodMillisecond,
            int codeElementsParserPeriodMillisecond, int programClassParserPeriodMillisecond)
        {
            // Protect against concurrent updates
            lock (timersSyncObject)
            {
                // Already started, nothing to do
                if (scannerTimer != null)
                {
                    return;
                }

                // Check that periods for sucessive steps are consistent
                if (preprocessorPeriodMillisecond <= scannerPeriodMillisecond ||
                   codeElementsParserPeriodMillisecond <= preprocessorPeriodMillisecond ||
                   programClassParserPeriodMillisecond <= codeElementsParserPeriodMillisecond)
                {
                    throw new ArgumentException("Compiler step execution periods should be set with increasing values : any other setup would waste CPU resources");
                }

                // Initialize timers
                if (CompilationResultsForCopy != null)
                {
                    scannerTimer = new Timer(state => CompilationResultsForCopy.RefreshTokensDocumentSnapshot(), null, 0, scannerPeriodMillisecond);
                    preprocessorTimer = new Timer(state => CompilationResultsForCopy.RefreshProcessedTokensDocumentSnapshot(), null, preprocessorPeriodMillisecond, preprocessorPeriodMillisecond);
                }
                else
                {
                    scannerTimer = new Timer(state => CompilationResultsForProgram.RefreshTokensDocumentSnapshot(), null, 0, scannerPeriodMillisecond);
                    preprocessorTimer = new Timer(state => CompilationResultsForProgram.RefreshProcessedTokensDocumentSnapshot(), null, preprocessorPeriodMillisecond, preprocessorPeriodMillisecond);
                    codeElementsParserTimer = new Timer(state => CompilationResultsForProgram.RefreshCodeElementsDocumentSnapshot(), null, codeElementsParserPeriodMillisecond, codeElementsParserPeriodMillisecond);
                    programClassParserTimer = new Timer(state => CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot(), null, programClassParserPeriodMillisecond, programClassParserPeriodMillisecond);
                }
            }
        }

        /// <summary>
        /// Stop asynchronous continuous compilation in background threads : blocks until all callbacks are finished
        /// </summary>
        public void StopContinuousBackgroundCompilation()
        {
            // Protect against concurrent updates
            lock (timersSyncObject)
            {
                // Already stopped, nothing to do
                if (scannerTimer != null)
                {
                    return;
                }

                // Dispose timers and wait for all callbacks to finish
                EventWaitHandle[] waitHandles = null;
                if (CompilationResultsForCopy != null)
                {
                    waitHandles = new EventWaitHandle[2];
                    for (int i = 0; i < 2; i++)
                    {
                        waitHandles[i] = new EventWaitHandle(false, EventResetMode.AutoReset);
                    }
                    scannerTimer.Dispose(waitHandles[0]);
                    preprocessorTimer.Dispose(waitHandles[1]);
                }
                else
                {
                    waitHandles = new EventWaitHandle[4];
                    for (int i = 0; i < 4; i++)
                    {
                        waitHandles[i] = new EventWaitHandle(false, EventResetMode.AutoReset);
                    }
                    scannerTimer.Dispose(waitHandles[0]);
                    preprocessorTimer.Dispose(waitHandles[1]);
                    codeElementsParserTimer.Dispose(waitHandles[2]);
                    programClassParserTimer.Dispose(waitHandles[3]);
                }
                WaitHandle.WaitAll(waitHandles);
            }
        }

        /// <summary>
        /// Start listening to document change events
        /// </summary>
        public virtual void StartDocumentProcessing()
        {
            // Start compilation process
            TextDocument.StartSendingChangeEvents();
        }

        /// <summary>
        /// Monitor changes to the source Cobol file and automatically update TextDocument contents after each file update
        /// </summary>
        public virtual void StartContinuousFileProcessing()
        {
            // Reload text document each time an external change is applied to the cobol file
            ObserverTextDocument observerTextDocument = new ObserverTextDocument(CobolFile, TextDocument);
            CobolFile.CobolFileChanged += observerTextDocument.OnCobolFileChanged;

            // Start compilation process
            TextDocument.StartSendingChangeEvents();
            CobolFile.StartMonitoringExternalChanges();
        }

        /// <summary>
        /// Reload the entire file content in the text document each time the file is updated
        /// </summary>
        private class ObserverTextDocument
        {
            private CobolFile cobolFile;
            private ITextDocument textDocument;

            public ObserverTextDocument(CobolFile cobolFile, ITextDocument textDocument)
            {
                this.cobolFile = cobolFile;
                this.textDocument = textDocument;
            }

            public void OnCobolFileChanged(object sender, CobolFileChangedEvent fileEvent)
            {
                if (fileEvent.Type == CobolFileChangeType.FileChanged)
                {
                    textDocument.LoadChars(cobolFile.ReadChars());
                }
                else
                {
                    throw new InvalidOperationException("File change type " + fileEvent.Type + " is not supported in this configuration");
                }
            }
        }
    }
}
