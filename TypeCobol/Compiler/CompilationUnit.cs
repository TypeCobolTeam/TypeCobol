using System;
using System.Collections.Generic;
using System.Reactive.Subjects;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Complete compilation pipeline : from text to syntax tree and code model, used for programs or classes
    /// </summary>
    public class CompilationUnit : CompilationDocument
    {
        /// <summary>
        /// Initializes a new compilation document from a list of text lines.
        /// This method does not scan the inserted text lines to produce tokens.
        /// You must explicitely call UpdateTokensLines() to start an initial scan of the document.
        /// </summary>
        public CompilationUnit(TextSourceInfo textSourceInfo, IEnumerable<ITextLine> initialTextLines, TypeCobolOptions compilerOptions, IProcessedTokensDocumentProvider processedTokensDocumentProvider) :
            base(textSourceInfo, initialTextLines, compilerOptions, processedTokensDocumentProvider)
        { }

        /// <summary>
        /// Creates a new snapshot of the document viewed as CodeElement objects after parsing.
        /// (if the processed tokens lines changed since the last time this method was called)
        /// Thread-safe : this method can be called from any thread.
        /// </summary>
        public void RefreshCodeElementsDocumentSnapshot()
        {
            // Make sure two threads don't try to update this snapshot at the same time
            lock (lockObjectForCodeElementsDocumentSnapshot)
            {
                // Capture previous snapshots at one point in time
                ProcessedTokensDocument processedTokensDocument = ProcessedTokensDocumentSnapshot;
                CodeElementsDocument previousCodeElementsDocument = CodeElementsDocumentSnapshot;

                // Check if an update is necessary and compute changes to apply since last version
                bool scanAllDocumentLines = false;
                IList<DocumentChange<IProcessedTokensLine>> processedTokensLineChanges = null;
                if (previousCodeElementsDocument == null)
                {
                    scanAllDocumentLines = true;
                }
                else if (processedTokensDocument.CurrentVersion == previousCodeElementsDocument.PreviousStepSnapshot.CurrentVersion)
                {
                    // Processed tokens lines did not change since last update => nothing to do
                    return;
                }
                else
                {
                    DocumentVersion<IProcessedTokensLine> previousProcessedTokensDocumentVersion = previousCodeElementsDocument.PreviousStepSnapshot.CurrentVersion;
                    processedTokensLineChanges = previousProcessedTokensDocumentVersion.GetReducedAndOrderedChangesInNewerVersion(processedTokensDocument.CurrentVersion);
                }

                // Track all changes applied to the document while updating this snapshot
                DocumentChangedEvent<ICodeElementsLine> documentChangedEvent = null;

                // Apply text changes to the compilation document
                if (scanAllDocumentLines)
                {
                    CodeElementsParserStep.ParseDocument(TextSourceInfo, ((ImmutableList<CodeElementsLine>)previousCodeElementsDocument.Lines), CompilerOptions);
                }
                else
                {
                    ImmutableList<CodeElementsLine>.Builder codeElementsDocumentLines = ((ImmutableList<CodeElementsLine>)previousCodeElementsDocument.Lines).ToBuilder();
                    IList<DocumentChange<ICodeElementsLine>> documentChanges = CodeElementsParserStep.ParseProcessedTokensLinesChanges(TextSourceInfo, codeElementsDocumentLines, processedTokensLineChanges, PrepareDocumentLineForUpdate, CompilerOptions);

                    // Create a new version of the document to track these changes
                    DocumentVersion<ICodeElementsLine> currentCodeElementsLinesVersion = previousCodeElementsDocument.CurrentVersion;
                    currentCodeElementsLinesVersion.changes = documentChanges;
                    currentCodeElementsLinesVersion.next = new DocumentVersion<ICodeElementsLine>(currentCodeElementsLinesVersion);

                    // Prepare an event to signal document change to all listeners
                    documentChangedEvent = new DocumentChangedEvent<ICodeElementsLine>(currentCodeElementsLinesVersion, currentCodeElementsLinesVersion.next);
                    currentCodeElementsLinesVersion = currentCodeElementsLinesVersion.next;

                    // Update the code elements document snapshot
                    CodeElementsDocumentSnapshot = new CodeElementsDocument(processedTokensDocument, currentCodeElementsLinesVersion, codeElementsDocumentLines);
                }

                // Send events to all listeners
                codeElementsLinesChangedEventsSource.OnNext(documentChangedEvent);
            }
        }

        /// <summary>
        /// Last snapshot of the compilation unit viewed as a set of code elements, after parsing the processed tokens.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary>                        
        public CodeElementsDocument CodeElementsDocumentSnapshot { get; private set; }

        // Broadcast document code elements lines changes to all listeners
        private ISubject<DocumentChangedEvent<ICodeElementsLine>> codeElementsLinesChangedEventsSource = new Subject<DocumentChangedEvent<ICodeElementsLine>>();

        /// <summary>
        /// Subscribe to this events source to be notified of all changes in the processed tokens lines of the document
        /// </summary>
        public IObservable<DocumentChangedEvent<ICodeElementsLine>> CodeElementsLinesChangedEventsSource
        {
            get { return codeElementsLinesChangedEventsSource; }
        }

        /// <summary>
        /// Creates a new snapshot of the document viewed as complete Cobol Program or Class.
        /// (if the code elements lines changed since the last time this method was called)
        /// Thread-safe : this method can be called from any thread.
        /// </summary>
        public void RefreshProgramOrClassSnapshot()
        {
            // Make sure two threads don't try to update this snapshot at the same time
            lock (lockObjectForProgramOrClass)
            {
                // Capture previous snapshots at one point in time
                CodeElementsDocument codeElementsDocument = CodeElementsDocumentSnapshot;

                // Check if an update is necessary and compute changes to apply since last version
                if (codeElementsLinesVersionForCurrentProgramOrClass == null || codeElementsLinesVersionForCurrentProgramOrClass != codeElementsDocument.CurrentVersion)
                {
                    codeElementsLinesVersionForCurrentProgramOrClass = codeElementsDocument.CurrentVersion;

                    // Program and Class parsing is not incremental : the objects are rebuilt each time this method is called
                    Program newProgram;
                    Class newClass;
                    IList<ParserDiagnostic> newDiagnostics;
                    ProgramClassParserStep.ParseProgramOrClass(TextSourceInfo, ((ImmutableList<CodeElementsLine>)codeElementsDocument.Lines), CompilerOptions, out newProgram, out newClass, out newDiagnostics);
                    Program = newProgram;
                    Class = newClass;
                    Diagnostics = newDiagnostics;
                }
            }
        }

        // Update Program or Class only when code elements changed
        private DocumentVersion<ICodeElementsLine> codeElementsLinesVersionForCurrentProgramOrClass;

        /// <summary>
        /// Last snapshot of the compilation unit viewed as a complete Cobol program, after parsing the code elements.
        /// Only one of the two properties Program or Class can be not null.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary> 
        public Program Program { get; private set; }

        /// <summary>
        /// Last snapshot of the compilation unit viewed as a complete Cobol class, after parsing the code elements.
        /// Only one of the two properties Program or Class can be not null.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary> 
        public Class Class { get; private set; }

        /// <summary>
        /// Errors found while parsing Program or Class
        /// </summary>
        public IList<ParserDiagnostic> Diagnostics { get; private set; }
        
        #region Thread ownership and synchronization

        // Synchronize accesses during snapshots updates
        protected readonly object lockObjectForCodeElementsDocumentSnapshot = new object();
        protected readonly object lockObjectForProgramOrClass = new object();

        #endregion
    }

    /*
        /// <summary>
        /// Syntax tree produced by the parsing phase
        /// </summary>
        public SyntaxDocument SyntaxDocument     { get; private set; }

        /// <summary>
        /// Object oriented model of the Cobol program
        /// </summary>
        public SemanticsDocument SemanticsDocument      { get; private set; }

        /// <summary>     
        /// List of all external programs called from the current program 
        /// </summary>
        public IList<CompilationUnit> CallReferences { get; private set; }

        /// <summary>
        /// TextDocument where the target Cobol code will be generated
        /// </summary>
        public ITextDocument GeneratedTextDocument { get; private set; }

        /// <summary>
        /// True if the current generated text in GeneratedTextDocument is different from the GeneratedCobolFile content 
        /// </summary>
        public bool HasGeneratedTextChangesToSave { get; private set; }

        /// <summary>
        /// CobolFile used to save the results of the Cobol code generation
        /// </summary>
        public CobolFile GeneratedCobolFile { get; private set; }

        /// <summary>
        /// Compiler options 
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public CompilationUnit(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ColumnsLayout columnsLayout, TypeCobolOptions compilerOptions) :
            base(libraryName, textName, sourceFileProvider, documentProvider, columnsLayout, compilerOptions)
        {
            CompilerOptions = compilerOptions;

            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Load a Cobol source file in an existing text document
        /// </summary>
        public CompilationUnit(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ITextDocument textDocument, TypeCobolOptions compilerOptions) :
            base(libraryName, textName, sourceFileProvider, documentProvider, textDocument, compilerOptions)
        {
            CompilerOptions = compilerOptions;

            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Initialize the compilation unit from an existing text document, not yet associated with a Cobol file
        /// </summary>
        public CompilationUnit(ITextDocument textDocument, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions) :
            base(textDocument, sourceFileProvider, documentProvider, compilerOptions)
        {
            CompilerOptions = compilerOptions;

            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Initialize the compilation unit from an existing text document, already initialized from a Cobol file
        /// </summary>
        public CompilationUnit(ITextDocument textDocument, CobolFile cobolFile, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions) :
            base(textDocument, cobolFile, sourceFileProvider, documentProvider, compilerOptions)
        {
            // 5. Parse the processed tokens
            SyntaxDocument = new SyntaxDocument(ProcessedTokensDocument, compilerOptions);

            // 6. Build a code model from the parse tree, after symbol resolution
            SemanticsDocument = new SemanticsDocument(SyntaxDocument, compilerOptions);
        }

        /// <summary>
        /// Configure the compilation unit to only parse the source and build a code model, without exécuting the code generation phase
        /// </summary>
        public void SetupCodeAnalysisPipeline(IScheduler backgroundParsingScheduler, int bufferingDelayMillisecond)
        {
            base.SetupDocumentProcessingPipeline(backgroundParsingScheduler, bufferingDelayMillisecond); 

            // Parser always operates on the same thread as Preprocessor
            ProcessedTokensDocument.TokensChangedEventsSource.Subscribe(SyntaxDocument);

            // TypeChecker alway operates on the same thread as Parser
            SyntaxDocument.ParseNodeChangedEventsSource.Subscribe(SemanticsDocument);
        }

        /// <summary>
        /// Configure the compilation unit to parse the source, build a code model, and then generate Cobol code
        /// </summary>
        public void SetupTextGenerationPipeline(IScheduler backgroundParsingScheduler, int bufferingDelayMillisecond, IScheduler textGenerationScheduler)
        {
            SetupCodeAnalysisPipeline(backgroundParsingScheduler, bufferingDelayMillisecond);

            // TypeCobolGenerator always operates on the same thread as TypeChecker
            TypeCobolGenerator generator = new TypeCobolGenerator(SemanticsDocument, GeneratedTextDocument);

            // Mono thread configuration : Generated code is written to the TextDocument in the same thread
            if (textGenerationScheduler == null)
            {
                SemanticsDocument.CodeModelChangedEventsSource.Subscribe(generator);
            }
            // Multi-thread configuration : generator could need to update the TextDocument on the UI thread
            else
            {
                SemanticsDocument.CodeModelChangedEventsSource.
                    ObserveOn(textGenerationScheduler).Subscribe(generator);
            }
        } 
    }*/
}
