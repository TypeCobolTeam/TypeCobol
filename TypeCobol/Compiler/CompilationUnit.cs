﻿using System.Text;
using TypeCobol.Analysis;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Complete compilation pipeline : from text to syntax tree and code model, used for programs or classes
    /// </summary>
    public class CompilationUnit : CompilationDocument
    {
        private readonly AnalyzerProviderWrapper _analyzerProvider;

        /// <summary>
        /// Initializes a new compilation document from a list of text lines.
        /// This method does not scan the inserted text lines to produce tokens.
        /// You must explicitly call UpdateTokensLines() to start an initial scan of the document.
        /// </summary>
        public CompilationUnit(TextSourceInfo textSourceInfo, bool isImported, IEnumerable<ITextLine> initialTextLines, TypeCobolOptions compilerOptions, IDocumentImporter documentImporter, MultilineScanState initialScanState, IAnalyzerProvider analyzerProvider) :
            base(textSourceInfo, isImported, initialTextLines, compilerOptions, documentImporter, initialScanState)
        {
            // Initialize performance stats 
            PerfStatsForCodeElementsParser = new PerfStatsForParsingStep(CompilationStep.CodeElementsParser);
            PerfStatsForTemporarySemantic = new PerfStatsForParsingStep(CompilationStep.ProgramClassParser);
            PerfStatsForProgramCrossCheck = new PerfStatsForParsingStep(CompilationStep.ProgramCrossCheck);
            PerfStatsForCodeQualityCheck = new PerfStatsForCompilationStep(CompilationStep.CodeQualityCheck);
            if (analyzerProvider is AnalyzerProviderWrapper analyzerProviderWrapper)
            {
                _analyzerProvider = analyzerProviderWrapper;
            }
            else if (analyzerProvider != null)
            {
                // Wrap the given provider to secure it
                _analyzerProvider = new AnalyzerProviderWrapper();
                _analyzerProvider.AddProvider(analyzerProvider);
            }
            else
            {
                _analyzerProvider = null;
            }

            _history = this.TrackChanges(15);
        }

        /// <summary>
        /// Creates a new snapshot of the document viewed as CodeElement objects after parsing.
        /// (if the processed tokens lines changed since the last time this method was called)
        /// Thread-safe : this method can be called from any thread.
        /// </summary>
        public void RefreshCodeElementsDocumentSnapshot()
        {
            // Track all changes applied to the document while updating this snapshot
            DocumentChangedEvent<ICodeElementsLine> documentChangedEvent = null;

            // Make sure two threads don't try to update this snapshot at the same time
            lock (lockObjectForCodeElementsDocumentSnapshot)
            {
                // Start perf measurement
                var perfStatsForParserInvocation = PerfStatsForCodeElementsParser.OnStartRefreshParsingStep();

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
                    PerfStatsForCodeElementsParser.OnStopRefreshParsingStep();
                    return;
                }
                else
                {
                    DocumentVersion<IProcessedTokensLine> previousProcessedTokensDocumentVersion = previousCodeElementsDocument.PreviousStepSnapshot.CurrentVersion;
                    processedTokensLineChanges = previousProcessedTokensDocumentVersion.GetReducedAndOrderedChangesInNewerVersion(processedTokensDocument.CurrentVersion);
                }

                // Apply text changes to the compilation document
                if (scanAllDocumentLines)
                {
                    if (processedTokensDocument != null)
                    {
                        // Parse the whole document for the first time
                        CodeElementsParserStep.ParseDocument(processedTokensDocument, CompilerOptions, perfStatsForParserInvocation);

                        // Create the first code elements document snapshot
                        CodeElementsDocumentSnapshot = new CodeElementsDocument(processedTokensDocument, new DocumentVersion<ICodeElementsLine>(this), (ISearchableReadOnlyList<ICodeElementsLine>)processedTokensDocument.Lines);
                    }
                }
                else
                {
                    IList<DocumentChange<ICodeElementsLine>> documentChanges = CodeElementsParserStep.ParseProcessedTokensLinesChanges(processedTokensDocument, processedTokensLineChanges, PrepareDocumentLineForUpdate, CompilerOptions, perfStatsForParserInvocation);

                    // Create a new version of the document to track these changes
                    DocumentVersion<ICodeElementsLine> currentCodeElementsLinesVersion = previousCodeElementsDocument.CurrentVersion;
                    currentCodeElementsLinesVersion.changes = documentChanges;
                    currentCodeElementsLinesVersion.next = new DocumentVersion<ICodeElementsLine>(currentCodeElementsLinesVersion);

                    // Prepare an event to signal document change to all listeners
                    documentChangedEvent = new DocumentChangedEvent<ICodeElementsLine>(currentCodeElementsLinesVersion, currentCodeElementsLinesVersion.next);
                    currentCodeElementsLinesVersion = currentCodeElementsLinesVersion.next;

                    // Update the code elements document snapshot
                    CodeElementsDocumentSnapshot = new CodeElementsDocument(processedTokensDocument, currentCodeElementsLinesVersion, (ISearchableReadOnlyList<ICodeElementsLine>)processedTokensDocument.Lines);
                }

                // Stop perf measurement
                PerfStatsForCodeElementsParser.OnStopRefreshParsingStep();
            }

            // Send events to all listeners
            EventHandler<DocumentChangedEvent<ICodeElementsLine>> codeElementsLinesChanged = CodeElementsLinesChanged; // avoid race condition
            if (documentChangedEvent != null && codeElementsLinesChanged != null)
            {
                codeElementsLinesChanged(this, documentChangedEvent);
            }

#if DEBUG
            //Update CE diag count for future checks
            _codeElementDiagnosticCount = AllDiagnostics(true).Count;
#endif
        }

        /// <summary>
        /// Last snapshot of the compilation unit viewed as a set of code elements, after parsing the processed tokens.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary>                        
        public CodeElementsDocument CodeElementsDocumentSnapshot { get; private set; }

        /// <summary>
        /// Subscribe to this event to be notified of all changes in the code elements lines of the document
        /// </summary>
        public event EventHandler<DocumentChangedEvent<ICodeElementsLine>> CodeElementsLinesChanged;

        /// <summary>
        /// Performance stats for the RefreshCodeElementsDocumentSnapshot method
        /// </summary>
        public PerfStatsForParsingStep PerfStatsForCodeElementsParser { get; private set; }

#if DEBUG
        private int _codeElementDiagnosticCount;

        /// <summary>
        /// Debug-only check to avoid creation of diagnostics on CodeElements after they have been created/refreshed.
        /// Must be called at the end of each compilation step defined after <see cref="RefreshCodeElementsDocumentSnapshot"/>.
        /// </summary>
        private void CheckCodeElementDiagnostics()
        {
            var actualCodeElementDiagnosticCount = AllDiagnostics(true).Count;
            if (actualCodeElementDiagnosticCount != _codeElementDiagnosticCount)
            {
                System.Diagnostics.Debug.Fail("CodeElement diagnostics should not be created after CE phase !");
            }
        }
#endif

        public string AntlrResult {
            get
            {
                StringBuilder builder = new StringBuilder();

                if (CodeElementsParserStep.AntlrPerformanceProfiler != null && PerfStatsForCodeElementsParser.ActivateDetailedAntlrPofiling)
                {
                    builder.Append("---CODE ELEMENT PARSER STEP---\n");
                    builder.Append(CodeElementsParserStep.AntlrPerformanceProfiler.WriteInfoToString());
                }

                return builder.ToString();
            }
        }

        /// <summary>
        /// Creates a new snapshot of the document viewed as complete Cobol Program or Class.
        /// Thread-safe : this method can be called from any thread.
        /// </summary>
        public void RefreshProgramClassDocumentSnapshot()
        {
            // Make sure two threads don't try to update this snapshot at the same time
            bool snapshotWasUpdated = false;
            lock (lockObjectForProgramClassDocumentSnapshot)
            {
                PerfStatsForProgramCrossCheck.OnStartRefreshParsingStep();

                // Capture previous snapshot at one point in time
                TemporarySemanticDocument temporarySnapshot = TemporaryProgramClassDocumentSnapshot;

                // Check if an update is necessary and compute changes to apply since last version
                if ((TemporaryProgramClassDocumentSnapshot != null) && (ProgramClassDocumentSnapshot == null || ProgramClassDocumentSnapshot.PreviousStepSnapshot.CurrentVersion != temporarySnapshot.CurrentVersion))
                {
                    // Program and Class parsing is not incremental : the objects are rebuilt each time this method is called
                    SourceFile root = temporarySnapshot.Root;
                    Dictionary<CodeElement, Node> nodeCodeElementLinkers = temporarySnapshot.NodeCodeElementLinkers ?? new Dictionary<CodeElement, Node>();
                    ProgramClassParserStep.CrossCheckPrograms(root, temporarySnapshot, this.CompilerOptions);
              
                    // Capture the result of the parse in a new snapshot
                    ProgramClassDocumentSnapshot = new ProgramClassDocument(
                        temporarySnapshot, ProgramClassDocumentSnapshot?.CurrentVersion + 1 ?? 0,
                        root, nodeCodeElementLinkers);
                    snapshotWasUpdated = true;
                }

                PerfStatsForProgramCrossCheck.OnStopRefreshParsingStep();
            }

            // Send events to all listeners
            EventHandler<ProgramClassEvent> programClassChanged = ProgramClassChanged; // avoid race condition
            EventHandler<ProgramClassEvent> programClassNotChanged = ProgramClassNotChanged;
            if (snapshotWasUpdated && programClassChanged != null)
            {
                programClassChanged(this, new ProgramClassEvent() { Version= ProgramClassDocumentSnapshot.CurrentVersion});
            }
            else if (!snapshotWasUpdated && programClassNotChanged != null)
            {
                programClassNotChanged(this, new ProgramClassEvent() { Version = ProgramClassDocumentSnapshot.CurrentVersion });
            }

#if DEBUG
            CheckCodeElementDiagnostics();
#endif
        }

        private readonly IncrementalChangesHistory _history;

        /// <summary>
        /// Creates a temporary snapshot which contains element before the cross check phase
        /// Usefull to create a program symboltable without checking nodes.
        /// For instance : it's used to load all the symbols from every dependencies before running the cross check phase to resolve symbols.
        /// </summary>
        public void ProduceTemporarySemanticDocument()
        {
            lock (lockObjectForTemporarySemanticDocument)
            {
                // Start perf measurement
                var perfStatsForParserInvocation = PerfStatsForTemporarySemantic.OnStartRefreshParsingStep();

                // Capture previous snapshot at one point in time
                CodeElementsDocument codeElementsDocument = CodeElementsDocumentSnapshot;

                if (CodeElementsDocumentSnapshot != null && (TemporaryProgramClassDocumentSnapshot == null || TemporaryProgramClassDocumentSnapshot.PreviousStepSnapshot.CurrentVersion != CodeElementsDocumentSnapshot.CurrentVersion))
                {
                    var customAnalyzers = _analyzerProvider?.CreateSyntaxDrivenAnalyzers(CompilerOptions, TextSourceInfo);

                    // Program and Class parsing is not incremental : the objects are rebuilt each time this method is called
                    ProgramClassParserStep.CupParseProgramOrClass(
                        TextSourceInfo,
                        (ISearchableReadOnlyList<CodeElementsLine>) codeElementsDocument.Lines,
                        CompilerOptions,
                        CustomSymbols,
                        perfStatsForParserInvocation,
                        customAnalyzers,
                        _history,
                        out var root,
                        out var newDiagnostics,
                        out var nodeCodeElementLinkers,
                        out var typedVariablesOutsideTypedef,
                        out var typeThatNeedTypeLinking);

                    //Capture the syntax-driven analyzers results
                    var results = new Dictionary<string, object>();
                    if (customAnalyzers != null)
                    {
                        foreach (var analyzer in customAnalyzers)
                        {
                            try
                            {
                                results.Add(analyzer.Identifier, analyzer.GetResult());
                            }
                            catch (Exception exception)
                            {
                                var diagnostic = new Diagnostic(MessageCode.AnalyzerFailure, Diagnostic.Position.Default, analyzer.Identifier, exception.Message, exception);
                                newDiagnostics.Add(diagnostic);
                            }
                        }
                    }

                    // Capture the produced results
                    TemporaryProgramClassDocumentSnapshot = new TemporarySemanticDocument(codeElementsDocument, new DocumentVersion<ICodeElementsLine>(this), codeElementsDocument.Lines,  root, newDiagnostics, nodeCodeElementLinkers,
                        typedVariablesOutsideTypedef, typeThatNeedTypeLinking, results);

                    //Direct copy parsing : remove redundant root 01 level if any.
                    if (UseDirectCopyParsing)
                    {
                        var workingStorageSection = root.MainProgram
                            .Children[0]  //Data Division
                            .Children[0]; //Working-Storage Section
                        if (workingStorageSection.ChildrenCount > 0)
                        {
                            var firstDataDefinition = workingStorageSection.Children[0];
                            if (firstDataDefinition.ChildrenCount == 0)
                            {
                                firstDataDefinition.Remove();
                            }
                        }
                        else
                        {
                            //Copy contains instructions that could not be parsed as a DataDefinition
                            var diagnostic = new Diagnostic(MessageCode.Warning, Diagnostic.Position.Default, "This parser does not support COPYs containing COBOL statements.");
                            newDiagnostics.Add(diagnostic);
                        }
                    }
                }

                // Stop perf measurement
                PerfStatsForTemporarySemantic.OnStopRefreshParsingStep();
            }

#if DEBUG
            CheckCodeElementDiagnostics();
#endif
        }

        /// <summary>
        /// Launch AST analyzers to perform code quality analysis.
        /// Update the list of quality-check diagnostics.
        /// </summary>
        public void RefreshCodeAnalysisDocumentSnapshot()
        {
            bool documentUpdated = false;
            lock (lockObjectForCodeAnalysisDocumentSnapshot)
            {
                PerfStatsForCodeQualityCheck.OnStartRefresh();

                var programClassDocument = ProgramClassDocumentSnapshot;
                if (programClassDocument != null && CodeAnalysisDocumentNeedsUpdate(out int version))
                {
                    List<Diagnostic> diagnostics = new List<Diagnostic>();
                    Dictionary<string, object> results = new Dictionary<string, object>();
                    var analyzers = _analyzerProvider?.CreateQualityAnalyzers(CompilerOptions);
                    if (analyzers != null)
                    {
                        //Results from previous steps
                        var temporarySemanticDocument = programClassDocument.PreviousStepSnapshot;
                        var codeElementsDocument = (CodeElementsDocument) temporarySemanticDocument.PreviousStepSnapshot;
                        var processedTokensDocument = (ProcessedTokensDocument) codeElementsDocument.PreviousStepSnapshot;
                        var tokensDocument = (TokensDocument) processedTokensDocument.PreviousStepSnapshot;

                        //Launch code analysis, gather quality rules violations and analyzer additional results
                        foreach (var analyzer in analyzers)
                        {
                            try
                            {
                                analyzer.Inspect(tokensDocument);
                                analyzer.Inspect(processedTokensDocument);
                                analyzer.Inspect(codeElementsDocument);
                                analyzer.Inspect(temporarySemanticDocument);
                                analyzer.Inspect(programClassDocument);
                                diagnostics.AddRange(analyzer.Diagnostics);
                                results[analyzer.Identifier] = analyzer.GetResult();
                            }
                            catch (Exception exception)
                            {
                                var diagnostic = new Diagnostic(MessageCode.AnalyzerFailure, Diagnostic.Position.Default, analyzer.Identifier, exception.Message, exception);
                                diagnostics.Add(diagnostic);
                            }
                        }
                    }

                    //Create updated snapshot
                    CodeAnalysisDocumentSnapshot = new InspectedProgramClassDocument(programClassDocument, version, diagnostics, results);
                    documentUpdated = true;
                }

                PerfStatsForCodeQualityCheck.OnStopRefresh();

                bool CodeAnalysisDocumentNeedsUpdate(out int newVersion)
                {
                    if (CodeAnalysisDocumentSnapshot == null)
                    {
                        //Not yet computed
                        newVersion = 0;
                        return true;
                    }

                    if (CodeAnalysisDocumentSnapshot.PreviousStepSnapshot.CurrentVersion != programClassDocument.CurrentVersion)
                    {
                        //Obsolete version
                        newVersion = CodeAnalysisDocumentSnapshot.CurrentVersion + 1;
                        return true;
                    }

                    //No need for update
                    newVersion = -1;
                    return false;
                }
            }

            EventHandler<ProgramClassEvent> codeAnalysisCompleted = CodeAnalysisCompleted;
            if (documentUpdated && codeAnalysisCompleted != null)
            {
                codeAnalysisCompleted(this, new ProgramClassEvent() { Version = CodeAnalysisDocumentSnapshot.CurrentVersion });
            }

#if DEBUG
            CheckCodeElementDiagnostics();
#endif
        }

        /// <summary>
        /// Snapshot of the compilation unit viewed as a complete Cobol program or class, after parsing the code elements.
        /// Only one of the two properties Program or Class can be not null.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary> 
        public ProgramClassDocument ProgramClassDocumentSnapshot { get; private set; }


        /// <summary>
        /// Temporary Snapshot stored between AST and SemanticCrossCheck phase. 
        /// Only use this snapshot to go from AST to SemanticCrossCheck phase. 
        /// </summary>
        public TemporarySemanticDocument TemporaryProgramClassDocumentSnapshot { get; private set; }


        /// <summary>
        /// Final snapshot of the compilation unit, it captures the fully parsed Cobol program
        /// along with all code-quality diagnostics produced during the code analysis phase.
        /// This property is thread-safe.
        /// </summary>
        public InspectedProgramClassDocument CodeAnalysisDocumentSnapshot { get; private set; }

        /// <summary>
        /// Return All diagnostics from all snapshots (token, CodeElement, Node, ...) with the possibility
        /// to get only diagnostics up to CodeElement phase.
        /// </summary>
        /// <param name="onlyCodeElementDiagnostics">True to get diagnostics produced up to CodeElement phase, False to get everything.</param>
        /// <returns>A list of selected diagnostics.</returns>
        public IList<Diagnostic> AllDiagnostics(bool onlyCodeElementDiagnostics)
        {
            CodeElementsDocument codeElementsDocumentSnapshot;
            lock (lockObjectForCodeElementsDocumentSnapshot)
            {
                codeElementsDocumentSnapshot = CodeElementsDocumentSnapshot;
            }

            //No CodeElements yet, so the base method will return every diagnostics
            if (codeElementsDocumentSnapshot == null)
            {
                return base.AllDiagnostics();
            }

            //CodeElements parsing diagnostics
            var diagnostics = new List<Diagnostic>();
            foreach (var codeElementsLine in codeElementsDocumentSnapshot.Lines)
            {
                codeElementsLine.CollectDiagnostics(diagnostics);
            }

            if (onlyCodeElementDiagnostics) return diagnostics; //No need to go further

            TemporarySemanticDocument temporarySemanticDocument;
            lock (lockObjectForTemporarySemanticDocument)
            {
                temporarySemanticDocument = TemporaryProgramClassDocumentSnapshot;
            }

            if (temporarySemanticDocument == null) return diagnostics;

            //Node parsing diagnostics
            diagnostics.AddRange(temporarySemanticDocument.Diagnostics);

            //Diagnostics on nodes themselves
            temporarySemanticDocument.Root?.AcceptASTVisitor(new DiagnosticsChecker(diagnostics));

            //Code analysis diagnostics
            InspectedProgramClassDocument inspectedProgramClassDocument;
            lock (lockObjectForCodeAnalysisDocumentSnapshot)
            {
                inspectedProgramClassDocument = CodeAnalysisDocumentSnapshot;
            }

            if (inspectedProgramClassDocument == null) return diagnostics;

            diagnostics.AddRange(inspectedProgramClassDocument.Diagnostics);

            return diagnostics;
        }

        /// <summary>
        /// Return all diagnostics from all snapshots
        /// </summary>
        /// <returns></returns>
        public override IList<Diagnostic> AllDiagnostics() => AllDiagnostics(false);

        /// <summary>
        /// Subscribe to this event to be notified of all changes in the complete program or class view of the document
        /// </summary>
        public event EventHandler<ProgramClassEvent> ProgramClassChanged;

        /// <summary>
        /// Subscribe to this event to be notified when no changes in the complete program or class view of the document has been
        /// detected after a snapshot refresh.
        /// </summary>
        public event EventHandler<ProgramClassEvent> ProgramClassNotChanged;

        /// <summary>
        /// Subscribe to this event to get notified of new code analysis results.
        /// Re-use of <code>ProgramClassEvent</code> class but the version number does correspond to the <code>CodeAnalysisDocumentSnapshot</code> version.
        /// </summary>
        public event EventHandler<ProgramClassEvent> CodeAnalysisCompleted;

        /// <summary>
        /// Performance stats for the TemporaryProgramClassDocumentSnapshot method
        /// </summary>
        public PerfStatsForParsingStep PerfStatsForTemporarySemantic { get; }

        public PerfStatsForParsingStep PerfStatsForProgramCrossCheck { get; }

        public PerfStatsForCompilationStep PerfStatsForCodeQualityCheck { get; }

        #region Thread ownership and synchronization

        // Synchronize accesses during snapshots updates
        protected readonly object lockObjectForCodeElementsDocumentSnapshot = new object();
        protected readonly object lockObjectForTemporarySemanticDocument = new object();
        protected readonly object lockObjectForProgramClassDocumentSnapshot = new object();
        protected readonly object lockObjectForCodeAnalysisDocumentSnapshot = new object();

        #endregion
    }

    public class ProgramClassEvent : EventArgs
    {
        public int Version { get; set; }
    }
}
