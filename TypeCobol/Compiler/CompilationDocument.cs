using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using JetBrains.Annotations;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.AntlrUtils;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Partial compilation pipeline : from text to preprocessor, used for COPY text
    /// </summary>
    public class CompilationDocument
    {
        protected static void AddScannerDiagnostics(ITokensLine tokensLine, List<Diagnostic> diagnostics)
        {
            if (tokensLine.ScannerDiagnostics != null)
            {
                diagnostics.AddRange(tokensLine.ScannerDiagnostics);
            }
        }

        protected static void AddPreprocessorDiagnostics(IProcessedTokensLine processedTokensLine, List<Diagnostic> diagnostics)
        {
            if (processedTokensLine.CompilerListingControlDirective?.ParsingDiagnostics != null)
            {
                diagnostics.AddRange(processedTokensLine.CompilerListingControlDirective.ParsingDiagnostics);
            }

            if (processedTokensLine.PreprocessorDiagnostics != null)
            {
                diagnostics.AddRange(processedTokensLine.PreprocessorDiagnostics);
            }
        }

        /// <summary>
        /// Text source name and format
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; private set; }

        /// <summary>
        /// TypeCobol compiler options
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>Optional custom symbol table to use for name and type resolution.</summary>
        public CodeModel.SymbolTable CustomSymbols = null;

        /// <summary>
        /// The build system implements an efficient way to retrieve CompilationDocument
        /// for all COPY compiler directives
        /// </summary>
        private IDocumentImporter _documentImporter;

        // Internal storage of the document lines :
        // - the document lines are stored in a tree structure, allowing efficient updates
        // - the tree nodes do not store a pointer to their father node, enabling efficient snapshots of the tree
        // - iterating on the elements of this list requires the allocation of an external stack object, which is retrieved from a pool
        // - accessing an element by index requires traversing the tree from its root, a O(log n) operation
        private ImmutableList<CodeElementsLine>.Builder compilationDocumentLines;

        /// <summary>
        /// Text names variations declared in REMARKS compiler directives or automaticly detected in CompilerDirectiveBuilder.
        /// </summary>
        public List<RemarksDirective.TextNameVariation> CopyTextNamesVariations { get; set; }

        public HashSet<string> MissingCopies { get; }

        /// <summary>
        /// Issue #315
        /// </summary>
        private MultilineScanState InitialScanState { get; }

        /// <summary>
        /// Special compilation mode for copies that are not imported from another document.
        /// </summary>
        protected bool UseDirectCopyParsing { get; }

#if EUROINFO_RULES
        /// <summary>
        /// Collected CopyNames if -cpyr report is selected.
        /// </summary>
        public IDictionary<string, HashSet<string>> CollectedCopyNames { get; private set; }
#endif

        /// <summary>
        /// Informations used to track the performance of each compilation step
        /// </summary>
        public class PerfStatsForCompilationStep
        {
            public PerfStatsForCompilationStep(CompilationStep compilationStep)
            {
                CompilationStep = compilationStep;
            }

            public CompilationStep CompilationStep { get; private set; }

            public int RefreshCount { get; private set; }
            public int LastRefreshTime { get; private set; }

            public int FirstCompilationTime { get; private set; }
            public int TotalRefreshTime { get; private set; }
            public int AverageRefreshTime { get { return RefreshCount < 2 ? 0 : TotalRefreshTime / (RefreshCount - 1); } }

            private Stopwatch stopWatch = new Stopwatch();

            public void OnStartRefresh()
            {
                stopWatch.Restart();
            }

            public void OnStopRefresh()
            {
                stopWatch.Stop();

                RefreshCount++;
                LastRefreshTime = (int)stopWatch.ElapsedMilliseconds;

                if (RefreshCount == 1)
                {
                    FirstCompilationTime = LastRefreshTime;
                }
                else
                {
                    TotalRefreshTime += LastRefreshTime;
                }
            }
        }

        public class PerfStatsForParsingStep : PerfStatsForCompilationStep
        {
            public PerfStatsForParsingStep(CompilationStep compilationStep) : base(compilationStep)
            { }

            public PerfStatsForParserInvocation FirstParsingTime { get; private set; }
            public PerfStatsForParserInvocation LastParsingTime { get; private set; }
            public PerfStatsForParserInvocation TotalParsingTime { get; private set; }

            /// <summary>
            /// Set to true to activate very detailed Anltr profiling statistics, which can then be accessed 
            /// through XxxParserStep.AntlrPerformanceProfiler static properties
            /// </summary>
            public bool ActivateDetailedAntlrPofiling { get; set; }

            public PerfStatsForParserInvocation OnStartRefreshParsingStep()
            {
                LastParsingTime = new PerfStatsForParserInvocation(ActivateDetailedAntlrPofiling);
                OnStartRefresh();
                return LastParsingTime;
            }

            public void OnStopRefreshParsingStep()
            {
                OnStopRefresh();
                if (FirstParsingTime == null)
                {
                    FirstParsingTime = LastParsingTime;
                    TotalParsingTime = new PerfStatsForParserInvocation(ActivateDetailedAntlrPofiling);
                }
                TotalParsingTime.Add(LastParsingTime);
            }
        }


        // --- Initialization ---

        /// <summary>
        /// Initializes a new compilation document from a list of text lines.
        /// This method does not scan the inserted text lines to produce tokens.
        /// You must explicitely call UpdateTokensLines() to start an initial scan of the document.
        /// </summary>
        public CompilationDocument(TextSourceInfo textSourceInfo, bool isImported, IEnumerable<ITextLine> initialTextLines, TypeCobolOptions compilerOptions, IDocumentImporter documentImporter,
            [NotNull] MultilineScanState initialScanState, List<RemarksDirective.TextNameVariation> copyTextNameVariations)
        {
            //Cannot import a program
            if (isImported) Debug.Assert(textSourceInfo.IsCopy);

            TextSourceInfo = textSourceInfo;
            CompilerOptions = compilerOptions;
            CopyTextNamesVariations = copyTextNameVariations ?? new List<RemarksDirective.TextNameVariation>();
            MissingCopies = new HashSet<string>(StringComparer.OrdinalIgnoreCase);

            this._documentImporter = documentImporter;

            // Initialize the compilation document lines
            compilationDocumentLines = ImmutableList<CodeElementsLine>.Empty.ToBuilder();

            // ... with the initial list of text lines received as a parameter
            if (initialTextLines != null)
            {
                // Insert Cobol text lines in the internal tree structure
                compilationDocumentLines.AddRange(initialTextLines.Select(textLine => CreateNewDocumentLine(textLine, textSourceInfo.ColumnsLayout)));
            }

            // Initialize document views versions
            currentTextLinesVersion = new DocumentVersion<ICobolTextLine>(this);
            currentTokensLinesVersion = new DocumentVersion<ITokensLine>(this);

            // Initialize performance stats 
            PerfStatsForText = new PerfStatsForCompilationStep(CompilationStep.Text);
            PerfStatsForScanner = new PerfStatsForCompilationStep(CompilationStep.Scanner);
            PerfStatsForPreprocessor = new PerfStatsForParsingStep(CompilationStep.Preprocessor);

            InitialScanState = initialScanState;
            UseDirectCopyParsing = textSourceInfo.IsCopy && !isImported;
        }

        /// <summary>
        /// Document line factory for the compiler processing steps : create new line from text
        /// </summary>
        protected CodeElementsLine CreateNewDocumentLine(ITextLine textLine, ColumnsLayout columnsLayout)
        {
            // Ensure all document lines are read-only snapshots
            ITextLine textLineSnapshot;
            if (!textLine.IsReadOnly)
            {
                textLineSnapshot = new TextLineSnapshot(textLine);
            }
            else
            {
                textLineSnapshot = textLine;
            }

            return new CodeElementsLine(textLineSnapshot, columnsLayout);
        }

        /// <summary>
        /// Document line factory for the compiler processing steps : create new version of a line by copy if necessary before an update
        /// </summary>
        protected object PrepareDocumentLineForUpdate(int index, object previousLineVersion, CompilationStep compilationStep)
        {
            CodeElementsLine originalLine = (CodeElementsLine)previousLineVersion;
            // If the compilation step was not yet applied to this line, we don't need a new version of the line
            if (originalLine.CanStillBeUpdatedBy(compilationStep))
            {
                return originalLine;
            }
            // If the compilation step was previously applied to this line, we need to create a new version of the line
            else
            {
                CodeElementsLine newLinePreparedForUpdate = new CodeElementsLine(originalLine, compilationStep);
                compilationDocumentLines[index] = newLinePreparedForUpdate;
                return newLinePreparedForUpdate;
            }
        }

        // --- Text lines ---

        /// <summary>
        /// Current list of text lines.
        /// NOT thread-safe : this property can only be accessed from the owner thread.
        /// </summary>
        public IReadOnlyList<ICobolTextLine> CobolTextLines
        {
            get
            {
                VerifyAccess();
                return compilationDocumentLines;
            }
        }

        /// <summary>
        /// Update the text lines of the document after a text change event.
        /// NOT thread-safe : this method can only be called from the owner thread.
        /// </summary>
        public void UpdateTextLines(TextChangedEvent textChangedEvent)
        {
            // This method can only be called by the document owner thread
            if (documentOwnerThread == null)
            {
                documentOwnerThread = Thread.CurrentThread;
            }
            else
            {
                VerifyAccess();
            }

            // Make sure we don't update the document while taking a snapshot
            DocumentChangedEvent<ICobolTextLine> documentChangedEvent = null;
            lock (lockObjectForDocumentLines)
            {
                // Start perf measurement
                PerfStatsForText.OnStartRefresh();

                // Apply text changes to the compilation document
                IList<DocumentChange<ICobolTextLine>> documentChanges = new List<DocumentChange<ICobolTextLine>>(textChangedEvent.TextChanges.Count);
                foreach (TextChange textChange in textChangedEvent.TextChanges)
                {
                    DocumentChange<ICobolTextLine> appliedChange = null;
                    CodeElementsLine newLine = null;
                    bool encounteredCodeElement;
                    switch (textChange.Type)
                    {
                        case TextChangeType.DocumentCleared:
                            compilationDocumentLines.Clear();
                            appliedChange = new DocumentChange<ICobolTextLine>(DocumentChangeType.DocumentCleared, 0, null);
                            // Ignore all previous document changes : they are meaningless now that the document was completely cleared
                            documentChanges.Clear();
                            break;
                        case TextChangeType.LineInserted:
                            newLine = CreateNewDocumentLine(textChange.NewLine, TextSourceInfo.ColumnsLayout);
                            compilationDocumentLines.Insert(textChange.LineIndex, newLine);

                            encounteredCodeElement = false; //Will allow to update allow line index without erasing all diagnostics after the first encountered line with CodeElements

                            foreach (var lineToUpdate in compilationDocumentLines.Skip(textChange.LineIndex+1)) //Loop on every line that appears after added line
                            {
                                //Remove generated diagnostics for the line below the inserted line.
                                if (!encounteredCodeElement)
                                    lineToUpdate.ResetDiagnostics(); //Reset diag when on the same zone

                                lineToUpdate.ShiftDown();
                                 
                                if (lineToUpdate.CodeElements != null)
                                    encounteredCodeElement = true;
                            }

                            appliedChange = new DocumentChange<ICobolTextLine>(DocumentChangeType.LineInserted, textChange.LineIndex, newLine);
                            break;
                        case TextChangeType.LineUpdated:
                            newLine = CreateNewDocumentLine(textChange.NewLine, TextSourceInfo.ColumnsLayout);
                            compilationDocumentLines[textChange.LineIndex] = newLine;
                            // Check to see if this change can be merged with a previous one
                            bool changeAlreadyApplied = false;
                            foreach (DocumentChange<ICobolTextLine> documentChangeToAdjust in documentChanges)
                            {
                                if (documentChangeToAdjust.LineIndex == textChange.LineIndex)
                                {
                                    changeAlreadyApplied = true;
                                    break;
                                }
                            }
                            if (!changeAlreadyApplied)
                            {
                                appliedChange = new DocumentChange<ICobolTextLine>(DocumentChangeType.LineUpdated, textChange.LineIndex, newLine);
                            }
                            // Line indexes are not impacted
                            break;
                        case TextChangeType.LineRemoved:
                            if (compilationDocumentLines.LastOrDefault() == null)
                                continue;
                            if (compilationDocumentLines.Count <= textChange.LineIndex) //Avoid line remove exception
                                continue;

                            compilationDocumentLines.RemoveAt(textChange.LineIndex);
                            encounteredCodeElement = false; //Will allow to update allow line index without erasing all diagnostics after the first encountered line with CodeElements

                            foreach (var lineToUpdate in compilationDocumentLines.Skip(textChange.LineIndex)) //Loop on every line that appears after deleted line
                            {
                                //Remove generated diagnostics for the line below the deleted line.
                                if (!encounteredCodeElement)
                                    lineToUpdate.ResetDiagnostics();

                                lineToUpdate.ShiftUp();

                                if (lineToUpdate.CodeElements != null)
                                    encounteredCodeElement = true; 
                            }

                            appliedChange = new DocumentChange<ICobolTextLine>(DocumentChangeType.LineRemoved, textChange.LineIndex, null);
                            // Recompute the line indexes of all the changes prevously applied
                            IList<DocumentChange<ICobolTextLine>> documentChangesToRemove = null;
                            foreach (DocumentChange<ICobolTextLine> documentChangeToAdjust in documentChanges)
                            {
                                if (documentChangeToAdjust.LineIndex > textChange.LineIndex)
                                {
                                    documentChangeToAdjust.LineIndex = documentChangeToAdjust.LineIndex - 1;
                                }
                                else if (documentChangeToAdjust.LineIndex == textChange.LineIndex)
                                {
                                    if (documentChangesToRemove == null)
                                    {
                                        documentChangesToRemove = new List<DocumentChange<ICobolTextLine>>(1);
                                    }
                                    documentChangesToRemove.Add(documentChangeToAdjust);
                                }
                            }
                            // Ignore all previous changes applied to a line now removed
                            if (documentChangesToRemove != null)
                            {
                                foreach (DocumentChange<ICobolTextLine> documentChangeToRemove in documentChangesToRemove)
                                {
                                    documentChanges.Remove(documentChangeToRemove);
                                }
                            }
                            break;
                    }
                    if (appliedChange != null)
                    {
                        documentChanges.Add(appliedChange);
                    }
                }

                // Create a new version of the document to track these changes
                currentTextLinesVersion.changes = documentChanges;
                currentTextLinesVersion.next = new DocumentVersion<ICobolTextLine>(currentTextLinesVersion);

                // Prepare an event to signal document change to all listeners
                documentChangedEvent = new DocumentChangedEvent<ICobolTextLine>(currentTextLinesVersion, currentTextLinesVersion.next);
                currentTextLinesVersion = currentTextLinesVersion.next;

                // Stop perf measurement
                PerfStatsForText.OnStopRefresh();
            }

            // Send events to all listeners
            EventHandler<DocumentChangedEvent<ICobolTextLine>> textLinesChanged = TextLinesChanged; // avoid race condition
            if (textLinesChanged != null)
            {
                textLinesChanged(this, documentChangedEvent);
            }
        }

        // Linked list of changes applied to the document text lines
        private DocumentVersion<ICobolTextLine> currentTextLinesVersion;

        /// <summary>
        /// Current version of the text lines of the document.
        /// NOT thread-safe : this method can only be called from the owner thread.
        /// </summary>
        public DocumentVersion<ICobolTextLine> CobolTextLinesVersion
        {
            get
            {
                VerifyAccess();
                return currentTextLinesVersion;
            }
        }

        /// <summary>
        /// Subscribe to this event to be notified of all changes in the text lines of the document
        /// </summary>
        public event EventHandler<DocumentChangedEvent<ICobolTextLine>> TextLinesChanged;

        /// <summary>
        /// Performance stats for the UpdateTextLines method
        /// </summary>
        public PerfStatsForCompilationStep PerfStatsForText { get; private set; }

        // --- Tokens lines ---

        /// <summary>
        /// Current list of tokens lines.
        /// NOT thread-safe : can only be accessed from the owner thread.
        /// </summary>
        public IReadOnlyList<ITokensLine> TokensLines
        {
            get
            {
                VerifyAccess();
                return compilationDocumentLines;
            }
        }

        /// <summary>
        /// Update the tokens lines of the document if the text lines changed since the last time this method was called.
        /// NOT thread-safe : this method can only be called from the owner thread.
        /// </summary>
        public void UpdateTokensLines(System.Action onVersion = null)
        {
            // This method can only be called by the document owner thread
            if (documentOwnerThread == null)
            {
                documentOwnerThread = Thread.CurrentThread;
            }
            else
            {
                VerifyAccess();
            }

            // Check if an update is necessary and compute changes to apply since last version
            bool scanAllDocumentLines = false;
            IList<DocumentChange<ICobolTextLine>> textLineChanges = null;
            if (textLinesVersionForCurrentTokensLines == null)
            {
                scanAllDocumentLines = true;
            }
            else if (currentTextLinesVersion == textLinesVersionForCurrentTokensLines)
            {
                // Text lines did not change since last update => nothing to do
                return;
            }
            else
            {
                textLineChanges = textLinesVersionForCurrentTokensLines.GetReducedAndOrderedChangesInNewerVersion(currentTextLinesVersion);
            }

            // Make sure we don't update the document while taking a snapshot
            DocumentChangedEvent<ITokensLine> documentChangedEvent = null;
            lock (lockObjectForDocumentLines)
            {
                // Start perf measurement
                PerfStatsForScanner.OnStartRefresh();

                // Apply text changes to the compilation document
                if (scanAllDocumentLines)
                {
                    ScannerStep.ScanDocument(TextSourceInfo, compilationDocumentLines, CompilerOptions, CopyTextNamesVariations, InitialScanState);
                    // Notify all listeners that the whole document has changed.
                    EventHandler wholeDocumentChanged = WholeDocumentChanged; // avoid race condition
                    wholeDocumentChanged?.Invoke(this, EventArgs.Empty);
                }
                else
                {
                    IList<DocumentChange<ITokensLine>> documentChanges = null;
                    documentChanges = ScannerStep.ScanTextLinesChanges(TextSourceInfo, compilationDocumentLines, textLineChanges, PrepareDocumentLineForUpdate, CompilerOptions, CopyTextNamesVariations, InitialScanState);
                    // Create a new version of the document to track these changes
                    currentTokensLinesVersion.changes = documentChanges;
                    currentTokensLinesVersion.next = new DocumentVersion<ITokensLine>(currentTokensLinesVersion);

                    // Prepare an event to signal document change to all listeners
                    documentChangedEvent = new DocumentChangedEvent<ITokensLine>(currentTokensLinesVersion, currentTokensLinesVersion.next);
                    currentTokensLinesVersion = currentTokensLinesVersion.next;
                    if (onVersion != null)
                        onVersion();

                    //So all lines changing incrementally must have their code element reset to NULL to make Code Element Incremental Parsing reparse them.
                    foreach (var change in documentChanges)
                    {
                        if (change.NewLine is TypeCobol.Compiler.Parser.CodeElementsLine)
                        {
                            TypeCobol.Compiler.Parser.CodeElementsLine ceLine =
                                (TypeCobol.Compiler.Parser.CodeElementsLine) change.NewLine;
                            ceLine.ResetCodeElements();
                        }
                    }
                }

                // Register that the tokens lines were synchronized with the current text lines version
                textLinesVersionForCurrentTokensLines = currentTextLinesVersion;

                // Stop perf measurement
                PerfStatsForScanner.OnStopRefresh();
            }

            // Send events to all listeners
            EventHandler<DocumentChangedEvent<ITokensLine>> tokensLinesChanged = TokensLinesChanged; // avoid race condition
            if (documentChangedEvent != null && tokensLinesChanged != null)
            {
                tokensLinesChanged(this, documentChangedEvent);
            }
        }

        // Linked list of changes applied to the document text lines
        private DocumentVersion<ICobolTextLine> textLinesVersionForCurrentTokensLines;

        // Linked list of changes applied to the document tokens lines
        private DocumentVersion<ITokensLine> currentTokensLinesVersion;

        /// <summary>
        /// Current version of the text lines of the document.
        /// NOT thread-safe : this method can only be called from the owner thread.
        /// </summary>
        public DocumentVersion<ITokensLine> TokensLinesVersion
        {
            get
            {
                VerifyAccess();
                return currentTokensLinesVersion;
            }
        }

        /// <summary>
        /// Subscribe to this event to be notified of all changes in the tokens lines of the document
        /// </summary>
        public event EventHandler<DocumentChangedEvent<ITokensLine>> TokensLinesChanged;

        /// <summary>
        /// Subscribe to this event to be notified when whole document has changed.
        /// </summary>
        public event EventHandler WholeDocumentChanged;

        /// <summary>
        /// Performance stats for the UpdateTokensLines method
        /// </summary>
        public PerfStatsForCompilationStep PerfStatsForScanner { get; private set; }

        // --- Document snapshots ---

        /// <summary>
        /// Creates a new snapshot of the document viewed as tokens BEFORE compiler directives processing.
        /// Thread-safe : this method can be called from any thread.
        /// </summary>
        public void RefreshTokensDocumentSnapshot()
        {
            // Make sure we don't update the document while taking a snapshot
            lock (lockObjectForDocumentLines)
            {
                // Create a new snapshot only if things changed since last snapshot
                if (TokensDocumentSnapshot == null || TokensDocumentSnapshot.CurrentVersion != currentTokensLinesVersion)
                {
                    TokensDocumentSnapshot = new TokensDocument(TextSourceInfo, textLinesVersionForCurrentTokensLines, currentTokensLinesVersion, compilationDocumentLines.ToImmutable());
                }
            }
        }

        /// <summary>
        /// Last snapshot of the compilation document viewed as a raw set of tokens, before processing the compiler directives.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary>
        public TokensDocument TokensDocumentSnapshot { get; private set; }

        /// <summary>
        /// Creates a new snapshot of the document viewed as tokens AFTER compiler directives processing.
        /// (if the tokens lines changed since the last time this method was called)
        /// Thread-safe : this method can be called from any thread.
        /// </summary>
        public void RefreshProcessedTokensDocumentSnapshot()
        {
            // Make sure two threads don't try to update this snapshot at the same time
            lock (lockObjectForProcessedTokensDocumentSnapshot)
            {
                // Capture previous snapshots at one point in time
                TokensDocument tokensDocument = TokensDocumentSnapshot;
                ProcessedTokensDocument previousProcessedTokensDocument = ProcessedTokensDocumentSnapshot;

                // Check if an update is necessary and compute changes to apply since last version
                bool scanAllDocumentLines = false;
                IList<DocumentChange<ITokensLine>> tokensLineChanges = null;
                if (previousProcessedTokensDocument == null)
                {
                    scanAllDocumentLines = true;
                }
                else if (tokensDocument.CurrentVersion == previousProcessedTokensDocument.PreviousStepSnapshot.CurrentVersion)
                {
                    // Tokens lines did not change since last update => nothing to do
                    return;
                }
                else
                {
                    DocumentVersion<ITokensLine> previousTokensDocumentVersion = previousProcessedTokensDocument.PreviousStepSnapshot.CurrentVersion;
                    tokensLineChanges = previousTokensDocumentVersion.GetReducedAndOrderedChangesInNewerVersion(tokensDocument.CurrentVersion);
                }

                // Start perf measurement
                var perfStatsForParserInvocation = PerfStatsForPreprocessor.OnStartRefreshParsingStep();

                // Track all changes applied to the document while updating this snapshot
                DocumentChangedEvent<IProcessedTokensLine> documentChangedEvent = null;

                // Apply text changes to the compilation document
                bool refreshMissingCopies = true;
                List<string> missingCopies = null;
                if (scanAllDocumentLines)
                {
                    if (tokensDocument != null)
                    {
                        // Process all lines of the document for the first time
                        PreprocessorStep.ProcessDocument(this, ((ImmutableList<CodeElementsLine>)tokensDocument.Lines), _documentImporter, perfStatsForParserInvocation, out missingCopies);

                        // Create the first processed tokens document snapshot
                        ProcessedTokensDocumentSnapshot = CreateProcessedTokensDocument(new DocumentVersion<IProcessedTokensLine>(this), (ISearchableReadOnlyList<CodeElementsLine>) tokensDocument.Lines);
                    }
                    else
                    {
                        // No new ProcessedTokensDocument created, so no need to refresh missing copies
                        refreshMissingCopies = false;
                    }
                }
                else
                {
                    ImmutableList<CodeElementsLine>.Builder processedTokensDocumentLines = ((ImmutableList<CodeElementsLine>) tokensDocument.Lines).ToBuilder();
                    IList<DocumentChange<IProcessedTokensLine>> documentChanges = PreprocessorStep.ProcessChanges(this,
                        processedTokensDocumentLines, tokensLineChanges, PrepareDocumentLineForUpdate,
                        _documentImporter, perfStatsForParserInvocation, out missingCopies);

                    // Create a new version of the document to track these changes
                    DocumentVersion<IProcessedTokensLine> currentProcessedTokensLineVersion = previousProcessedTokensDocument.CurrentVersion;
                    currentProcessedTokensLineVersion.changes = documentChanges;
                    currentProcessedTokensLineVersion.next = new DocumentVersion<IProcessedTokensLine>(currentProcessedTokensLineVersion);

                    // Prepare an event to signal document change to all listeners
                    documentChangedEvent = new DocumentChangedEvent<IProcessedTokensLine>(currentProcessedTokensLineVersion, currentProcessedTokensLineVersion.next);
                    currentProcessedTokensLineVersion = currentProcessedTokensLineVersion.next;

                    // Update the processed tokens document snapshot
                    ProcessedTokensDocumentSnapshot = CreateProcessedTokensDocument(currentProcessedTokensLineVersion, processedTokensDocumentLines.ToImmutable());
                }

                ProcessedTokensDocument CreateProcessedTokensDocument(DocumentVersion<IProcessedTokensLine> version, ISearchableReadOnlyList<CodeElementsLine> lines)
                {
                    //Apply automatic replacing of partial-words for direct copy parsing mode
                    return UseDirectCopyParsing
                        ? new AutoReplacePartialWordsTokensDocument(tokensDocument, version, lines, CompilerOptions)
                        : new ProcessedTokensDocument(tokensDocument, version, lines, CompilerOptions);
                }

                // Refresh missing copies
                if (refreshMissingCopies)
                {
                    MissingCopies.Clear();
                    foreach (var missingCopy in missingCopies)
                    {
                        MissingCopies.Add(missingCopy);
                    }
                }

                // Stop perf measurement
                PerfStatsForPreprocessor.OnStopRefreshParsingStep();

                // Send events to all listeners
                EventHandler<DocumentChangedEvent<IProcessedTokensLine>> processedTokensLinesChangedEventsSource = ProcessedTokensLinesChangedEventsSource; // avoid race condition
                if (documentChangedEvent != null && processedTokensLinesChangedEventsSource != null)
                {
                    processedTokensLinesChangedEventsSource(this, documentChangedEvent);
                }
            }
        }

        /// <summary>
        /// Last snapshot of the compilation document viewed as a final set of tokens, after processing the compiler directives (COPY & REPLACE).
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary>
        public ProcessedTokensDocument ProcessedTokensDocumentSnapshot { get; private set; }

        /// <summary>
        /// Subscribe to this event to be notified of all changes in the processed tokens lines of the document
        /// </summary>
        public event EventHandler<DocumentChangedEvent<IProcessedTokensLine>> ProcessedTokensLinesChangedEventsSource;

        /// <summary>
        /// Performance stats for the RefreshProcessedTokensDocumentSnapshot method
        /// </summary>
        public PerfStatsForParsingStep PerfStatsForPreprocessor { get; private set; }

        #region Thread ownership and synchronization
        // Inspired from ICSharpCode.AvalonEdit.Document.TextDocument
        // Copyright (c) 2014 AlphaSierraPapa for the SharpDevelop Team

        // Synchronize accesses during compilationDocumentLines updates 
        Thread documentOwnerThread;
        protected readonly object lockObjectForDocumentLines = new object();

        // Synchronize accesses during snapshots updates
        protected readonly object lockObjectForProcessedTokensDocumentSnapshot = new object();

        /// <summary>
        /// Verifies that the current thread is the documents owner thread.
        /// Throws an <see cref="InvalidOperationException"/> if the wrong thread accesses the CompilationDocument.
        /// </summary>
        /// <remarks>
        /// <para>The CompilationDocument class is not thread-safe. A document instance expects to have a single owner thread
        /// and will throw an <see cref="InvalidOperationException"/> when accessed from another thread.
        /// It is possible to change the owner thread using the <see cref="SetOwnerThread"/> method.</para>
        /// </remarks>
        public void VerifyAccess()
        {
#if DEBUG
            if (Thread.CurrentThread != documentOwnerThread)
                throw new InvalidOperationException("CompilationDocument can be accessed only from the thread that owns it.");
#endif
        }

        /// <summary>
        /// Transfers ownership of the document to another thread. This method can be used to load
        /// a file into a TextDocument on a background thread and then transfer ownership to the UI thread
        /// for displaying the document.
        /// </summary>
        /// <remarks>
        /// <inheritdoc cref="VerifyAccess"/>
        /// <para>
        /// The owner can be set to null, which means that no thread can access the document. But, if the document
        /// has no owner thread, any thread may take ownership by calling <see cref="SetOwnerThread"/>.
        /// </para>
        /// </remarks>
        public void SetOwnerThread(Thread newOwner)
        {
            // We need to lock here to ensure that in the null owner case,
            // only one thread succeeds in taking ownership.
            lock (lockObjectForDocumentLines)
            {
                if (documentOwnerThread != null)
                {
                    VerifyAccess();
                }
                documentOwnerThread = newOwner;
            }
        }
        #endregion


        /// <summary>
        /// Return all diagnostics from all snapshots
        /// </summary>
        /// <returns></returns>
        public virtual IList<Diagnostic> AllDiagnostics()
        {
            var diagnostics = new List<Diagnostic>();

            ProcessedTokensDocument processedTokensDocument;
            lock (lockObjectForProcessedTokensDocumentSnapshot)
            {
                processedTokensDocument = ProcessedTokensDocumentSnapshot;
            }

            if (processedTokensDocument != null)
            {
                //We got a ProcessedTokensDocument, iterate over its lines
                foreach (var processedTokensLine in processedTokensDocument.Lines)
                {
                    AddScannerDiagnostics(processedTokensLine, diagnostics);
                    AddPreprocessorDiagnostics(processedTokensLine, diagnostics);
                }
            }
            else
            {
                //Try get a TokensDocument...
                TokensDocument tokensDocument;
                lock (lockObjectForDocumentLines)
                {
                    tokensDocument = TokensDocumentSnapshot;
                }

                if (tokensDocument != null)
                {
                    //Iterate over TokensLines
                    foreach (var tokensLine in tokensDocument.Lines)
                    {
                        AddScannerDiagnostics(tokensLine, diagnostics);
                    }
                }
                //else no snapshot available yet
            }

            return diagnostics;
        }

#if EUROINFO_RULES
        internal void CollectUsedCopy(CopyDirective copy)
        {
            if (CollectedCopyNames == null)
            {
                CollectedCopyNames = new Dictionary<string, HashSet<string>>(StringComparer.OrdinalIgnoreCase);
            }

            if (!CollectedCopyNames.TryGetValue(copy.TextName, out var suffixedNames))
            {
                suffixedNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                CollectedCopyNames.Add(copy.TextName, suffixedNames);
            }

            if (copy.InsertSuffixChar)
            {
                suffixedNames.Add(copy.TextName + copy.Suffix);
            }
        }
#endif

    }
}
