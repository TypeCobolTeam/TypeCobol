using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Threading;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Partial compilation pipeline : from text to preprocessor, used for COPY text
    /// </summary>
    public class CompilationDocument
    {
        /// <summary>
        /// Text source name and format
        /// </summary>
        public TextSourceInfo TextSourceInfo { get; private set; }

        /// <summary>
        /// TypeCobol compiler options
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>
        /// The build system implements an efficient way to retrieve ProcessedTokensDocuments
        /// for all COPY compiler directives
        /// </summary>
        private IProcessedTokensDocumentProvider processedTokensDocumentProvider;

        // Internal storage of the document lines :
        // - the document lines are stored in a tree structure, allowing efficient updates
        // - the tree nodes do not store a pointer to their father node, enabling efficient snapshots of the tree
        // - iterating on the elements of this list requires the allocation of an external stack object, which is retrieved from a pool
        // - accessing an element by index requires traversing the tree from its root, a O(log n) operation
        private ImmutableList<CodeElementsLine>.Builder compilationDocumentLines;

        // --- Initialization ---

        /// <summary>
        /// Initializes a new compilation document from a list of text lines.
        /// This method does not scan the inserted text lines to produce tokens.
        /// You must explicitely call Synchronize
        /// </summary>
        public CompilationDocument(TextSourceInfo textSourceInfo, IEnumerable<ITextLine> initialTextLines, TypeCobolOptions compilerOptions, IProcessedTokensDocumentProvider processedTokensDocumentProvider)
        {
            TextSourceInfo = textSourceInfo;
            CompilerOptions = compilerOptions;
            this.processedTokensDocumentProvider = processedTokensDocumentProvider;

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
                // Apply text changes to the compilation document
                IList<DocumentChange<ICobolTextLine>> documentChanges = new List<DocumentChange<ICobolTextLine>>(textChangedEvent.TextChanges.Count);
                foreach (TextChange textChange in textChangedEvent.TextChanges)
                {
                    DocumentChange<ICobolTextLine> appliedChange = null;
                    CodeElementsLine newLine = null;
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
                            appliedChange = new DocumentChange<ICobolTextLine>(DocumentChangeType.LineInserted, textChange.LineIndex, newLine);
                            // Recompute the line indexes of all the changes prevously applied
                            foreach (DocumentChange<ICobolTextLine> documentChangeToAdjust in documentChanges)
                            {
                                if(documentChangeToAdjust.LineIndex >= textChange.LineIndex)
                                {
                                    documentChangeToAdjust.LineIndex = documentChangeToAdjust.LineIndex + 1;
                                }
                            }
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
                            compilationDocumentLines.RemoveAt(textChange.LineIndex);
                            appliedChange = new DocumentChange<ICobolTextLine>(DocumentChangeType.LineRemoved, textChange.LineIndex, null);
                            // Recompute the line indexes of all the changes prevously applied
                            IList<DocumentChange<ICobolTextLine>> documentChangesToRemove = null;
                            foreach (DocumentChange<ICobolTextLine> documentChangeToAdjust in documentChanges)
                            {
                                if (documentChangeToAdjust.LineIndex > textChange.LineIndex)
                                {
                                    documentChangeToAdjust.LineIndex = documentChangeToAdjust.LineIndex - 1;
                                }
                                else if(documentChangeToAdjust.LineIndex == textChange.LineIndex)
                                {
                                    if(documentChangesToRemove == null)
                                    {
                                        documentChangesToRemove = new List<DocumentChange<ICobolTextLine>>(1);
                                    }
                                    documentChangesToRemove.Add(documentChangeToAdjust);
                                }
                            }
                            // Ignore all previous changes applied to a line now removed
                            if(documentChangesToRemove != null)
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
            }
           
            // Send events to all listeners
            textLinesChangedEventsSource.OnNext(documentChangedEvent);
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

        // Broadcast document text lines changes to all listeners
        private ISubject<DocumentChangedEvent<ICobolTextLine>> textLinesChangedEventsSource = new Subject<DocumentChangedEvent<ICobolTextLine>>();

        /// <summary>
        /// Subscribe to this events source to be notified of all changes in the text lines of the document
        /// </summary>
        public IObservable<DocumentChangedEvent<ICobolTextLine>> TextLinesChangedEventsSource
        {
            get { return textLinesChangedEventsSource; }
        }

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
        public void UpdateTokensLines()
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
            if(textLinesVersionForCurrentTokensLines == null)
            {
                scanAllDocumentLines = true;
            }
            else if(currentTextLinesVersion == textLinesVersionForCurrentTokensLines)
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
                // Apply text changes to the compilation document
                if (scanAllDocumentLines)
                {
                    ScannerStep.ScanDocument(TextSourceInfo, compilationDocumentLines, CompilerOptions);
                }
                else
                {
                    IList<DocumentChange<ITokensLine>> documentChanges = ScannerStep.ScanTextLinesChanges(TextSourceInfo, compilationDocumentLines, textLineChanges, PrepareDocumentLineForUpdate, CompilerOptions);

                    // Create a new version of the document to track these changes
                    currentTokensLinesVersion.changes = documentChanges;
                    currentTokensLinesVersion.next = new DocumentVersion<ITokensLine>(currentTokensLinesVersion);

                    // Prepare an event to signal document change to all listeners
                    documentChangedEvent = new DocumentChangedEvent<ITokensLine>(currentTokensLinesVersion, currentTokensLinesVersion.next);
                    currentTokensLinesVersion = currentTokensLinesVersion.next;

                    // Register that the tokens lines were synchronized with the current text lines version
                    textLinesVersionForCurrentTokensLines = currentTextLinesVersion;
                }
            }

            // Send events to all listeners
            tokensLinesChangedEventsSource.OnNext(documentChangedEvent);
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

        // Broadcast document tokens lines changes to all listeners
        private ISubject<DocumentChangedEvent<ITokensLine>> tokensLinesChangedEventsSource = new Subject<DocumentChangedEvent<ITokensLine>>();

        /// <summary>
        /// Subscribe to this events source to be notified of all changes in the tokens lines of the document
        /// </summary>
        public IObservable<DocumentChangedEvent<ITokensLine>> TokensLinesChangedEventsSource
        {
            get { return tokensLinesChangedEventsSource; }
        }

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
            lock(lockObjectForProcessedTokensDocumentSnapshot)
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

                // Track all changes applied to the document while updating this snapshot
                DocumentChangedEvent<IProcessedTokensLine> documentChangedEvent = null;
               
                // Apply text changes to the compilation document
                if (scanAllDocumentLines)
                {
                    PreprocessorStep.ProcessDocument(TextSourceInfo, ((ImmutableList<CodeElementsLine>)previousProcessedTokensDocument.Lines), CompilerOptions, processedTokensDocumentProvider);
                }
                else
                {
                    ImmutableList<CodeElementsLine>.Builder processedTokensDocumentLines = ((ImmutableList<CodeElementsLine>)previousProcessedTokensDocument.Lines).ToBuilder();
                    IList<DocumentChange<IProcessedTokensLine>> documentChanges = PreprocessorStep.ProcessTokensLinesChanges(TextSourceInfo, processedTokensDocumentLines, tokensLineChanges, PrepareDocumentLineForUpdate, CompilerOptions, processedTokensDocumentProvider);

                    // Create a new version of the document to track these changes
                    DocumentVersion<IProcessedTokensLine> currentProcessedTokensLineVersion = previousProcessedTokensDocument.CurrentVersion;
                    currentProcessedTokensLineVersion.changes = documentChanges;
                    currentProcessedTokensLineVersion.next = new DocumentVersion<IProcessedTokensLine>(currentProcessedTokensLineVersion);

                    // Prepare an event to signal document change to all listeners
                    documentChangedEvent = new DocumentChangedEvent<IProcessedTokensLine>(currentProcessedTokensLineVersion, currentProcessedTokensLineVersion.next);
                    currentProcessedTokensLineVersion = currentProcessedTokensLineVersion.next;

                    // Update the processed tokens document snapshot
                    ProcessedTokensDocumentSnapshot = new ProcessedTokensDocument(tokensDocument, currentProcessedTokensLineVersion, processedTokensDocumentLines);
                }

                // Send events to all listeners
                processedTokensLinesChangedEventsSource.OnNext(documentChangedEvent);
            }
        }

        /// <summary>
        /// Last snapshot of the compilation document viewed as a final set of tokens, after processing the compiler directives (COPY & REPLACE).
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary>
        public ProcessedTokensDocument ProcessedTokensDocumentSnapshot { get; private set; }

        // Broadcast document processed tokens lines changes to all listeners
        private ISubject<DocumentChangedEvent<IProcessedTokensLine>> processedTokensLinesChangedEventsSource = new Subject<DocumentChangedEvent<IProcessedTokensLine>>();

        /// <summary>
        /// Subscribe to this events source to be notified of all changes in the processed tokens lines of the document
        /// </summary>
        public IObservable<DocumentChangedEvent<IProcessedTokensLine>> ProcessedTokensLinesChangedEventsSource
        {
            get { return processedTokensLinesChangedEventsSource; }
        }

        #region Thread ownership
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
    }
    /*
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
        /// View of a source document as lines of tokens after the lexical analysis stage 
        /// </summary>
		public TokensDocument TokensDocument { get; private set; }

        /// <summary>
        /// View of a source document after COPY and REPLACE processing
        /// </summary>
        public ProcessedTokensDocument ProcessedTokensDocument { get; private set; }

        /// <summary>
        /// List of CompilationDocuments imported by COPY directives in the current document
        /// </summary>
        public IList<CompilationDocument> CopyReferences { get; private set; }        
        
        /// <summary>
        /// Used to find the secondary files imported by the COPY statement 
        /// </summary>
        protected SourceFileProvider sourceFileProvider;

        /// <summary>
        /// Compiler options directing the scanner and preprocessor operations
        /// </summary>
        public TypeCobolOptions CompilerOptions { get; private set; }

        /// <summary>
        /// List of error messages produced while analyzing this document
        /// </summary>
        public IEnumerable<Diagnostic> Diagnostics 
        { 
            get 
            {
                foreach(ITokensLine line in TokensDocument.TokensLines)
                {
                    if (line.ScannerDiagnostics != null && line.ScannerDiagnostics.Count > 0)
                    {
                        foreach (Diagnostic diagnostic in line.ScannerDiagnostics)
                        {
                            yield return diagnostic;
                        }
                    }
                }
            } 
        }

        /// <summary>
        /// Load a Cobol source file in memory
        /// </summary>
        public CompilationDocument(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ColumnsLayout columnsLayout, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            CompilerOptions = compilerOptions;

            // 1. Find the Cobol source file
            CobolFile sourceFile;
            if(sourceFileProvider.TryGetFile(libraryName, textName, out sourceFile))
            {
                CobolFile = sourceFile;
            }
            else
            {
                throw new Exception(String.Format("Could not find a Cobol source file named {0}", textName));
            }

            // 2. Load it in a new text document in memory
            TextDocument = new ReadOnlyTextDocument(sourceFile.Name, sourceFile.Encoding, columnsLayout, sourceFile.ReadChars());

            // 3. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, compilerOptions);

            // 4. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Load a Cobol source file in an existing text document
        /// </summary>
        public CompilationDocument(string libraryName, string textName, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, ITextDocument textDocument, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            CompilerOptions = compilerOptions;

            // 1. Find the Cobol source file
            CobolFile sourceFile;
            if (sourceFileProvider.TryGetFile(libraryName, textName, out sourceFile))
            {
                CobolFile = sourceFile;
            }
            else
            {
                throw new Exception(String.Format("Could not find a Cobol source file named {0}", textName));
            }

            // 2. Load it in a new text document in memory
            TextDocument = textDocument;
            textDocument.LoadChars(sourceFile.ReadChars());

            // 3. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, CompilerOptions);

            // 4. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Initialize the compilation document from an existing text document, not yet associated with a Cobol file
        /// </summary>
        public CompilationDocument(ITextDocument textDocument, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            TextDocument = textDocument;
            CompilerOptions = compilerOptions;

            // 1. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, CompilerOptions);

            // 2. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Initialize the compilation document from an existing text document, already initialized from a Cobol file
        /// </summary>
        public CompilationDocument(ITextDocument textDocument, CobolFile cobolFile, SourceFileProvider sourceFileProvider, IProcessedTokensDocumentProvider documentProvider, TypeCobolOptions compilerOptions)
        {
            this.sourceFileProvider = sourceFileProvider;
            CobolFile = cobolFile;
            TextDocument = textDocument;
            CompilerOptions = compilerOptions;

            // 1. Prepare to scan the contents of the TextDocument to build TokensLines
            TokensDocument = new TokensDocument(TextDocument.Source, CompilerOptions);

            // 2. Prepare to compute the COPY and REPLACE statements
            CopyReferences = new List<CompilationDocument>();
            ProcessedTokensDocument = new ProcessedTokensDocument(TokensDocument, CompilerOptions, documentProvider);
        }

        /// <summary>
        /// Configure on which thread and at which frequency the preprocessor analyzes document changes
        /// </summary>
        public void SetupDocumentProcessingPipeline(IScheduler backgroundParsingScheduler, int bufferingDelayMillisecond)
        {
            // Scanner always operates on the same thread as TextDocument notifications
            TextDocument.TextChangedEventsSource.Subscribe(TokensDocument);

            // Mono thread configuration : Preprocessor operates on the same thread as TextDocument notifications
            if (backgroundParsingScheduler == null)
            {
                TokensDocument.TokensChangedEventsSource.Subscribe(ProcessedTokensDocument);
            }
            // Multi-thread configuration : Preprocessor operates independently on a background thread
            else
            {
                TokensDocument.TokensChangedEventsSource.Buffer(TimeSpan.FromMilliseconds(bufferingDelayMillisecond)).Where(evts => evts.Count > 0).
                    ObserveOn(backgroundParsingScheduler).Select(eventsList => TokensChangedEvent.Flatten(eventsList)).Subscribe(ProcessedTokensDocument);
            }
        }

        /// <summary>
        /// Scan and preprocess document changes
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
            CobolFile.CobolFileChangedEventsSource.Subscribe(observerTextDocument);

            // Start compilation process
            TextDocument.StartSendingChangeEvents();
            CobolFile.StartMonitoringExternalChanges();
        }

        /// <summary>
        /// Reload the entire file content in the text document each time the file is updated
        /// </summary>
        private class ObserverTextDocument : IObserver<CobolFileChangedEvent>
        {
            private CobolFile cobolFile;
            private ITextDocument textDocument;

            public ObserverTextDocument(CobolFile cobolFile, ITextDocument textDocument)
            {
                this.cobolFile = cobolFile;
                this.textDocument = textDocument;
            }

            public void OnNext(CobolFileChangedEvent value)
            {
                if (value.Type == CobolFileChangeType.FileChanged)
                {
                    textDocument.LoadChars(cobolFile.ReadChars());
                }
                else
                {
                    throw new InvalidOperationException("File change type " + value.Type + " is not supported in this configuration");
                }
            }

            public void OnCompleted()
            {
                // Do nothing
            }

            public void OnError(Exception error)
            {
                // Do nothing
            }
        }
    }

    /// <summary>
    /// Class used to group all the properties needed to load a CompilationDocument from a file
    /// </summary>
    public class DocumentFormat
    {
        public DocumentFormat(Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength, ColumnsLayout columnsLayout)
        {
            Encoding = encoding; 
            EndOfLineDelimiter = endOfLineDelimiter;
            if (endOfLineDelimiter == EndOfLineDelimiter.FixedLengthLines)
            {
                if (columnsLayout == ColumnsLayout.FreeTextFormat)
                {
                    throw new ArgumentException("With free text format, fixed length lines are not allowed");
                }
                else if (columnsLayout == ColumnsLayout.CobolReferenceFormat && fixedLineLength < 72)
                {
                    throw new ArgumentException("With Cobol reference format, fixed length lines must be at least 72 characters long");
                }
                FixedLineLength = fixedLineLength;
            }
            ColumnsLayout = columnsLayout;
        }

        /// <summary>
        /// Encoding used to read or write the Cobol file
        /// </summary>
        public Encoding Encoding { get; protected set; }

        /// <summary>
        /// Tells the compiler how to split a character stream in consecutive lines
        /// </summary>
        public EndOfLineDelimiter EndOfLineDelimiter { get; private set; }

        /// <summary>
        /// Used only in case the text is formatted with fixed length line
        /// </summary>
        public int FixedLineLength { get; private set; }

        /// <summary>
        /// Tells the compiler how to interpret the column position of each character in the source text
        /// </summary>
        public ColumnsLayout ColumnsLayout { get; private set; }

        // Most often used document formats
        public static DocumentFormat ZOsReferenceFormat = new DocumentFormat(IBMCodePages.GetDotNetEncodingFromIBMCCSID(1147), EndOfLineDelimiter.FixedLengthLines, 80, ColumnsLayout.CobolReferenceFormat);
        public static DocumentFormat RDZReferenceFormat = new DocumentFormat(Encoding.GetEncoding(1252) , EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.CobolReferenceFormat);
        public static DocumentFormat FreeTextFormat = new DocumentFormat(Encoding.GetEncoding(1252), EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
    }*/
}
