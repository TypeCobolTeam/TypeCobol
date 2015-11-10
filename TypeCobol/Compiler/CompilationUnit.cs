using System;
using System.Collections.Generic;
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
                    // Parse the whole document for the first time
                    CodeElementsParserStep.ParseDocument(TextSourceInfo, ((ImmutableList<CodeElementsLine>)processedTokensDocument.Lines), CompilerOptions);

                    // Create the first code elements document snapshot
                    CodeElementsDocumentSnapshot = new CodeElementsDocument(processedTokensDocument, new DocumentVersion<ICodeElementsLine>(this), ((ImmutableList<CodeElementsLine>)processedTokensDocument.Lines));
                }
                else
                {
                    ImmutableList<CodeElementsLine>.Builder codeElementsDocumentLines = ((ImmutableList<CodeElementsLine>)processedTokensDocument.Lines).ToBuilder();
                    IList<DocumentChange<ICodeElementsLine>> documentChanges = CodeElementsParserStep.ParseProcessedTokensLinesChanges(TextSourceInfo, codeElementsDocumentLines, processedTokensLineChanges, PrepareDocumentLineForUpdate, CompilerOptions);

                    // Create a new version of the document to track these changes
                    DocumentVersion<ICodeElementsLine> currentCodeElementsLinesVersion = previousCodeElementsDocument.CurrentVersion;
                    currentCodeElementsLinesVersion.changes = documentChanges;
                    currentCodeElementsLinesVersion.next = new DocumentVersion<ICodeElementsLine>(currentCodeElementsLinesVersion);

                    // Prepare an event to signal document change to all listeners
                    documentChangedEvent = new DocumentChangedEvent<ICodeElementsLine>(currentCodeElementsLinesVersion, currentCodeElementsLinesVersion.next);
                    currentCodeElementsLinesVersion = currentCodeElementsLinesVersion.next;

                    // Update the code elements document snapshot
                    CodeElementsDocumentSnapshot = new CodeElementsDocument(processedTokensDocument, currentCodeElementsLinesVersion, codeElementsDocumentLines.ToImmutable());
                }

                // Send events to all listeners
                EventHandler<DocumentChangedEvent<ICodeElementsLine>> codeElementsLinesChanged = CodeElementsLinesChanged; // avoid race condition
                if (documentChangedEvent != null && codeElementsLinesChanged != null)
                {
                    codeElementsLinesChanged(this, documentChangedEvent);
                }
            }
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
        /// Creates a new snapshot of the document viewed as complete Cobol Program or Class.
        /// (if the code elements lines changed since the last time this method was called)
        /// Thread-safe : this method can be called from any thread.
        /// </summary>
        public void RefreshProgramClassDocumentSnapshot()
        {
            // Make sure two threads don't try to update this snapshot at the same time
            bool snapshotWasUpdated = false;
            lock (lockObjectForProgramClassDocumentSnapshot)
            {
                // Capture previous snapshot at one point in time
                CodeElementsDocument codeElementsDocument = CodeElementsDocumentSnapshot;

                // Check if an update is necessary and compute changes to apply since last version
                if (ProgramClassDocumentSnapshot == null || ProgramClassDocumentSnapshot.PreviousStepSnapshot.CurrentVersion != codeElementsDocument.CurrentVersion)
                {
                    // Program and Class parsing is not incremental : the objects are rebuilt each time this method is called
                    Program newProgram;
                    Class newClass;
                    IList<ParserDiagnostic> newDiagnostics;
                    //TODO cast to ImmutableList<CodeElementsLine> sometimes fails here
                    ProgramClassParserStep.ParseProgramOrClass(TextSourceInfo, ((ImmutableList<CodeElementsLine>)codeElementsDocument.Lines), CompilerOptions, out newProgram, out newClass, out newDiagnostics);

                    // Capture the result of the parse in a new snapshot
                    ProgramClassDocumentSnapshot = new ProgramClassDocument(
                        codeElementsDocument, ProgramClassDocumentSnapshot == null ? 0 : ProgramClassDocumentSnapshot.CurrentVersion +1,
                        newProgram, newClass, newDiagnostics);
                    snapshotWasUpdated = true;
                }
            }

            // Send events to all listeners
            EventHandler<int> programClassChanged = ProgramClassChanged; // avoid race condition
            if (snapshotWasUpdated && programClassChanged != null)
            {
                programClassChanged(this, ProgramClassDocumentSnapshot.CurrentVersion);
            }
        }

        /// <summary>
        /// Last snapshot of the compilation unit viewed as a complete Cobol program or class, after parsing the code elements.
        /// Only one of the two properties Program or Class can be not null.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary> 
        public ProgramClassDocument ProgramClassDocumentSnapshot { get; private set; }

        /// <summary>
        /// Subscribe to this event to be notified of all changes in the complete program or class view of the document
        /// </summary>
        public event EventHandler<int> ProgramClassChanged;

        #region Thread ownership and synchronization

        // Synchronize accesses during snapshots updates
        protected readonly object lockObjectForCodeElementsDocumentSnapshot = new object();
        protected readonly object lockObjectForProgramClassDocumentSnapshot = new object();

        #endregion
    }
}
