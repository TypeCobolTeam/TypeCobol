using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Castle.Components.DictionaryAdapter;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Nodes;
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
        public CompilationUnit(TextSourceInfo textSourceInfo, IEnumerable<ITextLine> initialTextLines, TypeCobolOptions compilerOptions, IProcessedTokensDocumentProvider processedTokensDocumentProvider, List<RemarksDirective.TextNameVariation> copyTextNameVariations) :
            base(textSourceInfo, initialTextLines, compilerOptions, processedTokensDocumentProvider, copyTextNameVariations)
        {
            // Initialize performance stats 
            PerfStatsForCodeElementsParser = new PerfStatsForParsingStep(CompilationStep.CodeElementsParser);
            PerfStatsForProgramClassParser = new PerfStatsForParsingStep(CompilationStep.ProgramClassParser);

            
        }

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

                // Start perf measurement
                var perfStatsForParserInvocation = PerfStatsForCodeElementsParser.OnStartRefreshParsingStep();

                // Track all changes applied to the document while updating this snapshot
                DocumentChangedEvent<ICodeElementsLine> documentChangedEvent = null;

                // Apply text changes to the compilation document
                if (scanAllDocumentLines)
                {
                    if (processedTokensDocument != null)
                    {
                        // Parse the whole document for the first time
                        CodeElementsParserStep.ParseDocument(TextSourceInfo, ((ImmutableList<CodeElementsLine>)processedTokensDocument.Lines), CompilerOptions, perfStatsForParserInvocation);

                        // Create the first code elements document snapshot
                        CodeElementsDocumentSnapshot = new CodeElementsDocument(processedTokensDocument, new DocumentVersion<ICodeElementsLine>(this), ((ImmutableList<CodeElementsLine>)processedTokensDocument.Lines));
                    }
                }
                else
                {
                    ImmutableList<CodeElementsLine>.Builder codeElementsDocumentLines = ((ImmutableList<CodeElementsLine>)processedTokensDocument.Lines).ToBuilder();
                    IList<DocumentChange<ICodeElementsLine>> documentChanges = CodeElementsParserStep.ParseProcessedTokensLinesChanges(TextSourceInfo, codeElementsDocumentLines, processedTokensLineChanges, PrepareDocumentLineForUpdate, CompilerOptions, perfStatsForParserInvocation);

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

                // Stop perf measurement
                PerfStatsForCodeElementsParser.OnStopRefreshParsingStep();

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
        /// Performance stats for the RefreshCodeElementsDocumentSnapshot method
        /// </summary>
        public PerfStatsForParsingStep PerfStatsForCodeElementsParser { get; private set; }

        public string AntlrResult {
            get
            {
                StringBuilder builder = new StringBuilder();

                if (CodeElementsParserStep.AntlrPerformanceProfiler != null && PerfStatsForCodeElementsParser.ActivateDetailedAntlrPofiling)
                {
                    builder.Append("---CODE ELEMENT PARSER STEP---\n");
                    builder.Append(CodeElementsParserStep.AntlrPerformanceProfiler.WriteInfoToString());
                }

                if (ProgramClassParserStep.AntlrPerformanceProfiler != null && PerfStatsForProgramClassParser.ActivateDetailedAntlrPofiling)
                {
                    builder.Append("\n\n---PROGRAM CLASS PARSER STEP---\n");
                    builder.Append(ProgramClassParserStep.AntlrPerformanceProfiler.WriteInfoToString());
                }

                return builder.ToString();
            }
        }

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
                if ((CodeElementsDocumentSnapshot != null) && (ProgramClassDocumentSnapshot == null || ProgramClassDocumentSnapshot.PreviousStepSnapshot.CurrentVersion != codeElementsDocument.CurrentVersion))
                {
                    // Start perf measurement
                    var perfStatsForParserInvocation = PerfStatsForProgramClassParser.OnStartRefreshParsingStep();

                    // Program and Class parsing is not incremental : the objects are rebuilt each time this method is called
                    SourceFile root;
                    List<Diagnostic> newDiagnostics;
                    Dictionary<CodeElement, Node> nodeCodeElementLinkers = new Dictionary<CodeElement, Node>();
                    //TODO cast to ImmutableList<CodeElementsLine> sometimes fails here
                    ProgramClassParserStep.ParseProgramOrClass(TextSourceInfo, ((ImmutableList<CodeElementsLine>)codeElementsDocument.Lines), CompilerOptions, CustomSymbols, perfStatsForParserInvocation, out root, out newDiagnostics, out nodeCodeElementLinkers);
                
                    // Capture the result of the parse in a new snapshot
                    ProgramClassDocumentSnapshot = new ProgramClassDocument(
                        codeElementsDocument, ProgramClassDocumentSnapshot == null ? 0 : ProgramClassDocumentSnapshot.CurrentVersion + 1,
                        root, newDiagnostics, nodeCodeElementLinkers);
                    snapshotWasUpdated = true;

                    // Stop perf measurement
                    PerfStatsForProgramClassParser.OnStopRefreshParsingStep();
                }
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
        }

        /// <summary>
        /// Last snapshot of the compilation unit viewed as a complete Cobol program or class, after parsing the code elements.
        /// Only one of the two properties Program or Class can be not null.
        /// Tread-safe : accessible from any thread, returns an immutable object tree.
        /// </summary> 
        public ProgramClassDocument ProgramClassDocumentSnapshot { get; private set; }


        /// <summary>
        /// Return all diagnostics from all snaphost
        /// </summary>
        /// <returns></returns>
        public override IList<Diagnostic> AllDiagnostics()
        {
            var allDiagnostics = new List<Diagnostic>(base.AllDiagnostics());

            if (CodeElementsDocumentSnapshot != null && CodeElementsDocumentSnapshot.ParserDiagnostics != null)
            {
                allDiagnostics.AddRange(CodeElementsDocumentSnapshot.ParserDiagnostics);
            }

            if (ProgramClassDocumentSnapshot != null)
            {
                //Get all nodes diagnostics using visitor. 
                if (ProgramClassDocumentSnapshot.Root != null)
                    ProgramClassDocumentSnapshot.Root.AcceptASTVisitor(new DiagnosticsChecker(allDiagnostics));

                if (ProgramClassDocumentSnapshot.Diagnostics != null)
                    allDiagnostics.AddRange(ProgramClassDocumentSnapshot.Diagnostics);
            }

            if (CodeElementsDocumentSnapshot != null)
            {
                foreach (var ce in CodeElementsDocumentSnapshot.CodeElements)
                {
                    if (ce.Diagnostics != null)
                    {
                        allDiagnostics.AddRange(ce.Diagnostics);
                    }
                }
            }

            return allDiagnostics;
        }

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
        /// Performance stats for the RefreshProgramClassDocumentSnapshot method
        /// </summary>
        public PerfStatsForParsingStep PerfStatsForProgramClassParser { get; private set; }

        #region Thread ownership and synchronization

        // Synchronize accesses during snapshots updates
        protected readonly object lockObjectForCodeElementsDocumentSnapshot = new object();
        protected readonly object lockObjectForProgramClassDocumentSnapshot = new object();

        #endregion
    }

    public class ProgramClassEvent : EventArgs
    {
        public int Version { get; set; }
    }
}
