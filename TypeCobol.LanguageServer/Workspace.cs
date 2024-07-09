using System.Text;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.Context;
using TypeCobol.Tools.Options_Config;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.Logging;
using TypeCobol.Tools;
using System.Collections.Concurrent;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
#if EUROINFO_RULES
using System.Text.RegularExpressions;
using TypeCobol.Compiler.Preprocessor;
#endif

namespace TypeCobol.LanguageServer
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
        public SymbolTable CustomSymbols { get; private set; }
        public string RootDirectory { get; }
        public string Name { get; }
        private DependenciesFileWatcher _DepWatcher;
        private CopyWatcher _CopyWatcher;
        private System.Timers.Timer _semanticUpdaterTimer;
        private bool _timerDisabled;

        private List<FileCompiler> _fileCompilerWaitingForNodePhase;
        public TypeCobolConfiguration Configuration { get; private set; }
        public event EventHandler<DiagnosticEvent> DiagnosticsEvent;
        public event EventHandler<EventArgs> DocumentModifiedEvent;
        public event EventHandler<MissingCopiesEvent> MissingCopiesEvent;
        public event EventHandler<LoadingIssueEvent> LoadingIssueEvent;
        public event EventHandler<ThreadExceptionEventArgs> ExceptionTriggered;
        public event EventHandler<string> WarningTrigger;
        public System.Collections.Concurrent.ConcurrentQueue<MessageActionWrapper> MessagesActionsQueue { get; private set; }
        private Func<string, Uri, bool> _Logger;       
        /// <summary>
        /// Custom Analyzer Providers Loaded
        /// </summary>
        private IAnalyzerProvider[] _customAnalyzerProviders;
        /// <summary>
        /// Storage of all active workspace projects
        /// </summary>
        internal WorkspaceProjectStore WorkspaceProjectStore { get; }
        /// <summary>
        /// A Dictionary to associate an Uri of a Document to its Context.
        /// it represents all documents belonging to all workspace projects.
        /// </summary>
        private readonly ConcurrentDictionary<Uri, DocumentContext> _allOpenedDocuments;


        #region Testing Options

        /// <summary>
        /// Timer Disabled for TypeCobol.LanguageServer.
        /// </summary>
        public bool TimerDisabledOption
        {
            get
            {
                return _timerDisabled;
            }
            set
            {
                _timerDisabled = value;
            }
        }

        

        /// <summary>
        /// Lsr Test Options.
        /// </summary>
        public LsrTestingOptions LsrTestOptions { get; set; }

        /// <summary>
        /// LSR always test the source code at least
        /// </summary>
        public bool IsLsrSourceTesting => LsrTestOptions.HasFlag(LsrTestingOptions.LsrSourceDocumentTesting);

        /// <summary>
        /// LSR testing the Source document and scanning phase ?
        /// </summary>
        public bool IsLsrScannerTesting => LsrTestOptions.HasFlag(LsrTestingOptions.LsrScanningPhaseTesting);

        /// <summary>
        /// LSR testing the Source document, scanning phase, and preprocessing phase ?
        /// </summary>
        public bool IsLsrPreprocessingTesting => LsrTestOptions.HasFlag(LsrTestingOptions.LsrPreprocessingPhaseTesting);

        /// <summary>
        /// Testing the Source document, the scanning phase and code elements parsing phase.
        /// </summary>
        public bool IsLsrParserTesting => LsrTestOptions.HasFlag(LsrTestingOptions.LsrParsingPhaseTesting);

        /// <summary>
        /// Testing the Source document the scanning phase, the code elements parsing phase and the semantic analysis phase.
        /// </summary>
        public bool IsLsrSemanticTesting => LsrTestOptions.HasFlag(LsrTestingOptions.LsrSemanticPhaseTesting);

        /// <summary>
        /// Testing everything from source document to quality check using quality rules.
        /// </summary>
        public bool IsLsrCodeAnalysisTesting => LsrTestOptions.HasFlag(LsrTestingOptions.LsrCodeAnalysisPhaseTesting);
        #endregion

        /// <summary>
        /// True to use ANTLR for parsing a program
        /// </summary>
        public bool UseAntlrProgramParsing { get; set; }

        /// <summary>
        /// True to use EI Legacy automatic removal of first 01 level from CPY copy.
        /// </summary>
        public bool EILegacy_RemoveFirst01Level { get; set; }

        /// <summary>
        /// True to use EI Legacy copy suffixing mechanism
        /// </summary>
        public bool EILegacy_ApplyCopySuffixing { get; set; }

        /// <summary>
        /// Are we supporting Syntax Coloring Notifications.
        /// </summary>
        public bool UseSyntaxColoring { get; set; }

        /// <summary>
        /// Are we using the CFG view in the client.
        /// </summary>
        public bool UseCfgDfaDataRefresh { get; set; }

#if EUROINFO_RULES
        /// <summary>
        /// The Cpy Copy names file
        /// </summary>
        public string CpyCopyNamesMapFilePath { get; set; }
#endif

        public static readonly string DefaultCopyFolder;

        static Workspace()
        {
            var folder = Path.GetDirectoryName(System.Diagnostics.Process.GetCurrentProcess().MainModule.FileName);
            DefaultCopyFolder = folder + @"\DefaultCopies\";
        }

        public Workspace(string rootDirectoryFullName, string workspaceName, System.Collections.Concurrent.ConcurrentQueue<MessageActionWrapper> messagesActionsQueue, Func<string, Uri, bool> logger)
        {
            this._allOpenedDocuments = new ConcurrentDictionary<Uri, DocumentContext>();
            MessagesActionsQueue = messagesActionsQueue;
            _fileCompilerWaitingForNodePhase = new List<FileCompiler>();
            _Logger = logger;
            RootDirectory = rootDirectoryFullName;
            Name = workspaceName;

            this.Configuration = new TypeCobolConfiguration();
            WorkspaceProjectStore = new WorkspaceProjectStore(this);
        }

        internal void InitCopyDependencyWatchers()
        {
            _DepWatcher = new DependenciesFileWatcher(this, RefreshAllOpenedFiles);
            _CopyWatcher = new CopyWatcher(this, RefreshAllOpenedFiles);

            // Refresh all opened files in all WorkspaceProject instances
            void RefreshAllOpenedFiles()
            {
                RefreshCustomSymbols();
                foreach (var workspaceProject in WorkspaceProjectStore.AllProjects)
                {
                    workspaceProject.RefreshOpenedDocuments();
                }
            }
        }

        /// <summary>
        /// Start continuous background compilation on a newly opened file
        /// </summary>
        /// <param name="docContext">The Document context</param>
        /// <param name="sourceText">The source text</param>
        /// <param name="projectKey">Project's Key</param>
        /// <param name="copyFolders">List of copy folders associated to the project</param>
        /// <returns>The corresponding FileCompiler instance.</returns>
        public void OpenTextDocument(DocumentContext docContext, string sourceText, string projectKey, List<string> copyFolders) => OpenTextDocument(docContext, sourceText, projectKey, copyFolders, LsrTestOptions);

        /// <summary>
        /// Bind a document to a FileCompiler instance and update its content for a parsing..
        /// </summary>
        /// <param name="docContext">The document context to bind</param>
        /// <param name="sourceText">The source text of the document</param>
        /// <param name="lsrOptions">LSR options</param>
        /// <returns>The FileCompiler instance associated to the document context</returns>
        internal void BindFileCompilerSourceTextDocument(DocumentContext docContext, string sourceText, LsrTestingOptions lsrOptions)
        {
            System.Diagnostics.Debug.Assert(docContext.Project != null);
            //The document was already there ==> Stop any pending background compilation
            StopDocumentBackgroundCompilation(docContext);
            CompilationProject compilationProject = docContext.Project.Project;
            string fileName = Path.GetFileName(docContext.Uri.LocalPath);
            var encodingForAlphanumericLiterals = compilationProject.CompilationOptions.GetEncodingForAlphanumericLiterals();
            ITextDocument initialTextDocumentLines = new ReadOnlyTextDocument(fileName, encodingForAlphanumericLiterals,
                Configuration.Format.ColumnsLayout, docContext.IsCopy, sourceText);
            FileCompiler fileCompiler = null;

#if EUROINFO_RULES //Issue #583
            SymbolTable arrangedCustomSymbol = null;
            string inputFileName = string.Empty;

            using (var reader = new StringReader(sourceText))
            {
                string line;
                while ((line = reader.ReadLine()) != null)
                {
                    if (!line.StartsWith("       PROGRAM-ID.", StringComparison.OrdinalIgnoreCase)) continue;
                    inputFileName = line.Split('.')[1].Trim();
                    break;
                }
            }

            var matchingPgm = CustomSymbols?.Programs.Keys.FirstOrDefault(k => k.Equals(inputFileName, StringComparison.OrdinalIgnoreCase));
            if (matchingPgm != null)
            {
                arrangedCustomSymbol = new SymbolTable(CustomSymbols, SymbolTable.Scope.Namespace);
                var prog = CustomSymbols.Programs.Values.SelectMany(p => p).Where(p => p.Name != matchingPgm);
                arrangedCustomSymbol.CopyAllPrograms(new List<List<Program>>() { prog.ToList() });
                arrangedCustomSymbol.Programs.Remove(matchingPgm);
            }

            fileCompiler = new FileCompiler(initialTextDocumentLines, compilationProject.SourceFileProvider,
                compilationProject, compilationProject.CompilationOptions, arrangedCustomSymbol ?? CustomSymbols,
                compilationProject);
#else
            fileCompiler = new FileCompiler(initialTextDocumentLines, compilationProject.SourceFileProvider, compilationProject, compilationProject.CompilationOptions, CustomSymbols, compilationProject);
#endif

            // Set Any Language Server Connection Options.
            docContext.FileCompiler = fileCompiler;
            docContext.LanguageServerConnection(true);

            fileCompiler.CompilationResultsForProgram.SetOwnerThread(Thread.CurrentThread);
            fileCompiler.CompilationResultsForProgram.CodeAnalysisCompleted += FinalCompilationStepCompleted;
            fileCompiler.CompilationResultsForProgram.UpdateTokensLines();

            if (lsrOptions != LsrTestingOptions.LsrSourceDocumentTesting)
            {
                fileCompiler.CompileOnce(lsrOptions.ExecutionStep(fileCompiler.CompilerOptions.ExecToStep.Value), fileCompiler.CompilerOptions.HaltOnMissingCopy); //Let's parse file for the first time after opening. 
            }
        }

        /// <summary>
        /// Open a Text Document.
        /// </summary>
        /// <param name="docContext">The Document Context</param>
        /// <param name="sourceText">The source text</param>
        /// <param name="projectKey">Project's Key</param>        
        /// <param name="lsrOptions">LSR testing options</param>
        /// <returns></returns>
        private void OpenTextDocument(DocumentContext docContext, 
            string sourceText, string projectKey,
            List<string> copyFolders, LsrTestingOptions lsrOptions)
        {
            System.Diagnostics.Debug.Assert(docContext.Project == null);
            var workspaceProject = WorkspaceProjectStore.GetOrCreateProject(projectKey);
            workspaceProject.Configure(null, null, null, copyFolders);
            workspaceProject.AddDocument(docContext);
            this._allOpenedDocuments.TryAdd(docContext.Uri, docContext);
            docContext.Project = workspaceProject;
            BindFileCompilerSourceTextDocument(docContext, sourceText, lsrOptions);
        }

        /// <summary>
        /// Load Custom Analyzers
        /// </summary>
        /// <param name="extensionManager"></param>
        internal void LoadCustomAnalyzers(ExtensionManager extensionManager)
        {
            System.Diagnostics.Debug.Assert(extensionManager != null);
            this._customAnalyzerProviders = extensionManager.Activate<IAnalyzerProvider>().ToArray();
        }

        /// <summary>
        /// Try to get the DocumentContext instance associated to an opened document from its Uri
        /// </summary>
        /// <param name="docUri">The Document's uri to get the associated DocumentContext instance.</param>
        /// <param name="openedDocumentContext">[out] The DocumentContext instance of the opened document if any, null otherwise</param>
        /// <returns>True if the uri was associated to an opened document, false otherwise</returns>
        internal bool TryGetOpenedDocument(Uri docUri, out DocumentContext openedDocumentContext)
        {
            return this._allOpenedDocuments.TryGetValue(docUri, out openedDocumentContext);
        }

        /// <summary>
        /// Try to get the DocumentContext instance by name
        /// </summary>
        /// <param name="name">The document's name</param>
        /// <param name="openedDocumentContext"></param>
        /// <returns>true if the DocumentContext has been found, false otherwise</returns>
        private bool TryGetOpenedDocumentByName(string name, out DocumentContext openedDocumentContext)
        {
            openedDocumentContext = null;
            foreach (var entry in this._allOpenedDocuments)
            {
                if (entry.Key.LocalPath.Contains(name))
                {
                    openedDocumentContext = entry.Value;
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Update the text contents of the file
        /// </summary>
        public void UpdateSourceFile(Uri fileUri, RangeUpdate[] updates)
        {
            if (TryGetOpenedDocument(fileUri, out var contextToUpdate))
            {
                FileCompiler fileCompilerToUpdate = contextToUpdate.FileCompiler;
                _semanticUpdaterTimer?.Stop();

                fileCompilerToUpdate.CompilationResultsForProgram.UpdateTextLines(updates);
                if (IsLsrSourceTesting)
                {
                    //Log text lines string 
                    var sb = new StringBuilder();
                    foreach (var cobolTextLine in fileCompilerToUpdate.CompilationResultsForProgram.CobolTextLines)
                        sb.AppendLine(cobolTextLine.SourceText);
                    _Logger(sb.ToString(), fileUri);
                }
                
                var handler = new Action<object, ExecutionStepEventArgs>((sender, args) => { ExecutionStepEventHandler(sender, args, fileUri); });
                //Subscribe to FileCompilerEvent 
                fileCompilerToUpdate.ExecutionStepEventHandler += handler.Invoke;
                var execStep = LsrTestOptions.ExecutionStep(fileCompilerToUpdate.CompilerOptions.ExecToStep);
                if (execStep > ExecutionStep.CodeElement)
                    execStep = ExecutionStep.CodeElement; //The maximum execstep authorize for incremental parsing is CodeElement, 
                                                          //further it's for semantic, which is handle by NodeRefresh method


                fileCompilerToUpdate.CompileOnce(execStep, fileCompilerToUpdate.CompilerOptions.HaltOnMissingCopy);
                fileCompilerToUpdate.ExecutionStepEventHandler -= handler.Invoke;
                

                if (LsrTestOptions == LsrTestingOptions.NoLsrTesting || IsLsrSemanticTesting)
                {
                    if (!_timerDisabled) //If TimerDisabled is false, create a timer to automatically launch Node phase
                    {
                        lock (_fileCompilerWaitingForNodePhase)
                        {
                            if (!_fileCompilerWaitingForNodePhase.Contains(fileCompilerToUpdate))
                                _fileCompilerWaitingForNodePhase.Add(fileCompilerToUpdate); //Store that this fileCompiler will soon need a Node Phase
                        }

                        _semanticUpdaterTimer = new System.Timers.Timer(750);
                        _semanticUpdaterTimer.Elapsed += (sender, e) => TimerEvent(fileUri);
                        _semanticUpdaterTimer.Start();
                    }
                }
            }
        }

        private void ExecutionStepEventHandler(object oFileCompiler, ExecutionStepEventArgs executionStepEvent, Uri fileUri)
        {
            if (!(oFileCompiler is FileCompiler))
                return;

            var fileCompiler = (FileCompiler) oFileCompiler;
            switch (executionStepEvent.ExecutionStep)
            {
                case ExecutionStep.Scanner:
                    if (IsLsrScannerTesting)
                    {
                        //Return log information about updated tokens
                        var sb = new StringBuilder();
                        foreach (var token in fileCompiler.CompilationResultsForProgram.TokensDocumentSnapshot.SourceTokens)
                            sb.AppendLine(token.ToString());
                        _Logger(sb.ToString(), fileUri);
                    }
                    break;
                case ExecutionStep.Preprocessor:
                    if (IsLsrPreprocessingTesting)
                    {
                        //Return log information about updated processed tokens
                        var sb = new StringBuilder();
                        foreach (var token in fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot.GetProcessedTokens())
                            sb.AppendLine(token.ToString());
                        _Logger(sb.ToString(), fileUri);
                    }
                    break;
                case ExecutionStep.CodeElement:
                    if (IsLsrParserTesting)
                    {
                        //Return log information about code elements
                        var sb = new StringBuilder();
                        foreach (var codeElement in fileCompiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.CodeElements)
                            sb.AppendLine(codeElement.ToString());
                        _Logger(sb.ToString(), fileUri);
                    }
                    break;
                case ExecutionStep.AST:
                case ExecutionStep.SemanticCrossCheck:
                case ExecutionStep.CodeAnalysis:
                case ExecutionStep.Generate:
                default:
                    return;
            }
        }

        /// <summary>
        /// Event method called when the timer reach the Elapsed time
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="eventArgs"></param>
        /// <param name="fileCompiler"></param>
        private void TimerEvent(Uri fileUri)
        {
            try
            {
                _semanticUpdaterTimer.Stop();
                MessagesActionsQueue.Enqueue(new MessageActionWrapper(Refresh));
            }
            catch (Exception e)
            {
                //In case Timer Thread crash
                if (ExceptionTriggered != null)
                    ExceptionTriggered(null, new ThreadExceptionEventArgs(e));
            }

            void Refresh()
            {
                if (TryGetOpenedDocument(fileUri, out var docContext))
                {
                    RefreshSyntaxTree(docContext.FileCompiler, SyntaxTreeRefreshLevel.RebuildNodesAndPerformCodeAnalysis);
                }
            }
        }

        /// <summary>
        /// Lists all supported refresh modes for the RefreshSyntaxTree method.
        /// </summary>
        public enum SyntaxTreeRefreshLevel
        {
            /// <summary>
            /// Do not perform any refresh
            /// </summary>
            NoRefresh,

            /// <summary>
            /// Rebuilds semantic document and run SemanticCrossCheck on updated version
            /// </summary>
            RebuildNodes,

            /// <summary>
            /// Same as RebuildNodes but also launches code quality analysis
            /// </summary>
            RebuildNodesAndPerformCodeAnalysis,

            /// <summary>
            /// Rebuild nodes and run code quality analysis even if the file hasn't changed
            /// </summary>
            ForceFullRefresh
        }

        /// <summary>
        /// Use this method to force a node phase if there is a filecompiler waiting for node refresh. 
        /// </summary>
        /// <param name="fileCompiler">FileCompiler on which the node phase will be done</param>
        /// <param name="refreshLevel">Desired level of refresh</param>
        public void RefreshSyntaxTree(FileCompiler fileCompiler, SyntaxTreeRefreshLevel refreshLevel)
        {
            if (refreshLevel == SyntaxTreeRefreshLevel.NoRefresh) return; //nothing to do

            lock (_fileCompilerWaitingForNodePhase)
            {
                var fileCompilerNeedsRefresh = _fileCompilerWaitingForNodePhase.Contains(fileCompiler);
                if (fileCompilerNeedsRefresh)
                {
                    _fileCompilerWaitingForNodePhase.Remove(fileCompiler);
                }
                else
                {
                    if (refreshLevel < SyntaxTreeRefreshLevel.ForceFullRefresh)
                    {
                        //The file compiler does not need to be refreshed and the refresh was not forced, we abort
                        return;
                    }
                }
                
                //Perform refresh according to desired level
                switch (refreshLevel)
                {
                    case SyntaxTreeRefreshLevel.RebuildNodes:
                        RefreshNodes();
                        break;
                    case SyntaxTreeRefreshLevel.RebuildNodesAndPerformCodeAnalysis:
                    case SyntaxTreeRefreshLevel.ForceFullRefresh:
                        RefreshNodes();
                        RefreshCodeAnalysisResults();
                        break;
                }
            }

            void RefreshNodes()
            {
                if (LsrTestOptions != LsrTestingOptions.NoLsrTesting && !IsLsrSemanticTesting) return;
                fileCompiler.CompilationResultsForProgram.ProduceTemporarySemanticDocument(); //Produce the temporary snapshot before full cross check
                fileCompiler.CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot(); //Do a Node phase
            }

            void RefreshCodeAnalysisResults()
            {
                if (LsrTestOptions != LsrTestingOptions.NoLsrTesting && !IsLsrCodeAnalysisTesting) return;
                fileCompiler.CompilationResultsForProgram.RefreshCodeAnalysisDocumentSnapshot(); //Do a Quality check
            }
        }

        /// <summary>
        /// Try to close the document corresponding to the given uri.
        /// The document is removed from the workspace store and from its owner workspace project.
        /// Any pending background compilation will be stopped.
        /// </summary>
        /// <param name="fileUri">The Uri of the document to be closed</param>
        /// <returns>True if the document has been closed, false if the document is unknown</returns>
        public bool TryCloseSourceFile(Uri fileUri)
        {
            // Remove from opened documents store and target workspace project.
            DocumentContext contextToClose = RemoveDocumentFromUri(fileUri);
            if (contextToClose != null)
            {
                // Stop any pending compilation on the document.
                StopDocumentBackgroundCompilation(contextToClose);
                return true;
            }
            return false;
        }

        /// <summary>
        /// Remove a document from it's uri
        /// </summary>
        /// <param name="docUri">The document's uri</param>
        /// <returns>The removed document if any, null if no document was removed</returns>
        private DocumentContext RemoveDocumentFromUri(Uri docUri)
        {
            System.Diagnostics.Debug.Assert(docUri != null);
            // Now check if it was added in a document.
            if (TryGetOpenedDocument(docUri, out DocumentContext wksPrjDocContext))
            {                
                return CloseTextDocument(wksPrjDocContext) ? wksPrjDocContext : null;
            }
            else
            {
                System.Diagnostics.Debug.Fail("Document was not added in the workspace : " + docUri.ToString());
            }
            return null;
        }

        /// <summary>
        /// Close a DocumentContext instance
        /// </summary>
        /// <param name="documentContext">The DocumentContext to be closed</param>
        private bool CloseTextDocument(DocumentContext documentContext)
        {
            var workspaceProject = documentContext.Project;
            // Remove the document from the global set.
            bool bRemoved = _allOpenedDocuments.TryRemove(documentContext.Uri, out var storedDocument );
            System.Diagnostics.Debug.Assert(storedDocument == documentContext);
            if (workspaceProject != null)
            {
                documentContext.Project = null;
                if (workspaceProject.RemoveDocument(documentContext))
                {
                    if (workspaceProject.IsEmpty)
                    {   // Remove the project if it is empty
                        this.WorkspaceProjectStore.RemoveProject(workspaceProject);
                    }
                    return true;
                }
                else
                {
                    System.Diagnostics.Debug.Fail("Document was not added in the workspace project : " + documentContext.Uri.ToString());
                }
            }
            else 
            {
                if (!bRemoved)
                    System.Diagnostics.Debug.Fail("Document was not opened or is already closed : " + documentContext.Uri.ToString());
                else
                    System.Diagnostics.Debug.Fail("Document is not associated to a Workspace project : " + documentContext.Uri.ToString());
            }
            return false;
        }

        /// <summary>
        /// Stop continuous background compilation of a document.
        /// </summary>
        /// <param name="contextToStop">The Document Context to stop background compilation</param>
        /// <returns>True if a background compilation was stopped, false otherwise</returns>
        private bool StopDocumentBackgroundCompilation(DocumentContext contextToStop)
        {
            if (contextToStop != null)
            {
                FileCompiler fileCompilerToClose = contextToStop.FileCompiler;
                if (fileCompilerToClose != null)
                {                    
                    lock (_fileCompilerWaitingForNodePhase)
                    {
                        return _fileCompilerWaitingForNodePhase.Remove(fileCompilerToClose);
                    }
                }
            }
            return false;
        }


        /// <summary>
        /// Handle the Configuration change notification.
        /// </summary>
        /// <param name="arguments">The arguments</param>
        public void DidChangeConfigurationParams(string[] arguments)
        {
#if EUROINFO_RULES
            var previouslyLoadedCpyCopyNamesMap = Configuration.CpyCopyNameMap;
#endif

            Configuration = new TypeCobolConfiguration();
            var options = TypeCobolOptionSet.GetCommonTypeCobolOptions(Configuration);

            TypeCobolOptionSet.InitializeCobolOptions(Configuration, arguments, options);

            //Adding default copies folder
            Configuration.CopyFolders.Add(DefaultCopyFolder);

            if (Configuration.UseAntlrProgramParsing)
                UseAntlrProgramParsing = true;

            if (Configuration.EILegacy_RemoveFirst01Level)
                EILegacy_RemoveFirst01Level = true;

            if (Configuration.EILegacy_ApplyCopySuffixing)
                EILegacy_ApplyCopySuffixing = true;

            if (Configuration.ExecToStep >= ExecutionStep.Generate)
                Configuration.ExecToStep = ExecutionStep.CodeAnalysis; //Language Server does not support Cobol Generation for now

#if EUROINFO_RULES
            if (previouslyLoadedCpyCopyNamesMap != null)
            {
                //re-use already loaded file
                Configuration.CpyCopyNameMap = previouslyLoadedCpyCopyNamesMap;
            }
            else
            {
                //load file according to user-supplied value in command line
                Configuration.LoadCpyCopyNameMap(CpyCopyNamesMapFilePath);
            }
#endif
            var typeCobolOptions = new TypeCobolOptions(Configuration);

            //Configure CFG/DFA analyzer(s) + external analyzers if any
            var analyzerProviderWrapper = new AnalyzerProviderWrapper(str => _Logger(str, null));
            analyzerProviderWrapper.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer(Configuration.CfgBuildingMode, o));
            if (UseCfgDfaDataRefresh && Configuration.CfgBuildingMode != CfgBuildingMode.Standard)
            {
                analyzerProviderWrapper.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer(CfgBuildingMode.Standard, o));
            }
            System.Diagnostics.Debug.Assert(this._customAnalyzerProviders != null);
            foreach (var a in this._customAnalyzerProviders)
            {
                analyzerProviderWrapper.AddProvider(a);
            }

            RefreshCustomSymbols();

            // DidChangeConfiguration notification is always for the Default Workspace Project.
            // This is for compatibility with for instance TypeCobol plugin and fallback.
            this.WorkspaceProjectStore.DefaultWorkspaceProject.Configure(Configuration.Format, typeCobolOptions, analyzerProviderWrapper, Configuration.CopyFolders);

            // Update All other WorkspaceProject instances because global configurations have changed
            foreach (var workspaceProject in this.WorkspaceProjectStore.NamedProjects)
            {
                // Passing null as CopyFolders will force to reuse the same set of Copy folders.
                workspaceProject.Configure(Configuration.Format, typeCobolOptions, analyzerProviderWrapper, null);
            }            

            //Dispose previous watchers before setting new ones            
            if (_CopyWatcher != null)
            {
                _CopyWatcher.Dispose();
                foreach (var copyFolder in Configuration.CopyFolders)
                {
                    _CopyWatcher.SetDirectoryWatcher(copyFolder);
                }
            }
            if (_DepWatcher != null)
            {
                _DepWatcher.Dispose();
                foreach (var depFolder in Configuration.Dependencies)
                {
                    _DepWatcher.SetDirectoryWatcher(depFolder);
                }
                foreach (var intrinsicFolder in Configuration.Copies)
                {
                    _DepWatcher.SetDirectoryWatcher(intrinsicFolder);
                }
            }
        }

        /// <summary>
        /// Handle a Did Change Project Configuration notification from the client.
        /// </summary>
        /// <param name="projectKey">The project key whose configuration has changed</param>
        /// <param name="copyFolders">List of Copy Folders to be associated with the underlying CompilationProject instance.</param>
        internal void UpdateWorkspaceProjectConfiguration(string projectKey, List<string> copyFolders)
        {
            var workspaceProject = this.WorkspaceProjectStore.GetOrCreateProject(projectKey);
            workspaceProject.Configure(null, null, null, copyFolders);
        }

        /// <summary>
        /// The method is called in response to a MissingCopyNotification from the client.
        /// The RemainingMissingCopies list can contain a list of COPY that the client fails to load,
        /// or an empty list if all COPY have been loaded.
        /// </summary>
        /// <param name="fileUri">Uri of the document from which the response is emitted</param>
        /// <param name="remainingMissingCopies">The list of unloaded COPY if any, an empty list otherwise</param>
        public void UpdateMissingCopies(Uri fileUri, List<string> remainingMissingCopies)
        {
            /*
             * TODO Keeping the current behavior here (clear cache + refresh project documents)
             * But this should be converted into didChangeWatchedFiles notification instead
             * so the client can signal that it finished loading some copies.
             *
             * Then the MissingCopiesNotification from client to server could be fully removed.
             */

            if (_CopyWatcher == null)
            {
                // No Copy Watcher ==> Refresh opened files ourselves.
                if (this.TryGetOpenedDocument(fileUri, out var docContext))
                {
                    System.Diagnostics.Debug.Assert(docContext.Project != null);
                    docContext.Project.Project.CopyCache.Clear();
                    docContext.Project.RefreshOpenedDocuments();
                }
            }
        }

        /// <summary>
        /// Consider external changes that happened on copy files.
        /// The opened documents are refreshed depending on whether they use the modified copies or not.
        /// </summary>
        /// <param name="changedCopies">List of modified copies.</param>
        internal void AcknowledgeCopyChanges(List<ChangedCopy> changedCopies)
        {
            // Group changed copies by target copy cache
            var evictions = WorkspaceProjectStore.AllProjects.ToDictionary(wp => wp.Project.CopyCache, wp => new List<string>());
            foreach (var changedCopy in changedCopies)
            {
                if (changedCopy.ClearAllCaches)
                {
                    // Evict from all caches
                    foreach (var toEvict in evictions.Values)
                    {
                        toEvict.Add(changedCopy.CopyName);
                    }
                }
                else if (WorkspaceProjectStore.TryGetProject(changedCopy.OwnerProject, out var workspaceProject))
                {
                    // Evict from target project cache only
                    evictions[workspaceProject.Project.CopyCache].Add(changedCopy.CopyName);
                }
                else
                {
                    // Inconsistent notification from client, the target project could not be found
                    LoggingSystem.LogMessage(LogLevel.Warning, $"Copy to WorkspaceProject mismatch: could not find project '{changedCopy.OwnerProject}'.");
                }
            }

            // Remove obsolete data from caches
            var evicted = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            foreach (var eviction in evictions)
            {
                eviction.Key.Evict(eviction.Value, evicted);
            }

            // Find programs depending on the obsolete copies
            var dependentPrograms = new List<DocumentContext>();
            foreach (var openedDocument in _allOpenedDocuments.Values)
            {
                var usedCopies = openedDocument.FileCompiler?.CompilationResultsForProgram?.CopyTextNamesVariations;
                if (usedCopies == null || usedCopies.Count == 0) continue;

                foreach (var usedCopy in usedCopies)
                {
                    string fileName = usedCopy.GetFileName(openedDocument.FileCompiler.CompilerOptions);
                    if (evicted.Contains(fileName))
                    {
                        dependentPrograms.Add(openedDocument);
                        break;
                    }
                }
            }

            //Refresh all dependent programs
            foreach (var dependentProgram in dependentPrograms)
            {
                RefreshOpenedDocument(dependentProgram);
            }
        }

        /// <summary>
        /// Refresh One opened document
        /// </summary>
        /// <param name="docContext">The Document context to be refreshed</param>
        internal void RefreshOpenedDocument(DocumentContext docContext)
        {
            //Read latest version of source text from CompilationResults
            var sourceText = new StringBuilder();
            var compilationResults = docContext.FileCompiler?.CompilationResultsForProgram;
            if (compilationResults == null)
                return;
            foreach (var cobolTextLine in compilationResults.CobolTextLines)
                sourceText.AppendLine(cobolTextLine.Text);

            //Disconnect previous LanguageServer connection
            docContext.LanguageServerConnection(false);

            //Bind to new FileCompiler
            BindFileCompilerSourceTextDocument(docContext, sourceText.ToString(), LsrTestingOptions.NoLsrTesting);
        }

        private void RefreshCustomSymbols()
        {
            bool diagDetected = false;
            Dictionary<string, List<string>> detectedDiagnostics = new Dictionary<string, List<string>>();
            EventHandler<Tools.APIHelpers.DiagnosticsErrorEvent> DiagnosticsErrorEvent = null;
            DiagnosticsErrorEvent += delegate (object sender, Tools.APIHelpers.DiagnosticsErrorEvent diagEvent)
            {
                //Delegate Event to handle diagnostics generated while loading dependencies/intrinsics
                if (diagEvent.Diagnostic.Info.Severity == Severity.Error)
                {
                    diagDetected = true;
                    if (detectedDiagnostics.ContainsKey(diagEvent.Path))
                        detectedDiagnostics[diagEvent.Path].Add(diagEvent.Diagnostic.ToString());
                    else
                        detectedDiagnostics.Add(diagEvent.Path, new List<string>() { diagEvent.Diagnostic.ToString() });
                }
                    
            };
            CustomSymbols = null;
            try
            {
                CustomSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(Configuration.Copies, Configuration.Format, DiagnosticsErrorEvent); //Refresh Intrinsics
                CustomSymbols = Tools.APIHelpers.Helpers.LoadDependencies(Configuration, CustomSymbols, DiagnosticsErrorEvent, out _, out IDictionary<string, IEnumerable<string>> missingCopies); //Refresh Dependencies

                if (MissingCopiesEvent != null && missingCopies.Count > 0)
                {
                    MissingCopiesEvent(missingCopies.First().Key, new MissingCopiesEvent() { Copies = missingCopies.SelectMany(c => c.Value).Distinct().ToList() });
                    return;//Do not report diagnostics if copies are missing
                }

                if (diagDetected)
                {
                    var message = "An error occured while trying to load Intrinsics or Dependencies files.";
                    if (LoadingIssueEvent!= null)
                        LoadingIssueEvent(null, new LoadingIssueEvent() {Message = message}); //Send notification to client

                    var sb = new StringBuilder();
                    sb.AppendLine(message);
                    foreach (var dicItem in detectedDiagnostics)
                    {
                        sb.AppendLine("");
                        sb.AppendLine(dicItem.Key); //Add file path 
                        foreach (var diagText in dicItem.Value)
                        {
                            sb.AppendLine(" - " + diagText); //Add associated diagnostics
                        }
                    }
                    if (WarningTrigger != null)
                        WarningTrigger(null, sb.ToString()); //Send warning notification to display info to the user. 
                }
                else
                {//Send an LoadingIssueEvent with an empty message to tell the client that there are no issues.
                    if (LoadingIssueEvent != null)
                        LoadingIssueEvent(null, new LoadingIssueEvent() { Message = "" });
                }
            }
            catch (Exception exception)
            {
                if (LoadingIssueEvent != null)
                    LoadingIssueEvent(null, new LoadingIssueEvent() { Message = "An error occured while trying to load Intrinsics or Dependencies files." }); //Send notification to client

                LoggingSystem.LogException(exception);
            }
        }

        /// <summary>
        /// CodeAnalysis completion event handler.
        /// </summary>
        /// <param name="cUnit">Sender of the event is the CompilationUnit.</param>
        /// <param name="programEvent">Event arg, contains the version number of the most up-to-date InspectedProgramClassDocument.</param>
        private void FinalCompilationStepCompleted(object cUnit, ProgramClassEvent programEvent)
        {
            var compilationUnit = cUnit as CompilationUnit;

            // Search for corresponding opened document.
            Uri fileUri = null;
            if (TryGetOpenedDocumentByName(compilationUnit.TextSourceInfo.Name, out var documentCtx))
            {
                fileUri = documentCtx.Uri;
            }

            // No document found
            if (fileUri == null)  return;

            // Need to handle the groups instead of the diagnostics, so the order of appearance will be the same within the groups
            // This is meant to avoid the modification of the LSR tests
            IEnumerable<Diagnostic> diags = new List<Diagnostic>();
            var severityGroups = compilationUnit?.AllDiagnostics().GroupBy(d => d.Info.Severity);

            if (severityGroups != null)
            {
                foreach (var group in severityGroups.OrderBy(d => d.Key))
                {
                    // Add Errors first, then Warnings, then Infos
                    diags = diags.Any() ? diags.Concat(group) : group;
                }
            }

            if (DiagnosticsEvent != null)
                DiagnosticsEvent(fileUri, new DiagnosticEvent() { Diagnostics = diags.Take(Configuration.MaximumDiagnostics == 0 
                    ? 200 : Configuration.MaximumDiagnostics) });

            if (MissingCopiesEvent != null && compilationUnit?.MissingCopies.Count > 0)
                MissingCopiesEvent(fileUri, new MissingCopiesEvent() { Copies = new List<string>(compilationUnit.MissingCopies) });

            DocumentModifiedEvent?.Invoke(fileUri, new EventArgs());
        }

        #region Data Layout
        private const char SPACE = ' ';
        private const string UNDEFINED = "***";
        private const string FILLER = "FILLER";
        private const string GROUP = "GROUP";
        private const string OCCURS = "OCCURS {0}";
        private const string OCCURS_SUFFIX_START = " (";
        private const string OCCURS_SUFFIX = "1";
        private const string OCCURS_SUFFIX_END = ")";
        private const string PIC = "PIC {0}";
        private const string REDEFINES = "REDEFINES {0}";

        /// <summary>
        /// Get the Data Layout rows for a Program or a Copy (output = CSV)
        /// </summary>
        /// <param name="compilationUnit">Compilation unit resulting from parsing the Program/Copy</param>
        /// <param name="separator">Separator to be used in the rows</param>
        /// <returns></returns>
        public string[] GetDataLayoutAsCSV(CompilationUnit compilationUnit, string separator)
        {
            var rows = new List<string>();
            foreach (var dataLayoutNode in CollectDataLayoutNodes(compilationUnit))
            {
                var row = CreateRow(dataLayoutNode, separator);
                if (row != null)
                {
                    rows.Add(row);
                }
            }

            return rows.ToArray();

            static string CreateRow(Tuple<int, DataDefinition, int> dataLayoutNode, string separator)
            {
                var nodeLevel = dataLayoutNode.Item1;
                var dataDefinition = dataLayoutNode.Item2;
                var occursDimension = dataLayoutNode.Item3;

                var row = new StringBuilder();

                //TODO manage slack bytes (property is dataDefinition.SlackBytes)

                // Line number (starting at 1)
                AppendToRow(dataDefinition.CodeElement.GetLineInMainSource() + 1);

                // Node level
                AppendToRow(nodeLevel);

                // Level number
                AppendToRow(dataDefinition.CodeElement.LevelNumber);

                // Name
                AppendNameToRow(dataDefinition, occursDimension);

                // Declaration (Picture, Usage, REDEFINES, OCCURS, ...)
                AppendDeclarationToRow(dataDefinition);

                // Start/End/Length
                var start = dataDefinition.StartPosition;
                var length = dataDefinition.PhysicalLength;
                AppendToRow(start);
                AppendToRow(GetEnd(start, length));
                row.Append(length);

                return row.ToString();

                void AppendToRow(object value)
                {
                    row.Append(value).Append(separator);
                }

                void AppendNameToRow(DataDefinition dataDefinition, int occursDimension)
                {
                    row.Append(dataDefinition.Name ?? FILLER);
                    if (occursDimension > 0)
                    {
                        AppendOccursSuffixToRow(occursDimension);
                    }
                    row.Append(separator);
                }

                void AppendOccursSuffixToRow(int occursDimension)
                {
                    row.Append(OCCURS_SUFFIX_START);
                    row.Append(OCCURS_SUFFIX);
                    for (int i = 1; i < occursDimension; i++)
                    {
                        row.Append(SPACE).Append(OCCURS_SUFFIX);
                    }
                    row.Append(OCCURS_SUFFIX_END);
                }

                void AppendDeclarationToRow(DataDefinition dataDefinition)
                {
                    var initialRowLength = row.Length;

                    CodeElementType type = dataDefinition.CodeElement.Type;
                    if (type == CodeElementType.DataRedefinesEntry)
                    {
                        row.Append(string.Format(REDEFINES, ((DataRedefines) dataDefinition).CodeElement.RedefinesDataName.Name));
                    }
                    else if (type == CodeElementType.DataDescriptionEntry)
                    {
                        if (dataDefinition.Picture != null)
                        {
                            row.Append(string.Format(PIC, dataDefinition.Picture.Value));
                        }
                        string usage = ((DataDescriptionEntry) dataDefinition.CodeElement).Usage?.Token.Text;
                        if (!string.IsNullOrWhiteSpace(usage))
                        {
                            AppendSpaceIfNeeded();
                            row.Append(usage);
                        }

                        if (row.Length == initialRowLength && dataDefinition.ChildrenCount > 0)
                        {
                            row.Append(GROUP);
                        }
                    }

                    if (dataDefinition.IsTableOccurence)
                    {
                        AppendSpaceIfNeeded();
                        row.Append(string.Format(OCCURS, dataDefinition.MaxOccurencesCount));
                    }

                    row.Append(separator);

                    void AppendSpaceIfNeeded()
                    {
                        if (row.Length != initialRowLength)
                        {
                            row.Append(SPACE);
                        }
                    }
                }

                static object GetEnd(long start, long length)
                {
                    var end = start + length - 1;
                    return (end > 0) ? end : UNDEFINED;
                }
            }
        }

        private List<Tuple<int, DataDefinition, int>> CollectDataLayoutNodes(CompilationUnit compilationUnit)
        {
            var dataLayoutNodes = new List<Tuple<int, DataDefinition, int>>();
            Node dataDivision = compilationUnit?.TemporaryProgramClassDocumentSnapshot?.Root?.MainProgram?.GetChildren<DataDivision>()?.FirstOrDefault();
            if (dataDivision != null)
            {
                // Consider data declared in the Working storage
                var workingStorage = dataDivision.GetChildren<WorkingStorageSection>().FirstOrDefault();
                if (workingStorage != null)
                {
                    CollectDataLayoutNodes(0, workingStorage, 0);
                }

                // Consider also data declared in the Local storage
                var localStorage = dataDivision.GetChildren<LocalStorageSection>().FirstOrDefault();
                if (localStorage != null)
                {
                    CollectDataLayoutNodes(0, localStorage, 0);
                }
            }

            return dataLayoutNodes;

            void CollectDataLayoutNodes(int nodeLevel, Node node, int occursDimension)
            {
                foreach (var child in node.Children)
                {
                    if (child is DataDefinition childDefinition)
                    {
                        var childOccursDimension = childDefinition.IsTableOccurence ? occursDimension + 1 : occursDimension;
                        if (IsInScope(childDefinition))
                        {
                            dataLayoutNodes.Add(new Tuple<int, DataDefinition, int>(nodeLevel, childDefinition, childOccursDimension));
                        }
                        if (childDefinition.Children.Count > 0)
                        {
                            CollectDataLayoutNodes(nodeLevel + 1, childDefinition, childOccursDimension);
                        }
                    }
                }

                static bool IsInScope(DataDefinition dataDefinition)
                {
                    DataDefinitionEntry codeElement = dataDefinition.CodeElement;
                    if (codeElement == null || codeElement.Line < 0)
                    {
                        // Ignore node without CodeElement or with negative line number
                        return false;
                    }

                    // Ignore level 88 and 66
                    CodeElementType type = codeElement.Type;
                    return type != CodeElementType.DataConditionEntry && type != CodeElementType.DataRenamesEntry;
                }
            }
        }
        #endregion

#if EUROINFO_RULES
        public (string[], int) GetRemarksData(CompilationUnit compilationUnit)
        {
            // Get used CPYs from parsed code
            var usedCPYs = compilationUnit.CopyTextNamesVariations;

            // Check for COPY instructions in Debug lines
            var debugUsedCPYs = compilationUnit.CobolTextLines
                .Where(line => line.Type == CobolTextLineType.Debug)
                .SelectMany(FindTextNameVariations);

            // Concat and filter
            var allUsedCPYs = usedCPYs.Concat(debugUsedCPYs)
                .Where(v => this.Configuration.IsCpyCopy(v.TextName))
                .Select(v => v.TextNameWithSuffix.ToUpperInvariant())
                .Distinct()
                .ToArray();

            // Get expected REMARKS location
            int insertionLine = 0;
            var programIdentification = compilationUnit.CodeElementsDocumentSnapshot.CodeElements.FirstOrDefault(ce => ce.Type == CodeElementType.ProgramIdentification);
            if (programIdentification != null)
            {
                insertionLine = programIdentification.LineEnd + 1;
            }

            return (allUsedCPYs, insertionLine);

            IEnumerable<RemarksDirective.TextNameVariation> FindTextNameVariations(ICobolTextLine debugLine)
            {
                const string textNameCaptureRegex = @"COPY\s+(\w+)";
                var matches = Regex.Matches(debugLine.SourceText, textNameCaptureRegex, RegexOptions.IgnoreCase);
                foreach (Match match in matches)
                {
                    if (match.Groups.Count > 1)
                    {
                        yield return new RemarksDirective.TextNameVariation(match.Groups[1].Value);
                    }
                }
            }
        }
#endif
    }

    [Flags]
    public enum LsrTestingOptions
    {
        NoLsrTesting = 0,
        LsrSourceDocumentTesting = 0x1 << 0,
        LsrScanningPhaseTesting = LsrSourceDocumentTesting | 0x1 << 1,
        LsrPreprocessingPhaseTesting = LsrScanningPhaseTesting | 0x01 << 2,
        LsrParsingPhaseTesting = LsrPreprocessingPhaseTesting | 0x01 << 3,
        LsrSemanticPhaseTesting = LsrParsingPhaseTesting | 0x1 << 4,
        LsrCodeAnalysisPhaseTesting = LsrSemanticPhaseTesting | 0x1 << 5
    }

    public class DiagnosticEvent : EventArgs
    {
        public IEnumerable<Compiler.Diagnostics.Diagnostic> Diagnostics { get; set; }  
    }

    public class MissingCopiesEvent : EventArgs
    {
        public List<string> Copies { get; set; }
    }

    public class LoadingIssueEvent : EventArgs
    {
        public string Message { get; set; }
    }

    public class ChangedCopy
    {
        private const string ALL_PROJECTS = "$all";

        /// <summary>
        /// Try parse a file event uri into a ChangedCopy.
        /// A changed copy respects the format 'projectKey/copyName' where projectKey can be:
        /// - empty
        /// - equals to '$all'
        /// - a valid workspace project identifier
        /// </summary>
        /// <param name="fileEventUri">String representing a modified copy</param>
        /// <param name="changedCopy">[out] new ChangedCopy instance if parsing was successful, null otherwise</param>
        /// <returns>True if parsing succeeded, False otherwise</returns>
        public static bool TryParse(string fileEventUri, out ChangedCopy changedCopy)
        {
            string[] parts = fileEventUri?.Split('/');

            if (parts == null || parts.Length != 2)
            {
                //Null, no slash or more than one slash -> invalid
                changedCopy = null;
                return false;
            }

            //projectKey/copyName, if a second '/' is present, everything after it is ignored
            string projectKey = parts[0].Trim();
            string copyName = parts[1].Trim();
            switch (projectKey)
            {
                case "":
                    //default project
                    changedCopy = new ChangedCopy(copyName, false, null);
                    break;
                case ALL_PROJECTS:
                    //special syntax, all projects
                    changedCopy = new ChangedCopy(copyName, true, null);
                    break;
                default:
                    //named project
                    changedCopy = new ChangedCopy(copyName, false, projectKey);
                    break;
            }

            return true;
        }

        public string CopyName { get; }

        public bool ClearAllCaches { get; }

        public string OwnerProject { get; }

        private ChangedCopy(string copyName, bool clearAllCaches, string ownerProject)
        {
            CopyName = copyName;
            ClearAllCaches = clearAllCaches;
            OwnerProject = ownerProject;
        }
    }
}
