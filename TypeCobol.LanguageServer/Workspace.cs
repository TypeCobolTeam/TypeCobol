using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Timers;
using Analytics;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.CustomExceptions;
using TypeCobol.LanguageServer.Context;
using TypeCobol.Tools.Options_Config;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.Tools.APIHelpers;

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
        private SymbolTable _customSymbols;
        private string _rootDirectoryFullName;
        private string _workspaceName;
        private DependenciesFileWatcher _DepWatcher;
        private CopyWatcher _CopyWatcher;
        private System.Timers.Timer _semanticUpdaterTimer;
        private bool _timerDisabled;
        private readonly Dictionary<Uri, DocumentContext> _openedDocuments;
        private readonly object _lockForOpenedDocuments = new object();

        internal CompilationProject CompilationProject { get; private set; }

        private List<FileCompiler> _fileCompilerWaittingForNodePhase;
        public TypeCobolConfiguration Configuration { get; private set; }
        public EventHandler<DiagnosticEvent> DiagnosticsEvent { get; set; }
        public EventHandler<EventArgs> DocumentModifiedEvent { get; set; }
        public EventHandler<MissingCopiesEvent> MissingCopiesEvent { get; set; }
        public EventHandler<LoadingIssueEvent> LoadingIssueEvent { get; set; }
        public EventHandler<ThreadExceptionEventArgs> ExceptionTriggered { get; set; }
        public EventHandler<string> WarningTrigger { get; set; }
        public Queue<MessageActionWrapper> MessagesActionsQueue { get; private set; }
        private Func<string, Uri, bool> _Logger;

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

        /// <summary>
        /// True to use ANTLR for parsing a program
        /// </summary>
        public bool UseAntlrProgramParsing { get; set; }

        /// <summary>
        /// True to use Euro-Information replacement rules
        /// </summary>
        public bool UseEuroInformationLegacyReplacingSyntax { get; set; }

        /// <summary>
        /// Are we supporting Syntax Coloring Notifications.    
        /// </summary>
        public bool UseSyntaxColoring { get; set; }
        /// <summary>
        /// Are we supporting OutlineRefresh Notifications.    
        /// </summary>
        public bool UseOutlineRefresh { get; set; }

        /// <summary>
        /// Indicates whether this workspace has opened documents or not.
        /// </summary>
        public bool IsEmpty
        {
            get
            {
                lock (_lockForOpenedDocuments)
                {
                    return _openedDocuments.Count == 0;
                }
            }
        }

        #endregion


        public Workspace(string rootDirectoryFullName, string workspaceName, Queue<MessageActionWrapper> messagesActionsQueue, Func<string, Uri, bool> logger)
        {
            MessagesActionsQueue = messagesActionsQueue;
            Configuration = new TypeCobolConfiguration();
            _openedDocuments = new Dictionary<Uri, DocumentContext>();
            _fileCompilerWaittingForNodePhase = new List<FileCompiler>();
            _Logger = logger;

            this._rootDirectoryFullName = rootDirectoryFullName;
            this._workspaceName = workspaceName;

            var defaultDocumentFormat = new DocumentFormat(Encoding.GetEncoding("iso-8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat);
            this.CompilationProject = new CompilationProject(
                _workspaceName, _rootDirectoryFullName, Helpers.DEFAULT_EXTENSIONS, defaultDocumentFormat,
                new TypeCobolOptions(), null); //Initialize a default CompilationProject - has to be recreated after ConfigurationChange Notification
            this.CompilationProject.CompilationOptions.UseAntlrProgramParsing =
                this.CompilationProject.CompilationOptions.UseAntlrProgramParsing || UseAntlrProgramParsing;

            this.CompilationProject.CompilationOptions.UseEuroInformationLegacyReplacingSyntax =
                this.CompilationProject.CompilationOptions.UseEuroInformationLegacyReplacingSyntax ||
                UseEuroInformationLegacyReplacingSyntax;

            // Create the refresh action that will be used by file watchers
            Action refreshAction = RefreshOpenedFiles;

            _DepWatcher = new DependenciesFileWatcher(this, refreshAction);
            _CopyWatcher = new CopyWatcher(this, refreshAction);
        }

        /// <summary>
        /// Start continuous background compilation on a newly opened file
        /// </summary>
        /// <param name="docContext">The Document context</param>
        /// <param name="sourceText">The source text</param>
        /// <returns>The corresponding FileCompiler instance.</returns>
        public FileCompiler OpenTextDocument(DocumentContext docContext, string sourceText) => OpenTextDocument(docContext, sourceText, LsrTestOptions);

        private FileCompiler OpenTextDocument(DocumentContext docContext, string sourceText, LsrTestingOptions lsrOptions)
        {
            string fileName = Path.GetFileName(docContext.Uri.LocalPath);
            ITextDocument initialTextDocumentLines = new ReadOnlyTextDocument(fileName, Configuration.Format.Encoding, Configuration.Format.ColumnsLayout, sourceText);
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

            var matchingPgm =
                _customSymbols?.Programs.Keys.FirstOrDefault(
                    k => k.Equals(inputFileName, StringComparison.OrdinalIgnoreCase));
            if (matchingPgm != null)
            {
                arrangedCustomSymbol = new SymbolTable(_customSymbols, SymbolTable.Scope.Namespace);
                var prog = _customSymbols.Programs.Values.SelectMany(p => p).Where(p => p.Name != matchingPgm);
                arrangedCustomSymbol.CopyAllPrograms(new List<List<Program>>() {prog.ToList()});
                arrangedCustomSymbol.Programs.Remove(matchingPgm);
            }
            fileCompiler = new FileCompiler(initialTextDocumentLines, CompilationProject.SourceFileProvider,
                CompilationProject, CompilationProject.CompilationOptions, arrangedCustomSymbol ?? _customSymbols,
                false, CompilationProject);
#else
            fileCompiler = new FileCompiler(initialTextDocumentLines, CompilationProject.SourceFileProvider, CompilationProject, CompilationProject.CompilationOptions, _customSymbols, false, CompilationProject);
#endif
            //Set Any Language Server Connection Options.
            docContext.FileCompiler = fileCompiler;
            docContext.LanguageServerConnection(true);

            fileCompiler.CompilationResultsForProgram.UpdateTokensLines();

            lock (_lockForOpenedDocuments)
            {
                if (_openedDocuments.ContainsKey(docContext.Uri))
                    CloseSourceFile(docContext.Uri); //Close and remove the previous opened file.

                _openedDocuments.Add(docContext.Uri, docContext);
                fileCompiler.CompilationResultsForProgram.ProgramClassChanged += ProgramClassChanged;
            }

            fileCompiler.CompilationResultsForProgram.SetOwnerThread(Thread.CurrentThread);

            if (lsrOptions != LsrTestingOptions.LsrSourceDocumentTesting)
            {
                fileCompiler.CompileOnce(lsrOptions.ExecutionStep(fileCompiler.CompilerOptions.ExecToStep.Value), fileCompiler.CompilerOptions.HaltOnMissingCopy, fileCompiler.CompilerOptions.UseAntlrProgramParsing); //Let's parse file for the first time after opening. 
            }

            return fileCompiler;
        }

        public bool TryGetOpenedDocumentContext(Uri fileUri, out DocumentContext openedDocumentContext)
        {
            lock (_lockForOpenedDocuments)
            {
                return _openedDocuments.TryGetValue(fileUri, out openedDocumentContext);
            }
        }

        /// <summary>
        /// Update the text contents of the file
        /// </summary>
        public void UpdateSourceFile(Uri fileUri, TextChangedEvent textChangedEvent)
        {
            if (TryGetOpenedDocumentContext(fileUri, out var contextToUpdate))
            {
                FileCompiler fileCompilerToUpdate = contextToUpdate.FileCompiler;
                _semanticUpdaterTimer?.Stop();

                fileCompilerToUpdate.CompilationResultsForProgram.UpdateTextLines(textChangedEvent);
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
                if (execStep > ExecutionStep.SyntaxCheck)
                    execStep = ExecutionStep.SyntaxCheck; //The maximum execstep authorize for incremental parsing is SyntaxCheck, 
                                                          //further it's for semantic, which is handle by NodeRefresh method


                fileCompilerToUpdate.CompileOnce(execStep, fileCompilerToUpdate.CompilerOptions.HaltOnMissingCopy, fileCompilerToUpdate.CompilerOptions.UseAntlrProgramParsing);
                fileCompilerToUpdate.ExecutionStepEventHandler -= handler.Invoke;
                

                if (LsrTestOptions == LsrTestingOptions.NoLsrTesting || IsLsrSemanticTesting)
                {
                    if (!_timerDisabled) //If TimerDisabled is false, create a timer to automatically launch Node phase
                    {
                        lock (_fileCompilerWaittingForNodePhase)
                        {
                            if (!_fileCompilerWaittingForNodePhase.Contains(fileCompilerToUpdate))
                                _fileCompilerWaittingForNodePhase.Add(fileCompilerToUpdate); //Store that this fileCompiler will soon need a Node Phase
                        }

                        _semanticUpdaterTimer = new System.Timers.Timer(750);
                        _semanticUpdaterTimer.Elapsed += (sender, e) => TimerEvent(sender, e, fileCompilerToUpdate);
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
                        foreach (var token in fileCompiler.CompilationResultsForProgram.ProcessedTokensDocumentSnapshot.ProcessedTokensSource)
                            sb.AppendLine(token.ToString());
                        _Logger(sb.ToString(), fileUri);
                    }
                    break;
                case ExecutionStep.SyntaxCheck:
                    if (IsLsrParserTesting)
                    {
                        //Return log information about code elements
                        var sb = new StringBuilder();
                        foreach (var codeElement in fileCompiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.CodeElements)
                            sb.AppendLine(codeElement.ToString());
                        _Logger(sb.ToString(), fileUri);
                    }
                    break;
                case ExecutionStep.SemanticCheck:
                case ExecutionStep.CrossCheck:
                case ExecutionStep.QualityCheck:
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
        private void TimerEvent(object sender, ElapsedEventArgs eventArgs, FileCompiler fileCompiler)
        {
            try
            {
                _semanticUpdaterTimer.Stop();
                Action nodeRefreshAction = () => { RefreshSyntaxTree(fileCompiler, SyntaxTreeRefreshLevel.RebuildNodesAndPerformQualityCheck); };
                lock (MessagesActionsQueue)
                {
                    MessagesActionsQueue.Enqueue(new MessageActionWrapper(nodeRefreshAction));
                }
            }
            catch (Exception e)
            {
                //In case Timer Thread crash
                ExceptionTriggered(null, new ThreadExceptionEventArgs(e));
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
            /// Rebuilds semantic document and run CrossCheck on updated version
            /// </summary>
            RebuildNodes,

            /// <summary>
            /// Same as RebuildNodes but also launches code quality analysis
            /// </summary>
            RebuildNodesAndPerformQualityCheck,

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

            lock (_fileCompilerWaittingForNodePhase)
            {
                var fileCompilerNeedsRefresh = _fileCompilerWaittingForNodePhase.Contains(fileCompiler);
                if (fileCompilerNeedsRefresh)
                {
                    _fileCompilerWaittingForNodePhase.Remove(fileCompiler);
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
                    case SyntaxTreeRefreshLevel.RebuildNodesAndPerformQualityCheck:
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
        /// Stop continuous background compilation after a file has been closed
        /// </summary>
        public void CloseSourceFile(Uri fileUri)
        {
            lock (_lockForOpenedDocuments)
            {
                if (_openedDocuments.ContainsKey(fileUri))
                {
                    var contextToClose = _openedDocuments[fileUri];
                    FileCompiler fileCompilerToClose = contextToClose.FileCompiler;
                    _openedDocuments.Remove(fileUri);
                    fileCompilerToClose.CompilationResultsForProgram.ProgramClassChanged -= ProgramClassChanged;
                }
            }            
        }

        public void DidChangeConfigurationParams(string settings)
        {
            DidChangeConfigurationParams(settings.Split(' '));
        }

        /// <summary>
        /// Handle the Configuration change notification.
        /// </summary>
        /// <param name="arguments">The arguments</param>
        public void DidChangeConfigurationParams(IEnumerable<string> arguments)
        {
            Configuration = new TypeCobolConfiguration();
            var options = TypeCobolOptionSet.GetCommonTypeCobolOptions(Configuration);

            var errors = TypeCobolOptionSet.InitializeCobolOptions(Configuration, arguments, options);

            //Adding default copies folder
            var folder = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName);
            Configuration.CopyFolders.Add(folder + @"\DefaultCopies\");

            if (Configuration.Telemetry)
                AnalyticsWrapper.Telemetry.TelemetryVerboseLevel = TelemetryVerboseLevel.Completion; //If telemetry arg is passed enable telemetry

            if (Configuration.UseAntlrProgramParsing)
                UseAntlrProgramParsing = true;

            if (Configuration.UseEuroInformationLegacyReplacingSyntax)
                UseEuroInformationLegacyReplacingSyntax = true;

            if (Configuration.ExecToStep >= ExecutionStep.Generate)
                Configuration.ExecToStep = ExecutionStep.QualityCheck; //Language Server does not support Cobol Generation for now

            var typeCobolOptions = new TypeCobolOptions(Configuration);

            //Configure CFG/DFA analyzer + external analyzers if any
            var analyzerProvider = new CompositeAnalyzerProvider();
            analyzerProvider.AddActivator((o, t) => CfgDfaAnalyzerFactory.CreateCfgAnalyzer("cfg-dfa", Configuration.CfgBuildingMode));
            analyzerProvider.AddCustomProviders(Configuration.CustomAnalyzerFiles);

            CompilationProject = new CompilationProject(_workspaceName, _rootDirectoryFullName, Helpers.DEFAULT_EXTENSIONS, Configuration.Format, typeCobolOptions, analyzerProvider);

            if (Configuration.CopyFolders != null && Configuration.CopyFolders.Count > 0)
            {
                foreach (var copyFolder in Configuration.CopyFolders)
                {
                    CompilationProject.SourceFileProvider.AddLocalDirectoryLibrary(copyFolder, false,
                        new[] {".cpy"}, Configuration.Format.Encoding,
                        Configuration.Format.EndOfLineDelimiter, Configuration.Format.FixedLineLength);
                }
                
            }

            if (!IsEmpty)
                RefreshOpenedFiles();
            else
                RefreshCustomSymbols();

            //Dispose previous watcher before setting new ones
            _DepWatcher.Dispose();
            _CopyWatcher.Dispose();
            foreach (var depFolder in Configuration.CopyFolders)
            {
                _CopyWatcher.SetDirectoryWatcher(depFolder);
            }
            foreach (var depFolder in Configuration.Dependencies)
            {
                _DepWatcher.SetDirectoryWatcher(depFolder);
            }
            foreach (var intrinsicFolder in Configuration.Copies)
            {
                _DepWatcher.SetDirectoryWatcher(intrinsicFolder);
            }
        }

        public void UpdateMissingCopies(Uri fileUri, List<string> RemainingMissingCopies)
        {
            //TODO remove client to server notification properly.
        }

        /// <summary>
        /// Refresh all opened files' parser.
        /// </summary>
        public void RefreshOpenedFiles()
        {
            RefreshCustomSymbols();

            lock (_lockForOpenedDocuments)
            {
                var tempOpeneContexts = new Dictionary<Uri, DocumentContext >(_openedDocuments);
                foreach (var contextEntry in tempOpeneContexts)
                {
                    Uri uri = contextEntry.Key;
                    DocumentContext docContext = contextEntry.Value;
                    var sourceText = new StringBuilder();
                    foreach (var line in docContext.FileCompiler.TextDocument.Lines)
                        sourceText.AppendLine(line.Text);

                    //Disconnect previous LanguageServer connection
                    docContext.LanguageServerConnection(false);                    

                    OpenTextDocument(docContext, sourceText.ToString(), LsrTestingOptions.NoLsrTesting);
                }
            }
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
            _customSymbols = null;
            try
            {
                _customSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(Configuration.Copies, Configuration.Format, DiagnosticsErrorEvent); //Refresh Intrinsics
                _customSymbols = Tools.APIHelpers.Helpers.LoadDependencies(Configuration, _customSymbols, DiagnosticsErrorEvent, out List<RemarksDirective.TextNameVariation> usedCopies, out IDictionary<string, IEnumerable<string>> missingCopies); //Refresh Dependencies

                if (missingCopies.Count > 0)
                {
                    MissingCopiesEvent(missingCopies.First().Key, new MissingCopiesEvent() { Copies = missingCopies.SelectMany(c => c.Value).Distinct().ToList() });
                    return;//Do not report diagnostics if copies are missing
                }

                if (diagDetected)
                {
                    var message = "An error occured while trying to load Intrinsics or Dependencies files.";
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
                    WarningTrigger(null, sb.ToString()); //Send warning notification to display info to the user. 
                }
                else
                {//Send an LoadingIssueEvent with an empty message to tell the client that there are no issues.
                    LoadingIssueEvent(null, new LoadingIssueEvent() { Message = "" });
                }
            }
            catch (TypeCobolException typeCobolException)
            {
                LoadingIssueEvent(null, new LoadingIssueEvent() { Message = "An error occured while trying to load Intrinsics or Dependencies files." }); //Send notification to client

                AnalyticsWrapper.Telemetry.TrackException(typeCobolException, typeCobolException.Path);

                if (typeCobolException.NeedMail)
                    AnalyticsWrapper.Telemetry.SendMail(typeCobolException, Configuration.InputFiles, Configuration.CopyFolders, Configuration.CommandLine);
            }
            catch (Exception e)
            {
                LoadingIssueEvent(null, new LoadingIssueEvent() { Message = "An error occured while trying to load Intrinsics or Dependencies files." }); //Send notification to client

                AnalyticsWrapper.Telemetry.TrackException(e, null);
                AnalyticsWrapper.Telemetry.SendMail(e, Configuration.InputFiles, Configuration.CopyFolders, Configuration.CommandLine);
            }

        }

        /// <summary>
        /// Called by a ProgramClass changed event trigger. 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ProgramClassChanged(object cUnit, ProgramClassEvent programEvent)
        {
            var compilationUnit = cUnit as CompilationUnit;

            // Search for corresponding opened document.
            Uri fileUri;
            lock (_lockForOpenedDocuments)
            {
                fileUri = _openedDocuments.Keys.FirstOrDefault(k =>
                    compilationUnit != null && k.LocalPath.Contains(compilationUnit.TextSourceInfo.Name));
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
            
            DiagnosticsEvent(fileUri, new DiagnosticEvent() { Diagnostics = diags.Take(Configuration.MaximumDiagnostics == 0 ? 200 : Configuration.MaximumDiagnostics) });

            if (compilationUnit?.MissingCopies.Count > 0)
                MissingCopiesEvent(fileUri, new MissingCopiesEvent() { Copies = new List<string>(compilationUnit.MissingCopies) });

            DocumentModifiedEvent?.Invoke(fileUri, new EventArgs());
        }
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
}
