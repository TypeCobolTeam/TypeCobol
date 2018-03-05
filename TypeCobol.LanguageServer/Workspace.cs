using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Timers;
using Analytics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.CustomExceptions;
using TypeCobol.Tools.Options_Config;
using TypeCobol.LanguageServer.Utilities;

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
        private CompilationProject CompilationProject;
        private string[] _extensions = { ".cbl", ".cpy" };
        private DependenciesFileWatcher _DepWatcher;
        private System.Timers.Timer _semanticUpdaterTimer;
        private bool _timerDisabled;


        private TypeCobolConfiguration TypeCobolConfiguration { get; set; }
        private List<FileCompiler> _fileCompilerWaittingForNodePhase;
        public Dictionary<Uri, FileCompiler> OpenedFileCompiler{ get; private set; }
        public EventHandler<DiagnosticEvent> DiagnosticsEvent { get; set; }
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

        [Flags]
        public enum LsrTestingOptions
        {
            NoLsrTesting = 0,
            LsrSourceDocumentTesting = 0x1 << 0,
            LsrScanningPhaseTesting = LsrSourceDocumentTesting | 0x1 << 1,
            LsrPreprocessingPhaseTesting = LsrScanningPhaseTesting | 0x01 << 2,
            LsrParsingPhaseTesting = LsrPreprocessingPhaseTesting | 0x01 << 3,
            LsrSemanticPhaseTesting = LsrParsingPhaseTesting | 0x1 << 4
        }

        /// <summary>
        /// Lsr Test Options.
        /// </summary>
        public LsrTestingOptions LsrTestOptions
        {
            get; set;
        }

        /// <summary>
        /// LSR always test the source code at least
        /// </summary>
        public bool IsLsrSourceTesting
        {
            get { return (LsrTestOptions & LsrTestingOptions.LsrSourceDocumentTesting) == LsrTestingOptions.LsrSourceDocumentTesting; }
            set { LsrTestOptions = value  ? (LsrTestOptions | LsrTestingOptions.LsrSourceDocumentTesting) : LsrTestingOptions.NoLsrTesting; }
        }

        /// <summary>
        /// LSR testing the Source document and scanning phase ?
        /// </summary>
        public bool IsLsrScannerTesting
        {
            get { return (LsrTestOptions & LsrTestingOptions.LsrScanningPhaseTesting) == LsrTestingOptions.LsrScanningPhaseTesting; }
            set { LsrTestOptions = value ? (LsrTestOptions | LsrTestingOptions.LsrScanningPhaseTesting) : LsrTestingOptions.NoLsrTesting; }
        }

        /// <summary>
        /// LSR testing the Source document, scanning phase, and preprocessing phase ?
        /// </summary>
        public bool IsLsrPreprocessinTesting
        {
            get { return (LsrTestOptions & LsrTestingOptions.LsrPreprocessingPhaseTesting) == LsrTestingOptions.LsrPreprocessingPhaseTesting; }
            set { LsrTestOptions = value ? (LsrTestOptions | LsrTestingOptions.LsrPreprocessingPhaseTesting) : LsrTestingOptions.NoLsrTesting; }
        }

        /// <summary>
        /// Testing the Source document, the scanning phase and code elements parsing phase.
        /// </summary>
        public bool IsLsrParserTesting
        {
            get { return (LsrTestOptions & LsrTestingOptions.LsrParsingPhaseTesting) == LsrTestingOptions.LsrParsingPhaseTesting; }
            set { LsrTestOptions = value ? (LsrTestOptions | LsrTestingOptions.LsrParsingPhaseTesting) : LsrTestingOptions.NoLsrTesting; }
        }

        /// <summary>
        /// Testing the Source document the scanning phase, the code elements parsing phase and the semantic analysis phase.
        /// </summary>
        public bool IsLsrSemanticTesting
        {
            get { return (LsrTestOptions & LsrTestingOptions.LsrSemanticPhaseTesting) == LsrTestingOptions.LsrSemanticPhaseTesting; }
            set { LsrTestOptions = value ? (LsrTestOptions | LsrTestingOptions.LsrSemanticPhaseTesting) : LsrTestingOptions.NoLsrTesting; }
        }

        #endregion


        public Workspace(string rootDirectoryFullName, string workspaceName, Queue<MessageActionWrapper> messagesActionsQueue, Func<string, Uri, bool> logger)
        {
            MessagesActionsQueue = messagesActionsQueue;
            TypeCobolConfiguration = new TypeCobolConfiguration();
            OpenedFileCompiler = new Dictionary<Uri, FileCompiler>();
            _fileCompilerWaittingForNodePhase = new List<FileCompiler>();
            _Logger = logger;

            this._rootDirectoryFullName = rootDirectoryFullName;
            this._workspaceName = workspaceName;

            this.CompilationProject = new CompilationProject(
                _workspaceName, _rootDirectoryFullName, _extensions,
                Encoding.GetEncoding("iso-8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat,
                new TypeCobolOptions()); //Initialize a default CompilationProject - has to be recreated after ConfigurationChange Notification

            _DepWatcher = new DependenciesFileWatcher(this);
        }

        /// <summary>
        /// Start continuous background compilation on a newly opened file
        /// </summary>
        public void OpenSourceFile(Uri fileUri, string sourceText, LsrTestingOptions lsrOptions)
        {
            string fileName = Path.GetFileName(fileUri.LocalPath);
            ITextDocument initialTextDocumentLines = new ReadOnlyTextDocument(fileName, TypeCobolConfiguration.Format.Encoding, TypeCobolConfiguration.Format.ColumnsLayout, sourceText);
            FileCompiler fileCompiler = null;

#if EUROINFO_RULES //Issue #583
            SymbolTable arrangedCustomSymbol = null;
            var inputFileName = fileName.Substring(0, 8);
            var matchingPgm =
                _customSymbols.Programs.Keys.FirstOrDefault(
                    k => k.Equals(inputFileName, StringComparison.InvariantCultureIgnoreCase));
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


            fileCompiler.CompilationResultsForProgram.UpdateTokensLines();

            lock (OpenedFileCompiler)
            {
                if (OpenedFileCompiler.ContainsKey(fileUri))
                    CloseSourceFile(fileUri); //Close and remove the previous opened file.

                OpenedFileCompiler.Add(fileUri, fileCompiler);
                fileCompiler.CompilationResultsForProgram.ProgramClassChanged += ProgramClassChanged;
            }

            fileCompiler.CompilationResultsForProgram.SetOwnerThread(Thread.CurrentThread);

            if (lsrOptions != LsrTestingOptions.LsrSourceDocumentTesting)
            {
                fileCompiler.CompileOnce(); //Let's parse file for the first time after opening. 
            }
            else
            {
                fileCompiler.CompileOnce(lsrOptions.ExecutionStep(fileCompiler.CompilerOptions.ExecToStep.Value), fileCompiler.CompilerOptions.HaltOnMissingCopy); //Let's parse file for the first time after opening. 
            }
        }

        /// <summary>
        /// Update the text contents of the file
        /// </summary>
        public void UpdateSourceFile(Uri fileUri, TextChangedEvent textChangedEvent, bool bAsync)
        {
            FileCompiler fileCompilerToUpdate = null;
            if (OpenedFileCompiler.TryGetValue(fileUri, out fileCompilerToUpdate))
            {
                _semanticUpdaterTimer?.Stop();

                fileCompilerToUpdate.CompilationResultsForProgram.UpdateTextLines(textChangedEvent);
                if (IsLsrSourceTesting)
                {
                    //Log text lines string 
                    foreach (var cobolTextLine in fileCompilerToUpdate.CompilationResultsForProgram.CobolTextLines)
                    {
                        _Logger(cobolTextLine.SourceText, fileUri);
                    }
                }

                if (!bAsync)
                {//Don't wait asynchronous snapshot refresh.
                    if (IsLsrScannerTesting || LsrTestOptions == LsrTestingOptions.NoLsrTesting)
                    {
                        fileCompilerToUpdate.CompilationResultsForProgram.UpdateTokensLines(
                       () =>
                       {
                           if (LsrTestOptions == LsrTestingOptions.NoLsrTesting)
                           {
                               fileCompilerToUpdate.CompilationResultsForProgram.RefreshTokensDocumentSnapshot();
                               fileCompilerToUpdate.CompilationResultsForProgram
                                    .RefreshProcessedTokensDocumentSnapshot();
                               fileCompilerToUpdate.CompilationResultsForProgram
                                    .RefreshCodeElementsDocumentSnapshot();
                           }
                           else
                           {

                               if (IsLsrScannerTesting)
                               {
                                   fileCompilerToUpdate.CompilationResultsForProgram.RefreshTokensDocumentSnapshot();

                                   //Return log information about updated tokens
                                   //fileCompilerToUpdate.CompilationResultsForProgram.TokensDocumentSnapshot.

                               }
                               if (IsLsrPreprocessinTesting)
                               {
                                   fileCompilerToUpdate.CompilationResultsForProgram
                                        .RefreshProcessedTokensDocumentSnapshot();
                               }
                               if (IsLsrParserTesting)
                               {
                                   fileCompilerToUpdate.CompilationResultsForProgram
                                        .RefreshCodeElementsDocumentSnapshot();
                               }
                           }
                       }
                       );
                    }

                    if (LsrTestOptions == LsrTestingOptions.NoLsrTesting)
                    {
                        lock (_fileCompilerWaittingForNodePhase)
                        {
                            if (!_fileCompilerWaittingForNodePhase.Contains(fileCompilerToUpdate))
                                _fileCompilerWaittingForNodePhase.Add(fileCompilerToUpdate); //Store that this fileCompiler will soon need a Node Phase
                        }

                        if (!_timerDisabled) //If TimerDisabled is false, create a timer to automatically launch Node phase
                        {
                            _semanticUpdaterTimer = new System.Timers.Timer(1000);
                            _semanticUpdaterTimer.Elapsed += (sender, e) => TimerEvent(sender, e, fileCompilerToUpdate);
                            _semanticUpdaterTimer.Start();
                        }
                    }
                }
                else
                {
                    fileCompilerToUpdate.CompilationResultsForProgram.UpdateTokensLines();
                }
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
                Action nodeRefreshAction = () => { RefreshSyntaxTree(fileCompiler); };
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
        /// Use this method to force a node phase if there is a filecompiler waiting for node refresh. 
        /// </summary>
        /// <param name="fileCompiler">FileCompiler on which the node phase will be done</param>
        public void RefreshSyntaxTree(FileCompiler fileCompiler)
        {
            lock (_fileCompilerWaittingForNodePhase)
            {
                if (!_fileCompilerWaittingForNodePhase.Contains(fileCompiler)) return;                

                _fileCompilerWaittingForNodePhase.Remove(fileCompiler);
                if (LsrTestOptions != LsrTestingOptions.NoLsrTesting && !IsLsrSemanticTesting) return;
                fileCompiler.CompilationResultsForProgram.ProduceTemporarySemanticDocument(); //Produce the temporary snapshot before full cross check
                fileCompiler.CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot(); //Do a Node phase
            }
            
        }

        /// <summary>
        /// Stop continuous background compilation after a file has been closed
        /// </summary>
        public void CloseSourceFile(Uri fileUri)
        {
            lock (OpenedFileCompiler)
            {
                if (OpenedFileCompiler.ContainsKey(fileUri))
                {
                    var fileCompilerToClose = OpenedFileCompiler[fileUri];
                    OpenedFileCompiler.Remove(fileUri);
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
            TypeCobolConfiguration = new TypeCobolConfiguration();
            var options = TypeCobolOptionSet.GetCommonTypeCobolOptions(TypeCobolConfiguration);

            //Adding default copies folder
            var folder = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName);
            TypeCobolConfiguration.CopyFolders.Add(folder + @"\DefaultCopies\");

            if (TypeCobolConfiguration.Telemetry)
                AnalyticsWrapper.Telemetry.DisableTelemetry = false; //If telemetry arg is passed enable telemetry

            if (TypeCobolConfiguration.ExecToStep >= ExecutionStep.Generate)
                TypeCobolConfiguration.ExecToStep = ExecutionStep.CrossCheck; //Language Server does not support Cobol Generation for now

            var typeCobolOptions = new TypeCobolOptions
            {
                HaltOnMissingCopy = TypeCobolConfiguration.HaltOnMissingCopyFilePath != null,
                ExecToStep = TypeCobolConfiguration.ExecToStep,
#if EUROINFO_RULES
                AutoRemarksEnable = TypeCobolConfiguration.AutoRemarks
#endif
            };

            CompilationProject = new CompilationProject(_workspaceName, _rootDirectoryFullName, _extensions, TypeCobolConfiguration.Format.Encoding, TypeCobolConfiguration.Format.EndOfLineDelimiter, TypeCobolConfiguration.Format.FixedLineLength, TypeCobolConfiguration.Format.ColumnsLayout, typeCobolOptions);

            if (TypeCobolConfiguration.CopyFolders != null && TypeCobolConfiguration.CopyFolders.Count > 0)
            {
                foreach (var copyFolder in TypeCobolConfiguration.CopyFolders)
                {
                    CompilationProject.SourceFileProvider.AddLocalDirectoryLibrary(copyFolder, false,
                        new[] {".cpy"}, TypeCobolConfiguration.Format.Encoding,
                        TypeCobolConfiguration.Format.EndOfLineDelimiter, TypeCobolConfiguration.Format.FixedLineLength);
                }
                
            }

            if (OpenedFileCompiler.Count > 0)
                RefreshOpenedFiles();
            else
                RefreshCustomSymbols();

            //Dispose previous watcher before setting new ones
            _DepWatcher.Dispose();
            foreach (var depFolder in TypeCobolConfiguration.Dependencies)
            {
                _DepWatcher.SetDirectoryWatcher(depFolder);
            }
            foreach (var intrinsicFolder in TypeCobolConfiguration.Copies)
            {
                _DepWatcher.SetDirectoryWatcher(intrinsicFolder);
            }
        }

        public void UpdateMissingCopies(Uri fileUri, List<string> RemainingMissingCopies)
        {
            if (!OpenedFileCompiler.Any())
                return;
            var fileCompiler = OpenedFileCompiler[fileUri];
            if (fileCompiler == null)
                return;

            if (RemainingMissingCopies == null || RemainingMissingCopies.Count == 0)
            {
                fileCompiler.CompilationResultsForProgram.MissingCopies.RemoveAll(c => true);
                return;
            }

            fileCompiler.CompilationResultsForProgram.MissingCopies =
                fileCompiler.CompilationResultsForProgram.MissingCopies.Where(
                    c => RemainingMissingCopies.Any(rc => rc == c.TextName)).ToList();
        }

        /// <summary>
        /// Refresh all opened files' parser.
        /// </summary>
        public void RefreshOpenedFiles()
        {
            RefreshCustomSymbols();

            lock(OpenedFileCompiler)
            {
                var tempOpenedFileCompiler = new Dictionary<Uri, FileCompiler>(OpenedFileCompiler);
                foreach (var fileParser in tempOpenedFileCompiler)
                {
                    var sourceText = new StringBuilder();
                    foreach (var line in fileParser.Value.TextDocument.Lines)
                        sourceText.AppendLine(line.Text);

                    OpenSourceFile(fileParser.Key, sourceText.ToString(), LsrTestingOptions.NoLsrTesting);
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
                _customSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(TypeCobolConfiguration.Copies, TypeCobolConfiguration.Format, DiagnosticsErrorEvent); //Refresh Intrinsics
                _customSymbols = Tools.APIHelpers.Helpers.LoadDependencies(TypeCobolConfiguration.Dependencies, TypeCobolConfiguration.Format, _customSymbols, TypeCobolConfiguration.InputFiles, DiagnosticsErrorEvent); //Refresh Dependencies

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

                AnalyticsWrapper.Telemetry.TrackException(typeCobolException);

                if (typeCobolException.NeedMail)
                    AnalyticsWrapper.Telemetry.SendMail(typeCobolException, TypeCobolConfiguration.InputFiles, TypeCobolConfiguration.CopyFolders, TypeCobolConfiguration.CommandLine);
            }
            catch (Exception e)
            {
                LoadingIssueEvent(null, new LoadingIssueEvent() { Message = "An error occured while trying to load Intrinsics or Dependencies files." }); //Send notification to client

                AnalyticsWrapper.Telemetry.TrackException(e);
                AnalyticsWrapper.Telemetry.SendMail(e, TypeCobolConfiguration.InputFiles, TypeCobolConfiguration.CopyFolders, TypeCobolConfiguration.CommandLine);
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
            var fileUri = OpenedFileCompiler.Keys.FirstOrDefault(k => compilationUnit != null && k.LocalPath.Contains(compilationUnit.TextSourceInfo.Name));

            var diags = compilationUnit?.AllDiagnostics().Take(TypeCobolConfiguration.MaximumDiagnostics == 0 ? 100 : TypeCobolConfiguration.MaximumDiagnostics);
            DiagnosticsEvent(fileUri, new DiagnosticEvent() { Diagnostics = diags});

            if (compilationUnit?.MissingCopies.Count > 0)
                MissingCopiesEvent(fileUri, new MissingCopiesEvent() { Copies = compilationUnit.MissingCopies.Select(c => c.TextName).Distinct().ToList() });
        }


       

  
    }

    public static class LsrTestingOptionsExtensions
    {
        public static bool HasFlag(this Workspace.LsrTestingOptions value, Workspace.LsrTestingOptions flag)
        {
            return (value & flag) != 0;
        }
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
