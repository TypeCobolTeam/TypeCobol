using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using Analytics;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.CustomExceptions;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.LanguageServices.Editor
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

        private SymbolTable CustomSymbols;
        private string RootDirectoryFullName;
        private string WorkspaceName;
        private CompilationProject CompilationProject;
        private string[] Extensions = { ".cbl", ".cpy" };


        private TypeCobolConfiguration TypeCobolConfiguration { get; set; }
        public Dictionary<Uri, FileCompiler> OpenedFileCompiler{ get; private set; }
        public EventHandler<DiagnosticEvent> DiagnosticsEvent { get; set; }
        public EventHandler<MissingCopiesEvent> MissingCopiesEvent { get; set; }
        public EventHandler<LoadingIssueEvent> LoadingIssueEvent { get; set; }


        public Workspace(string rootDirectoryFullName, string workspaceName)
        {
            TypeCobolConfiguration = new TypeCobolConfiguration();
            OpenedFileCompiler = new Dictionary<Uri, FileCompiler>();

            this.RootDirectoryFullName = rootDirectoryFullName;
            this.WorkspaceName = workspaceName;

            this.CompilationProject = new CompilationProject(
                WorkspaceName, RootDirectoryFullName, Extensions,
                Encoding.GetEncoding("iso-8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat,
                new TypeCobolOptions()); //Initialize a default CompilationProject - has to be recreated after ConfigurationChange Notification

        }

        /// <summary>
        /// Start continuous background compilation on a newly opened file
        /// </summary>
        public void OpenSourceFile(Uri fileUri, string sourceText)
        {
            string fileName = Path.GetFileName(fileUri.LocalPath);
            ITextDocument initialTextDocumentLines = new ReadOnlyTextDocument(fileName, TypeCobolConfiguration.Format.Encoding, TypeCobolConfiguration.Format.ColumnsLayout, sourceText);
            var fileCompiler = new FileCompiler(initialTextDocumentLines, CompilationProject.SourceFileProvider, CompilationProject, CompilationProject.CompilationOptions, CustomSymbols, false, CompilationProject);
            fileCompiler.CompilationResultsForProgram.UpdateTokensLines();

            lock (OpenedFileCompiler)
            {
                if (OpenedFileCompiler.ContainsKey(fileUri))
                    CloseSourceFile(fileUri); //Close and remove the previous opened file.

                OpenedFileCompiler.Add(fileUri, fileCompiler);
                fileCompiler.CompilationResultsForProgram.ProgramClassChanged += ProgramClassChanged;
            }

            fileCompiler.CompilationResultsForProgram.SetOwnerThread(Thread.CurrentThread);
            fileCompiler.StartContinuousBackgroundCompilation(200, 500, 1000, 3000); //TODO: create a better refresh compilation
        }

        /// <summary>
        /// Update the text contents of the file
        /// </summary>
        public void UpdateSourceFile(Uri fileUri, TextChangedEvent textChangedEvent, bool bAsync)
        {
            FileCompiler fileCompilerToUpdate = null;
            if (OpenedFileCompiler.TryGetValue(fileUri, out fileCompilerToUpdate))
            {
                fileCompilerToUpdate.CompilationResultsForProgram.UpdateTextLines(textChangedEvent);
                if (!bAsync)
                {//Don't wait asynchroneous snapshot refresh.
                    fileCompilerToUpdate.CompilationResultsForProgram.UpdateTokensLines(
                        () =>
                            {
                                fileCompilerToUpdate.CompilationResultsForProgram.RefreshTokensDocumentSnapshot();
                                fileCompilerToUpdate.CompilationResultsForProgram.RefreshProcessedTokensDocumentSnapshot();
                                fileCompilerToUpdate.CompilationResultsForProgram.RefreshCodeElementsDocumentSnapshot();
                                fileCompilerToUpdate.CompilationResultsForProgram.RefreshProgramClassDocumentSnapshot();
                            }
                        );
                }
                else
                {
                    fileCompilerToUpdate.CompilationResultsForProgram.UpdateTokensLines();
                }
            }
        }

        /// <summary>
        /// Stop continuous background compilation after a file has been closed
        /// </summary>
        public void CloseSourceFile(Uri fileUri)
        {
            FileCompiler fileCompilerToClose = null;
            lock (OpenedFileCompiler)
            {
                if (OpenedFileCompiler.ContainsKey(fileUri))
                {
                    fileCompilerToClose = OpenedFileCompiler[fileUri];
                    OpenedFileCompiler.Remove(fileUri);
                    fileCompilerToClose.StopContinuousBackgroundCompilation();
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
            options.Parse(arguments);

            //Adding default copies folder
            var folder = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName);
            TypeCobolConfiguration.CopyFolders.Add(folder + @"\DefaultCopies\");

            if (TypeCobolConfiguration.Telemetry)
                AnalyticsWrapper.Telemetry.DisableTelemetry = false; //If telemetry arg is passed enable telemetry

            if (TypeCobolConfiguration.ExecToStep >= ExecutionStep.Generate)
                TypeCobolConfiguration.ExecToStep = ExecutionStep.SemanticCheck; //Language Server does not support Cobol Generation for now

            var typeCobolOptions = new TypeCobolOptions
            {
                HaltOnMissingCopy = TypeCobolConfiguration.HaltOnMissingCopyFilePath != null,
                ExecToStep = TypeCobolConfiguration.ExecToStep,
#if EUROINFO_RULES
                AutoRemarksEnable = TypeCobolConfiguration.AutoRemarks
#endif
            };

            CompilationProject = new CompilationProject(WorkspaceName, RootDirectoryFullName, Extensions, TypeCobolConfiguration.Format.Encoding, TypeCobolConfiguration.Format.EndOfLineDelimiter, TypeCobolConfiguration.Format.FixedLineLength, TypeCobolConfiguration.Format.ColumnsLayout, typeCobolOptions);

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
        private void RefreshOpenedFiles()
        {
            RefreshCustomSymbols();

            lock(OpenedFileCompiler)
            {
                var tempOpenedFileCompiler = new Dictionary<Uri, FileCompiler>(OpenedFileCompiler);
                foreach (var fileParser in tempOpenedFileCompiler)
                {
                    OpenSourceFile(fileParser.Key, fileParser.Value.TextDocument.TextSegment(0, fileParser.Value.TextDocument.Length - 1));
                }
            }
        }

        private void RefreshCustomSymbols()
        {
            bool diagDetected = false;
            EventHandler<Tools.APIHelpers.DiagnosticsErrorEvent> DiagnosticsErrorEvent = null;
            DiagnosticsErrorEvent += delegate (object sender, Tools.APIHelpers.DiagnosticsErrorEvent diagEvent)
            {
                //Delegate Event to handle diagnostics generated while loading dependencies/intrinsics
                diagDetected = true;
            };
            CustomSymbols = null;
            try
            {
                CustomSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(TypeCobolConfiguration.Copies, TypeCobolConfiguration.Format, DiagnosticsErrorEvent); //Refresh Intrinsics
                CustomSymbols = Tools.APIHelpers.Helpers.LoadDependencies(TypeCobolConfiguration.Dependencies, TypeCobolConfiguration.Format, CustomSymbols, TypeCobolConfiguration.InputFiles, DiagnosticsErrorEvent); //Refresh Dependencies

                if(diagDetected)
                    LoadingIssueEvent(null, new LoadingIssueEvent() {Message = "An error occured while trying to load Intrinsics or Dependencies files."}); //Send notification to client
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
            var fileUri = OpenedFileCompiler.Keys.FirstOrDefault(k => k.LocalPath.Contains(compilationUnit.TextSourceInfo.Name));

            var diags = compilationUnit.AllDiagnostics().Take(TypeCobolConfiguration.MaximumDiagnostics == 0 ? 100 : TypeCobolConfiguration.MaximumDiagnostics);
            DiagnosticsEvent(fileUri, new DiagnosticEvent() { Diagnostics = diags});

            if (compilationUnit.MissingCopies.Count > 0)
                MissingCopiesEvent(fileUri, new MissingCopiesEvent() { Copies = compilationUnit.MissingCopies.Select(c => c.TextName).Distinct().ToList() });
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
