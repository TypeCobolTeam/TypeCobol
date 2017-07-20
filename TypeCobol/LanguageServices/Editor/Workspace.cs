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
        public EventHandler<IList<Compiler.Diagnostics.Diagnostic>> DiagnosticsEvent { get; set; }
        public EventHandler<List<string>> MissingCopiesEvent { get; set; }


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
            var options = TypeCobolOptionSet.GetCommonTypeCobolOptions(TypeCobolConfiguration);
            options.Parse(settings.Split(' '));

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

            if (OpenedFileCompiler.Count > 0)
                RefreshOpenedFiles();
            else
                RefreshCustomSymbols();
        }

        public void UpdateMissingCopies(List<string> RemainingMissingCopies)
        {
            if(RemainingMissingCopies == null || RemainingMissingCopies.Count == 0)
            {
                CompilationProject.MissingCopys.RemoveAll(c => true);
                return;
            }

            var copiesToRemove = CompilationProject.MissingCopys.Except(RemainingMissingCopies);
            foreach (var copyToRemove in copiesToRemove)
                CompilationProject.MissingCopys.Remove(copyToRemove);
        }

        /// <summary>
        /// Refresh all opened files' parser.
        /// </summary>
        private void RefreshOpenedFiles()
        {
            RefreshCustomSymbols();

            foreach (var FileParser in OpenedFileCompiler)
            {
                OpenSourceFile(FileParser.Key, FileParser.Value.TextDocument.TextSegment(0, FileParser.Value.TextDocument.Length-1));
            }
        }

        private void RefreshCustomSymbols()
        {
            CustomSymbols = null;
            CustomSymbols = LoadIntrinsic(TypeCobolConfiguration.Copies, TypeCobolConfiguration.Format); //Refresh Intrinsics
            CustomSymbols = LoadDependencies(TypeCobolConfiguration.Dependencies, TypeCobolConfiguration.Format, CustomSymbols); //Refresh Dependencies
        }

        /// <summary>
        /// Called by a ProgramClass changed event trigger. 
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e"></param>
        private void ProgramClassChanged(object cUnit, int version)
        {
            var compilationUnit = cUnit as CompilationUnit;
            var fileUri = OpenedFileCompiler.Keys.FirstOrDefault(k => k.LocalPath.Contains(compilationUnit.TextSourceInfo.Name));

            var diags = compilationUnit.AllDiagnostics();
            if (diags.Count > 0)
                DiagnosticsEvent(fileUri, diags);
            if (CompilationProject.MissingCopys.Count > 0)
                MissingCopiesEvent(fileUri, CompilationProject.MissingCopys);
        }


        private SymbolTable LoadIntrinsic(List<string> paths, DocumentFormat intrinsicDocumentFormat)
        {
            var parser = new Parser();
            var table = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
            var instrincicFiles = new List<string>();

            foreach (string path in paths) instrincicFiles.AddRange(Tools.FileSystem.GetFiles(path, parser.Extensions, false));

            foreach (string path in instrincicFiles)
            {
                //try
                //{
                    parser.Init(path, new TypeCobolOptions { ExecToStep = ExecutionStep.SemanticCheck }, intrinsicDocumentFormat);
                    parser.Parse(path);

                    var diagnostics = parser.Results.AllDiagnostics();


                    foreach (var diagnostic in diagnostics)
                    {
                        //TODO: Send diagnostics to client...
                    }

                    //if (diagnostics.Count > 0)
                    //    throw new CopyLoadingException("Diagnostics detected while parsing Intrinsic file", path, null, logged: false, needMail: false);


                    //if (parser.Results.ProgramClassDocumentSnapshot.Root.Programs == null || parser.Results.ProgramClassDocumentSnapshot.Root.Programs.Count() == 0)
                    //{
                    //    throw new CopyLoadingException("Your Intrisic types/functions are not included into a program.", path, null, logged: true, needMail: false);
                    //}

                    foreach (var program in parser.Results.ProgramClassDocumentSnapshot.Root.Programs)
                    {
                        var symbols = program.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations);

                        //if (symbols.Types.Count == 0 && symbols.Functions.Count == 0)
                        //{
                        //    Server.AddError(writer, MessageCode.Warning, "No types and no procedures/functions found", path);
                        //    continue;
                        //}

                        table.CopyAllTypes(symbols.Types);
                        table.CopyAllFunctions(symbols.Functions);
                    }
                //}
                //catch (CopyLoadingException copyException)
                //{
                //    throw copyException; //Make CopyLoadingException trace back to runOnce()
                //}
                //catch (Exception e)
                //{
                //    throw new CopyLoadingException(e.Message + "\n" + e.StackTrace, path, e, logged: true, needMail: true);
                //}
            }
            return table;
        }

        private SymbolTable LoadDependencies(List<string> paths, DocumentFormat format, SymbolTable intrinsicTable)
        {
            var parser = new Parser(intrinsicTable);
            var table = new SymbolTable(intrinsicTable, SymbolTable.Scope.Namespace); //Generate a table of NameSPace containing the dependencies programs based on the previously created intrinsic table. 

            var dependencies = new List<string>();
            string[] extensions = { ".tcbl", ".cbl", ".cpy" };
            foreach (var path in paths)
            {
                dependencies.AddRange(Tools.FileSystem.GetFiles(path, extensions, true)); //Get File by name or search the directory for all files
            }

            foreach (string path in dependencies)
            {
                //try
                //{
                    parser.Init(path, new TypeCobolOptions { ExecToStep = ExecutionStep.SemanticCheck }, format);
                    parser.Parse(path); //Parse the dependencie file

                    var diagnostics = parser.Results.AllDiagnostics();
                    foreach (var diagnostic in diagnostics)
                    {
                        //TODO : Send diagnostics to client...
                    }
                    //if (diagnostics.Count > 0)
                    //    throw new DepedenciesLoadingException("Diagnostics detected while parsing dependency file", path, null, logged: false, needMail: false);

                    //if (parser.Results.ProgramClassDocumentSnapshot.Root.Programs == null || parser.Results.ProgramClassDocumentSnapshot.Root.Programs.Count() == 0)
                    //{
                    //    throw new DepedenciesLoadingException("Your dependency file is not included into a program", path, null, logged: true, needMail: false);
                    //}

                    foreach (var program in parser.Results.ProgramClassDocumentSnapshot.Root.Programs)
                    {
                        table.AddProgram(program); //Add program to Namespace symbol table
                    }

                //}
                //catch (DepedenciesLoadingException)
                //{
                //    throw; //Make DepedenciesLoadingException trace back to runOnce()
                //}
                //catch (Exception e)
                //{
                //    throw new DepedenciesLoadingException(e.Message + "\n" + e.StackTrace, path, e);
                //}
            }
            return table;
        }
    }
}
