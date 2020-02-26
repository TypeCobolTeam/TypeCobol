﻿using Analytics;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.CLI.CustomExceptions;
using TypeCobol.Codegen;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Report;
using TypeCobol.Compiler.Text;
using TypeCobol.CustomExceptions;
using TypeCobol.Tools.APIHelpers;
using TypeCobol.Tools.Options_Config;

using static TypeCobol.Tools.APIHelpers.Helpers;

namespace TypeCobol.Server
{
    /// <summary>
    /// CLI class contains runOnce method & other private methods to parse.
    /// </summary>
    class CLI
    {
        /// <summary>
        /// runOnce method to parse the input file(s).
        /// </summary>
        /// <param name="config">Config</param>
        internal static ReturnCode runOnce(TypeCobolConfiguration config)
        {
            Stopwatch stopWatch = new Stopwatch();
            stopWatch.Start();
            string debugLine = DateTime.Now + " start parsing of ";
            if (config.InputFiles.Count > 0)
            {
                debugLine += Path.GetFileName(config.InputFiles[0]);
            }
            debugLine += "\n";
            //Use user-defined log path if -log option used, otherwise use default location for log file
            File.AppendAllText(config.LogFile ?? TypeCobolConfiguration.DefaultLogFileName, debugLine);
            Console.WriteLine(debugLine);
            TextWriter textWriter = config.ErrorFile == null ?  Console.Error : File.CreateText(config.ErrorFile);
            AbstractErrorWriter errorWriter = config.IsErrorXML ? (AbstractErrorWriter) new XMLWriter(textWriter) : new ConsoleWriter(textWriter);
            errorWriter.Outputs = config.OutputFiles;

            ReturnCode returnCode;
            try
            {
                CLI cli = new CLI(config, errorWriter);
                returnCode = cli.Compile();
            }
            catch (Exception unexpected)
            {
                var typeCobolException = new ParsingException(MessageCode.GenerationFailled, null, config.InputFiles.FirstOrDefault(), unexpected);
                AnalyticsWrapper.Telemetry.TrackException(typeCobolException, typeCobolException.Path);
                AnalyticsWrapper.Telemetry.SendMail(unexpected, config.InputFiles, config.CopyFolders, config.CommandLine);

                Server.AddError(errorWriter, MessageCode.SyntaxErrorInParser, unexpected.Message + Environment.NewLine + unexpected.StackTrace, string.Empty);
                returnCode = ReturnCode.FatalError;
            }

            errorWriter.Write(returnCode);
            errorWriter.FlushAndClose();
            //as textWriter can be a Text file created, we need to close it
            textWriter.Close();

            stopWatch.Stop();
            debugLine = "                         parsed in " + stopWatch.Elapsed + " ms\n";
            //Use user-defined log path if -log option used, otherwise use default location for log file
            File.AppendAllText(config.LogFile ?? TypeCobolConfiguration.DefaultLogFileName, debugLine);
            Console.WriteLine(debugLine);

            AnalyticsWrapper.Telemetry.TrackMetricsEvent(EventType.Duration, LogType.Genration, "ExecutionTime", stopWatch.Elapsed.Milliseconds);

            return returnCode;
        }

        private readonly TypeCobolConfiguration _configuration;
        private readonly AbstractErrorWriter _errorWriter;
        private readonly List<DiagnosticsErrorEvent> _intrinsicsDiagnostics;
        private readonly List<DiagnosticsErrorEvent> _dependenciesDiagnostics;
        private readonly Dictionary<string, CompilationUnit> _parserResults;
        private readonly Dictionary<string, IList<Diagnostic>> _inputsDiagnosticsCache;
        private readonly HashSet<string> _usedCopies;
        private readonly HashSet<string> _missingCopies;
        private readonly Dictionary<string, List<GenerationException>> _generationExceptions;

        private CLI(TypeCobolConfiguration configuration, AbstractErrorWriter errorWriter)
        {
            _configuration = configuration;
            _errorWriter = errorWriter;
            _intrinsicsDiagnostics = new List<DiagnosticsErrorEvent>();
            _dependenciesDiagnostics = new List<DiagnosticsErrorEvent>();
            _parserResults = new Dictionary<string, CompilationUnit>(StringComparer.InvariantCultureIgnoreCase);
            _inputsDiagnosticsCache = new Dictionary<string, IList<Diagnostic>>(StringComparer.InvariantCultureIgnoreCase);
            _usedCopies = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            _missingCopies = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
            _generationExceptions = new Dictionary<string, List<GenerationException>>(StringComparer.InvariantCultureIgnoreCase);
        }

        private ReturnCode Compile()
        {
            //Load intrinsics and dependencies, it will build the root symbol table
            var rootSymbolTable = LoadIntrinsicsAndDependencies();

            //Add report listeners
            var reports = InitializeReports();

            //Normalize TypeCobolOptions, the parser does not need to go beyond SemanticCheck for the first phase
            var typeCobolOptions = new TypeCobolOptions(_configuration);
            if (_configuration.ExecToStep > ExecutionStep.SemanticCheck)
            {
                typeCobolOptions.ExecToStep = ExecutionStep.SemanticCheck;
            }

            //First phase : parse all inputs but do not make CrossCheck yet
            foreach (var inputFilePath in _configuration.InputFiles)
            {
                var parser = new Parser(rootSymbolTable);
                parser.Init(inputFilePath, typeCobolOptions, _configuration.Format, _configuration.CopyFolders);
                parser.Parse(inputFilePath);

                //Collect results : parsing results, used and missing copies
                _parserResults.Add(inputFilePath, parser.Results);
                CollectUsedCopies(parser.Results.CopyTextNamesVariations);
                CollectMissingCopies(parser.MissingCopys);

                //Add newly discovered programs in the root symbol table
                if (parser.Results.TemporaryProgramClassDocumentSnapshot != null)
                {
                    AddProgramsToRootTable(rootSymbolTable, parser.Results.TemporaryProgramClassDocumentSnapshot.Root.Programs);
                }
                
                //Optionally generate the source with expanded copy (note that it is not supported for multiple files)
                GenerateExpandingCopyFile(inputFilePath, parser.Results);
            }

            //Second phase : now that we have all known programs in the table, we can launch a CrossCheck
            if (_configuration.ExecToStep > ExecutionStep.SemanticCheck)
            {
                int fileIndex = 0;
                foreach (var parserResult in _parserResults)
                {
                    var inputFilePath = parserResult.Key;
                    var compilationUnit = parserResult.Value; 

                    //Force CrossCheck
                    compilationUnit.RefreshProgramClassDocumentSnapshot();

                    //Since collecting diagnostics may be costly, we cache them here
                    var currentFileDiagnostics = compilationUnit.AllDiagnostics();
                    _inputsDiagnosticsCache.Add(inputFilePath, currentFileDiagnostics);

                    //No need to go further if we have errors after cross-checking
                    bool currentFileHasErrors = currentFileDiagnostics.Any(diagnostic => diagnostic.Info.Severity == Severity.Error);
                    if (!currentFileHasErrors)
                    {
                        //Generate reports
                        CreateReports(inputFilePath, reports);

                        //Generate COBOL code
                        if (_configuration.ExecToStep >= ExecutionStep.Generate)
                        {
                            Generate(inputFilePath, fileIndex, compilationUnit);
                        }
                    }

                    //Increment file index to keep in-sync with output files
                    fileIndex++;
                }
            }

            //Write used and missing copies files
            WriteCopiesFile(_configuration.ExtractedCopiesFilePath, _usedCopies);
            WriteCopiesFile(_configuration.HaltOnMissingCopyFilePath, _missingCopies);

            return AddErrorsAndComputeReturnCode();
        }

        private SymbolTable LoadIntrinsicsAndDependencies()
        {
            var intrinsicsAndDependenciesParser = new Parser();

            intrinsicsAndDependenciesParser.CustomSymbols = LoadIntrinsic(_configuration.Copies, _configuration.Format, OnDiagnosticsInIntrinsics);
            intrinsicsAndDependenciesParser.CustomSymbols = LoadDependencies(_configuration, intrinsicsAndDependenciesParser.CustomSymbols, OnDiagnosticsInDependencies,
                out List<RemarksDirective.TextNameVariation> usedCopies,
                out IDictionary<string, IEnumerable<string>> missingCopies);

            CollectUsedCopies(usedCopies);
            CollectMissingCopies(missingCopies.SelectMany(fileMissingCopiesPair => fileMissingCopiesPair.Value));

            return intrinsicsAndDependenciesParser.CustomSymbols;

            //Local functions to collect Diagnostics in Intrinsics and Dependencies
            void OnDiagnosticsInIntrinsics(object sender, DiagnosticsErrorEvent diagnosticsErrorEvent)
            {
                _intrinsicsDiagnostics.Add(diagnosticsErrorEvent);
            }
            void OnDiagnosticsInDependencies(object sender, DiagnosticsErrorEvent diagnosticsErrorEvent)
            {
                _dependenciesDiagnostics.Add(diagnosticsErrorEvent);
            }
        }

        private void CollectUsedCopies(IEnumerable<RemarksDirective.TextNameVariation> usedCopies)
        {
            foreach (var textNameVariation in usedCopies)
            {
#if EUROINFO_RULES
                string copyName = _configuration.UseEuroInformationLegacyReplacingSyntax
                    ? textNameVariation.TextName
                    : textNameVariation.TextNameWithSuffix;
#else
                string copyName = textNameVariation.TextNameWithSuffix;
#endif
                _usedCopies.Add(copyName);
            }
        }

        private void CollectMissingCopies(IEnumerable<string> missingCopies)
        {
            foreach (var missingCopy in missingCopies)
            {
                _missingCopies.Add(missingCopy);
            }
        }

        private List<AbstractReport> InitializeReports()
        {
            List<AbstractReport> reports = new List<AbstractReport>();
            if (_configuration.ExecToStep >= ExecutionStep.CrossCheck)
            {
                if (!string.IsNullOrEmpty(_configuration.ReportCopyMoveInitializeFilePath))
                {
                    Compiler.Parser.NodeDispatcher.RegisterStaticNodeListenerFactory(
                        () =>
                        {
                            var report = new CopyMoveInitializeReport(_configuration.ReportCopyMoveInitializeFilePath);
                            reports.Add(report);
                            return report;
                        });

                }

                if (!string.IsNullOrEmpty(_configuration.ReportZCallFilePath))
                {
                    Compiler.Parser.NodeDispatcher.RegisterStaticNodeListenerFactory(
                        () =>
                        {
                            var report = new ZCallPgmReport(_configuration.ReportZCallFilePath);
                            reports.Add(report);
                            return report;
                        });

                }
            }

            return reports;
        }

        private void AddProgramsToRootTable(SymbolTable rootTable, IEnumerable<Program> programs)
        {
            if (_configuration.ExecToStep >= ExecutionStep.SemanticCheck)
            {
                foreach (var program in programs.Where(p => p.IsMainProgram))
                {
                    // a stacked or a nested program should not be referenced from another source file
                    var previousPrograms = rootTable.GetPrograms();
                    foreach (var previousProgram in previousPrograms)
                    {
                        previousProgram.SymbolTable.GetTableFromScope(SymbolTable.Scope.Namespace).AddProgram(program);
                    }

                    rootTable.AddProgram(program); //Add program to Namespace symbol table
                }
            }
        }

        private void GenerateExpandingCopyFile(string inputFilePath, CompilationUnit compilationUnit)
        {
            if (_configuration.ExecToStep >= ExecutionStep.Preprocessor && !string.IsNullOrEmpty(_configuration.ExpandingCopyFilePath))
            {
                if (_configuration.InputFiles.Count > 1)
                {
                    throw new NotSupportedException("Generation of source with expanded copies is not supported for multiple input files.");
                }

                try
                {
                    StringBuilder output = new StringBuilder();
                    var generator = GeneratorFactoryManager.Instance.Create(OutputFormat.ExpandingCopy.ToString(), compilationUnit, output, null, null, false);
                    var streamWriter = new StreamWriter(_configuration.ExpandingCopyFilePath);
                    generator.Generate(compilationUnit, ColumnsLayout.CobolReferenceFormat);
                    streamWriter.Write(output);
                    streamWriter.Flush();
                    streamWriter.Close();
                }
                catch (Exception exception)
                {
                    var generationException = new GenerationException(exception.Message, inputFilePath, exception);
                    StoreGenerationException(inputFilePath, generationException);
                }
            }
        }

        private void StoreGenerationException(string inputFilePath, GenerationException generationException)
        {
            if (!_generationExceptions.TryGetValue(inputFilePath, out var generationExceptions))
            {
                generationExceptions = new List<GenerationException>();
                _generationExceptions.Add(inputFilePath, generationExceptions);
            }

            generationExceptions.Add(generationException);
        }

        private void CreateReports(string inputFilePath, IEnumerable<AbstractReport> reports)
        {
            foreach (var report in reports)
            {
                try
                {
                    report.Report();
                    Console.WriteLine($"Succeed to emit report '{report.Filepath}'");
                }
                catch (Exception exception)
                {
                    Console.Error.WriteLine(exception.Message);
                    var generationException = new GenerationException(exception.Message, report.Filepath, exception);
                    StoreGenerationException(inputFilePath, generationException);
                }
            }
        }

        private void Generate(string inputFilePath, int fileIndex, CompilationUnit compilationUnit)
        {
            try
            {
                //Load skeletons if necessary
                List<Skeleton> skeletons = null;
                if (!string.IsNullOrEmpty(_configuration.skeletonPath))
                {
                    skeletons = Codegen.Config.Config.Parse(_configuration.skeletonPath);
                }

                //Get Generator from specified config.OutputFormat
                var sb = new StringBuilder();
                bool needLineMap = _configuration.LineMapFiles.Count > fileIndex;
                var generator = GeneratorFactoryManager.Instance.Create(_configuration.OutputFormat.ToString(),
                    compilationUnit,
                    sb, skeletons, AnalyticsWrapper.Telemetry.TypeCobolVersion, needLineMap);
                if (generator == null)
                {
                    throw new GenerationException("Unknown OutputFormat=" + _configuration.OutputFormat + "_", inputFilePath);
                }

                //Generate and check diagnostics
                generator.Generate(compilationUnit, ColumnsLayout.CobolReferenceFormat);
                if (generator.Diagnostics != null)
                {
                    throw new PresenceOfDiagnostics("Diagnostics Detected", inputFilePath);
                }

                //Check for existing lock
                var outputDirectory = new FileInfo(_configuration.OutputFiles[fileIndex]).Directory;
                var lockFilePath = outputDirectory.FullName + Path.DirectorySeparatorChar + "~.lock";
                if (File.Exists(lockFilePath))
                {
                    throw new GenerationException("Generation lock file already exists.", inputFilePath);
                }

                //Output Line Map Data.
                if (needLineMap && generator.HasLineMapData)
                {
                    using (var lineMapFileStream = new FileStream(_configuration.LineMapFiles[fileIndex], FileMode.Create))
                    {
                        try
                        {
                            generator.GenerateLineMapFile(lineMapFileStream);
                        }
                        catch (Exception e)
                        {
                            //Failed to generate Line Map File
                            Console.WriteLine($"Fail to generate Line Mapping File {_configuration.LineMapFiles[fileIndex]} : {e.Message}");
                        }
                        finally
                        {
                            lineMapFileStream.Close();
                        }
                    }
                }

                //Create lock file
                var lockWriter = new StreamWriter(lockFilePath);
                lockWriter.Flush();
                lockWriter.Close();

                //Write generated code to file
                using (var streamWriter = new StreamWriter(_configuration.OutputFiles[fileIndex]))
                {
                    try
                    {
                        streamWriter.Write(sb); //Write generated Cobol code inside file
                        streamWriter.Flush();
                    }
                    finally
                    {
                        File.Delete(lockFilePath); //Remove lock to allow watchers to read the file
                        streamWriter.Close();
                    }
                }
            }
            catch (Exception e)
            {
                var generationException = new GenerationException(e.Message, inputFilePath, e);
                StoreGenerationException(inputFilePath, generationException);
            }
        }

        private void WriteCopiesFile(string copiesFilePath, IEnumerable<string> copies)
        {
            if (!string.IsNullOrEmpty(copiesFilePath))
            {
                File.Delete(copiesFilePath);
                File.WriteAllLines(copiesFilePath, copies);
            }
        }

        private ReturnCode AddErrorsAndComputeReturnCode()
        {
            /*
             * Here we start with a Sucesss return code, then we check all diagnostics from
             * lowest to highest priority.
             */

            ReturnCode returnCode = ReturnCode.Success;

            foreach (var inputFilePath in _configuration.InputFiles)
            {
                //Update the diagnostics cache
                if (!_inputsDiagnosticsCache.TryGetValue(inputFilePath, out var diagnostics))
                {
                    diagnostics = _parserResults[inputFilePath].AllDiagnostics();
                }

                //Do not output more diagnostics than allowed
                var trimmedDiagnostics = diagnostics.Take(_configuration.MaximumDiagnostics == 0
                    ? diagnostics.Count
                    : _configuration.MaximumDiagnostics);
                _errorWriter.AddErrors(inputFilePath, trimmedDiagnostics);

                //Analytics
                foreach (var diagnostic in diagnostics)
                {
                    if (diagnostic.CatchedException != null)
                    {
                        AnalyticsWrapper.Telemetry.TrackException(diagnostic.CatchedException, inputFilePath);
                        AnalyticsWrapper.Telemetry.SendMail(diagnostic.CatchedException, _configuration.InputFiles,
                            _configuration.CopyFolders, _configuration.CommandLine);
                    }
                }

                UpdateReturnCode(diagnostics);

                if (_generationExceptions.TryGetValue(inputFilePath, out var generationExceptions))
                {
                    returnCode = ReturnCode.GenerationError;

                    foreach (var generationException in generationExceptions)
                    {
                        AnalyticsWrapper.Telemetry.TrackException(generationException, generationException.Path);

                        if (generationException.NeedMail)
                        {
                            AnalyticsWrapper.Telemetry.SendMail(generationException, _configuration.InputFiles, _configuration.CopyFolders, _configuration.CommandLine);
                        }

                        if (generationException.Logged)
                        {
                            Server.AddError(_errorWriter, generationException.MessageCode,
                                generationException.ColumnStartIndex,
                                generationException.ColumnEndIndex, generationException.LineNumber,
                                generationException.Message + "\n" + generationException.StackTrace,
                                generationException.Path);
                        }
                    }
                }
            }

            //Check diagnostics in dependencies
            if (_dependenciesDiagnostics.Count > 0)
            {
                foreach (var diagnosticsErrorEvent in _dependenciesDiagnostics)
                {
                    Server.AddError(_errorWriter, diagnosticsErrorEvent.Path, diagnosticsErrorEvent.Diagnostic);
                }
                UpdateReturnCode(_dependenciesDiagnostics.Select(dee => dee.Diagnostic).ToList());
            }

            //Check diagnostics in intrinsics
            if (_intrinsicsDiagnostics.Count > 0)
            {
                foreach (var diagnosticsErrorEvent in _intrinsicsDiagnostics)
                {
                    var diagnostic = diagnosticsErrorEvent.Diagnostic;
                    Server.AddError(_errorWriter, MessageCode.IntrinsicLoading, diagnostic.ColumnStart, diagnostic.ColumnEnd,
                        diagnostic.Line, "Error while parsing " + diagnosticsErrorEvent.Path + ": " + diagnostic,
                        diagnosticsErrorEvent.Path);
                }
                UpdateReturnCode(_intrinsicsDiagnostics.Select(dee => dee.Diagnostic).ToList());
            }

            //Always retrun MissingCopy when there is at least one missing copy because it could help the developer to correct several parsing errors at once
            if (_missingCopies.Count > 0)
            {
                returnCode = ReturnCode.MissingCopy;
            }

            return returnCode;

            //Local function to update the current result with a new batch of diagnostics
            void UpdateReturnCode(IList<Diagnostic> diagnostics)
            {
                if (returnCode < ReturnCode.Warning && diagnostics.Any(diagnostic => diagnostic.Info.Severity == Severity.Warning))
                {
                    returnCode = ReturnCode.Warning;
                }
                if (returnCode < ReturnCode.ParsingDiagnostics && diagnostics.Any(diagnostic => diagnostic.Info.Severity == Severity.Error))
                {
                    returnCode = ReturnCode.ParsingDiagnostics;
                }
            }
        }
    }
}
