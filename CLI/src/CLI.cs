using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using TypeCobol.CustomExceptions;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using Analytics;
using TypeCobol.CLI.CustomExceptions;
using System.Linq;
using System.Text;
using TypeCobol.Codegen;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Tools.Options_Config;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Report;

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
        internal static ReturnCode runOnce(TypeCobolConfiguration config) {
            Stopwatch stopWatch = new Stopwatch();
            stopWatch.Start();
            string debugLine = DateTime.Now + " start parsing of ";
            if (config.InputFiles.Count > 0)
            {
                debugLine += Path.GetFileName(config.InputFiles[0]);
            }
            debugLine += "\n";
            File.AppendAllText("TypeCobol.CLI.log", debugLine);
            Console.WriteLine(debugLine);

            TextWriter textWriter;
            if (config.ErrorFile == null) textWriter = Console.Error;
            else textWriter = File.CreateText(config.ErrorFile);
            AbstractErrorWriter errorWriter;
            if (config.IsErrorXML) errorWriter = new XMLWriter(textWriter);
            else errorWriter = new ConsoleWriter(textWriter);
            errorWriter.Outputs = config.OutputFiles;

            //Call the runOnce2() Methode and manage all the different kinds of exception. 

            ReturnCode returnCode;
            try
            {
                returnCode =  runOnce2(config, errorWriter);
            }
            catch(TypeCobolException typeCobolException)//Catch managed exceptions
            {
                AnalyticsWrapper.Telemetry.TrackException(typeCobolException, typeCobolException.Path);

                if (typeCobolException.NeedMail)
                    AnalyticsWrapper.Telemetry.SendMail(typeCobolException, config.InputFiles, config.CopyFolders, config.CommandLine);

                if (typeCobolException.Logged)
                {
                    Server.AddError(errorWriter, typeCobolException.MessageCode, typeCobolException.ColumnStartIndex,
                        typeCobolException.ColumnEndIndex, typeCobolException.LineNumber,
                        typeCobolException.Message + "\n" + typeCobolException.StackTrace, typeCobolException.Path);
                }
               

                if (typeCobolException is PresenceOfDiagnostics)
                    returnCode = ReturnCode.ParsingDiagnostics;
                else if (typeCobolException is ParsingException)
                    returnCode = ReturnCode.FatalError;
                else if(typeCobolException is GenerationException)
                    returnCode = ReturnCode.GenerationError;
                else if(typeCobolException is MissingCopyException)
                    returnCode = ReturnCode.MissingCopy;
                else
                    returnCode = ReturnCode.FatalError; //Just in case..
            }
            catch (Exception e)//Catch any other exception
            {
                var typeCobolException = new ParsingException(MessageCode.GenerationFailled, null, config.InputFiles.FirstOrDefault(), e);
                AnalyticsWrapper.Telemetry.TrackException(typeCobolException, typeCobolException.Path);
                AnalyticsWrapper.Telemetry.SendMail(e, config.InputFiles, config.CopyFolders, config.CommandLine);

                Server.AddError(errorWriter, MessageCode.SyntaxErrorInParser, e.Message + e.StackTrace, string.Empty);
                returnCode= ReturnCode.FatalError;
            }
            

            errorWriter.Write(returnCode);
            errorWriter.FlushAndClose();
            errorWriter = null;
            //as textWriter can be a Text file created, we need to close it
            textWriter.Close();

            stopWatch.Stop();
            debugLine = "                         parsed in " + stopWatch.Elapsed + " ms\n";
            File.AppendAllText("TypeCobol.CLI.log", debugLine);
            Console.WriteLine(debugLine);

            AnalyticsWrapper.Telemetry.TrackMetricsEvent(EventType.Duration, LogType.Genration, "ExecutionTime", stopWatch.Elapsed.Milliseconds); 
            
            return returnCode;
        }

        private static ReturnCode runOnce2(TypeCobolConfiguration config, AbstractErrorWriter errorWriter)
        {
            if (!string.IsNullOrEmpty(config.HaltOnMissingCopyFilePath))
            {
                //Delete the file
                File.Delete(config.HaltOnMissingCopyFilePath);
            }

            SymbolTable baseTable = null;

            #region Dependencies parsing
            var depParser = new Parser();
            bool diagDetected = false;
            
            #region Event Diags Handler
            EventHandler<Tools.APIHelpers.DiagnosticsErrorEvent> DiagnosticsErrorEvent = delegate (object sender, Tools.APIHelpers.DiagnosticsErrorEvent diagEvent)
            {
                //Delegate Event to handle diagnostics generated while loading dependencies/intrinsics
                diagDetected = true;
                var diagnostic = diagEvent.Diagnostic;
                Server.AddError(errorWriter, MessageCode.IntrinsicLoading,
                    diagnostic.ColumnStart, diagnostic.ColumnEnd, diagnostic.Line,
                    "Error while parsing " + diagEvent.Path + ": " + diagnostic, diagEvent.Path);
            };
            EventHandler<Tools.APIHelpers.DiagnosticsErrorEvent> DependencyErrorEvent = delegate (object sender, Tools.APIHelpers.DiagnosticsErrorEvent diagEvent)
            {
                //Delegate Event to handle diagnostics generated while loading dependencies/intrinsics
                Server.AddError(errorWriter, diagEvent.Path, diagEvent.Diagnostic);
            };
            #endregion

            depParser.CustomSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(config.Copies, config.Format, DiagnosticsErrorEvent); //Load intrinsic
            depParser.CustomSymbols = Tools.APIHelpers.Helpers.LoadDependencies(config.Dependencies, config.Format, depParser.CustomSymbols, config.InputFiles, config.CopyFolders, DependencyErrorEvent, out List<RemarksDirective.TextNameVariation> usedCopies, out IDictionary<string, IEnumerable<string>> missingCopies); //Load dependencies

            //Create extracted copies file even if copy are missing
            CreateExtractedCopiesFile(config, usedCopies);

            if (missingCopies.Count > 0 && !string.IsNullOrEmpty(config.HaltOnMissingCopyFilePath))
            {
                //Collect the missing copies
                File.WriteAllLines(config.HaltOnMissingCopyFilePath, missingCopies.SelectMany(mc => mc.Value).Distinct());

                //depParser.CustomSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(config.Copies, config.Format, DiagnosticsErrorEvent); //Load intrinsic////////////////////
                //depParser.CustomSymbols = Tools.APIHelpers.Helpers.LoadDependencies(config, depParser.CustomSymbols, DependencyErrorEvent); //Load dependencies

                //If copies are missing, don't try to parse main input files
                //throw  an exception for the first dependency file
                throw new MissingCopyException("Copy are missing in dependencies", missingCopies.First().Key, null, logged: false, needMail: false);
            }
            

            if (diagDetected)
                throw new CopyLoadingException("Diagnostics detected while parsing Intrinsic file", null, null, logged: false, needMail: false);
            

            baseTable = depParser.CustomSymbols;
            #endregion

            var typeCobolOptions = new TypeCobolOptions(config);

#if EUROINFO_RULES
            typeCobolOptions.AutoRemarksEnable = config.AutoRemarks;
#endif

            ReturnCode returnCode = ReturnCode.Success;
            List<Parser> parsers = new List<Parser>();
            List<Compiler.Report.AbstractReport> reports = new List<AbstractReport>();
            bool copyAreMissing = false;
            List<Diagnostic> diagnostics = new List<Diagnostic>();

            foreach (var inputFilePath in config.InputFiles)
            {
                var parser = new Parser();
                parser.CustomSymbols = baseTable;
                parsers.Add(parser);

                if (config.ExecToStep > ExecutionStep.SemanticCheck) //If inferior to semantic, use the execstep given by the user.
                    typeCobolOptions.ExecToStep = ExecutionStep.SemanticCheck;

                try
                {
                    parser.Init(inputFilePath, typeCobolOptions, config.Format, config.CopyFolders); //Init parser create CompilationProject & Compiler before parsing the given file
                }
                catch (Exception ex)
                {
                    throw new ParsingException(MessageCode.ParserInit, ex.Message, inputFilePath, ex); //Make ParsingException trace back to RunOnce()
                }

                #region Copy Report Init

                if (config.ExecToStep >= ExecutionStep.CrossCheck)
                {
                    if (!string.IsNullOrEmpty(config.ReportCopyMoveInitializeFilePath))
                    {
                        Compiler.Parser.NodeDispatcher.RegisterStaticNodeListenerFactory(
                            () => {
                                var report = new Compiler.Report.CopyMoveInitializeReport(config.ReportCopyMoveInitializeFilePath);
                                reports.Add(report); return report;
                            });
                        
                    }
                    if (!string.IsNullOrEmpty(config.ReportZCallFilePath))
                    {
                        Compiler.Parser.NodeDispatcher.RegisterStaticNodeListenerFactory(
                            () => {
                                var report = new Compiler.Report.ZCallPgmReport(config.ReportZCallFilePath);
                                reports.Add(report); return report;
                            });
                        
                    }
                }
                #endregion

                //Parse input file
                parser.Parse(inputFilePath);


                diagnostics.AddRange(parser.Results.AllDiagnostics()); //Get all diags
                errorWriter.AddErrors(inputFilePath,
                    diagnostics.Take(config.MaximumDiagnostics == 0
                        ? diagnostics.Count
                        : config.MaximumDiagnostics)); //Write diags into error file
                

                if (!string.IsNullOrEmpty(config.HaltOnMissingCopyFilePath) && parser.MissingCopys.Count > 0)
                {
                    //Collect the missing copies
                    copyAreMissing = true;
                    File.WriteAllLines(config.HaltOnMissingCopyFilePath, parser.MissingCopys);
                }

                //Create extraced copies file even if copy are missing
                CreateExtractedCopiesFile(config, parser.Results.CopyTextNamesVariations);

                if (copyAreMissing)
                    throw new MissingCopyException("Some copy are missing", inputFilePath, null, logged: false, needMail: false);


                if (parser.Results.CodeElementsDocumentSnapshot == null &&config.ExecToStep > ExecutionStep.Preprocessor)
                {
                    throw new ParsingException(MessageCode.SyntaxErrorInParser, "File \"" + inputFilePath + "\" has syntactic error(s) preventing codegen (CodeElements).", inputFilePath); //Make ParsingException trace back to RunOnce()
                }
                else if (parser.Results.TemporaryProgramClassDocumentSnapshot == null &&config.ExecToStep > ExecutionStep.SyntaxCheck)
                {
                    throw new ParsingException(MessageCode.SyntaxErrorInParser ,"File \"" + inputFilePath + "\" has semantic error(s) preventing codegen (ProgramClass).", inputFilePath); //Make ParsingException trace back to RunOnce()
                }

                if (config.ExecToStep >= ExecutionStep.SemanticCheck)
                {
                    foreach (var program in parser.Results.TemporaryProgramClassDocumentSnapshot.Root.Programs)
                    {
                        var previousPrograms = baseTable.GetPrograms();
                        foreach (var previousProgram in previousPrograms)
                        {
                            previousProgram.SymbolTable.GetTableFromScope(SymbolTable.Scope.Namespace).AddProgram(program);
                        }

                        baseTable.AddProgram(program); //Add program to Namespace symbol table
                    }
                }

                if (config.ExecToStep >= ExecutionStep.Preprocessor && !string.IsNullOrEmpty(config.ExpandingCopyFilePath))
                {
                    try
                    {
                        StringBuilder generatedCobolStringBuilder = new StringBuilder();
                        var generator = GeneratorFactoryManager.Instance.Create(TypeCobol.Tools.Options_Config.OutputFormat.ExpandingCopy.ToString(),
                            parser.Results, generatedCobolStringBuilder, null, null, false);
                        var streamWriter =  new StreamWriter(config.ExpandingCopyFilePath);
                        generator.Generate(parser.Results, ColumnsLayout.CobolReferenceFormat);
                        streamWriter.Write(generatedCobolStringBuilder);
                        streamWriter.Flush();
                        streamWriter.Close();
                    }
                    catch(Exception e)
                    {
                        throw new GenerationException(e.Message, inputFilePath, e);
                    }
                }
            }


            //Then do the CrossCheck when all the programs are loaded in SymbolTable
            if (config.ExecToStep > ExecutionStep.SemanticCheck)
            {
                int fileIndex = 0;
                foreach (var parser in parsers)
                {
                    parser.Results.RefreshProgramClassDocumentSnapshot(); //Do Cross Check phase for each file
                    diagnostics.Clear();
                    diagnostics.AddRange(parser.Results.AllDiagnostics()); //Get all diags
                    errorWriter.Errors.Clear(); //Clear errorWriter because of the potential previous diags
                    errorWriter.AddErrors(config.InputFiles[fileIndex],
                        diagnostics.Take(config.MaximumDiagnostics == 0
                            ? diagnostics.Count
                            : config.MaximumDiagnostics)); //Write diags into error file

                    if (diagnostics.Count > 0)
                    {
                        foreach (var diag in diagnostics)
                        {
                            if (diag.CatchedException != null)
                            {
                                AnalyticsWrapper.Telemetry.TrackException(diag.CatchedException, config.InputFiles[fileIndex]);
                                AnalyticsWrapper.Telemetry.SendMail(diag.CatchedException, config.InputFiles,
                                    config.CopyFolders, config.CommandLine);
                            }
                        }

                    }

                    if (diagnostics.All(d => d.Info.Severity != Severity.Error))
                    {
                        if (config.ExecToStep >= ExecutionStep.CrossCheck && reports != null && reports.Count > 0)
                        {
                            foreach (var report in reports)
                            {
                                try
                                {
                                    report.Report();
                                    string msg = string.Format(
                                        "Succeed to emit report '{0}'",
                                        report.Filepath);
                                    Console.WriteLine(msg);
                                }
                                catch (Exception e)
                                {
                                    Console.Error.WriteLine(e.Message);
                                    throw new GenerationException(e.Message, report.Filepath, e);
                                }
                            }
                            
                        }
                    }

                    if (diagnostics.Any(d => d.Info.Severity == Compiler.Diagnostics.Severity.Error))
                    {
                        throw new PresenceOfDiagnostics("Diagnostics Detected", config.InputFiles[fileIndex]); //Make ParsingException trace back to RunOnce()
                    }

                    if (diagnostics.Any(d => d.Info.Severity == Compiler.Diagnostics.Severity.Warning)) {
                        returnCode = ReturnCode.Warning;
                    }

                    if (config.ExecToStep >= ExecutionStep.Generate)
                    {
                        try
                        {
                            //Load skeletons if necessary
                            List<Skeleton> skeletons = null;
                            if (!(string.IsNullOrEmpty(config.skeletonPath)))
                            {
                                skeletons = TypeCobol.Codegen.Config.Config.Parse(config.skeletonPath);
                            }

                            var sb = new StringBuilder();
                            bool bNeedLineMap = config.LineMapFiles.Count > fileIndex;
                            //Get Generator from specified config.OutputFormat
                            var generator = GeneratorFactoryManager.Instance.Create(config.OutputFormat.ToString(),
                                parser.Results,
                                sb, skeletons, AnalyticsWrapper.Telemetry.TypeCobolVersion, bNeedLineMap);

                            if (generator == null)
                            {
                                throw new GenerationException("Unknown OutputFormat=" + config.OutputFormat + "_",
                                    config.InputFiles[fileIndex]);
                            }

                            //Generate and check diagnostics
                            generator.Generate(parser.Results, ColumnsLayout.CobolReferenceFormat);
                            if (generator.Diagnostics != null)
                            {
                                errorWriter.AddErrors(config.InputFiles[fileIndex],
                                    generator.Diagnostics); //Write diags into error file
                                throw new PresenceOfDiagnostics("Diagnostics Detected", config.InputFiles[fileIndex]);
                                //Make ParsingException trace back to RunOnce()
                            }

                            var outputDirectory = new FileInfo(config.OutputFiles[fileIndex]).Directory;
                            var lockFilePath = outputDirectory.FullName + Path.DirectorySeparatorChar + "~.lock";
                            if (File.Exists(lockFilePath))
                            {
                                errorWriter.AddErrors(config.InputFiles[fileIndex],
                                    new Diagnostic(MessageCode.GenerationFailled, 0, 0, 0));

                            }
                            else
                            {
                                //Output Line Map Data.
                                if (bNeedLineMap && generator.HasLineMapData)
                                {
                                    using (var fstream = new FileStream(config.LineMapFiles[fileIndex], FileMode.Create))
                                    {
                                        try
                                        {
                                            generator.GenerateLineMapFile(fstream);
                                        }
                                        catch (Exception e)
                                        {
                                            //Fail to generate Line Map File
                                            System.Console.WriteLine(string.Format("Fail to generate Line Mapping File {0} : {1}", config.LineMapFiles[fileIndex],  e.Message));
                                        }
                                        finally
                                        {
                                            fstream.Close();
                                        }
                                    }
                                }
                                var lockWriter = new StreamWriter(lockFilePath);
                                lockWriter.Flush();
                                lockWriter.Close();

                                using (var streamWriter = new StreamWriter(config.OutputFiles[fileIndex]))
                                {
                                    try
                                    {
                                        streamWriter.Write(sb); //Write generated Cobol inside file
                                        streamWriter.Flush();
                                    }
                                    catch (Exception)
                                    {
                                        throw;
                                    }
                                    finally
                                    {
                                        File.Delete(lockFilePath); //Remove lock to allow watchers to read the file
                                        streamWriter.Close();
                                    }
                                }
                            }
                        }
                        catch (PresenceOfDiagnostics)
                        {
                            throw; //Throw the same exception to let runOnce() knows there is a problem
                        }
                        catch (GenerationException)
                        {
                            throw; //Throw the same exception to let runOnce() knows there is a problem
                        }
                        catch (Exception e)
                        {
                            //Otherwise create a new GenerationException
                            throw new GenerationException(e.Message, config.InputFiles[fileIndex], e);
                        }

                        fileIndex++;
                    }

                }
            }

            return returnCode;
        } 
        

        private static void CreateExtractedCopiesFile(TypeCobolConfiguration config, List<RemarksDirective.TextNameVariation> copiesUsed)
        {
            if (config.ExecToStep >= ExecutionStep.Preprocessor && !string.IsNullOrEmpty(config.ExtractedCopiesFilePath))
            {
                if (copiesUsed.Count > 0)
                {
#if EUROINFO_RULES
                    IEnumerable<string> copiesName;
                    if (config.UseEuroInformationLegacyReplacingSyntax)
                    {
                        copiesName = copiesUsed.Select(cp => cp.TextName).Distinct(); //Get copies without suffix
                    }
                    else
                    {
                        copiesName = copiesUsed.Select(cp => cp.TextNameWithSuffix).Distinct(); //Get copies with suffix
                    }
#else
                    var copiesName = copiesUsed.Select(cp => cp.TextNameWithSuffix).Distinct(); //Get copies with suffix
#endif
                    //Create an output document of all the copy encountered by the parser
                    File.WriteAllLines(config.ExtractedCopiesFilePath, copiesName);
                }
                else
                    File.Delete(config.ExtractedCopiesFilePath);
            }
        }
    }
}
