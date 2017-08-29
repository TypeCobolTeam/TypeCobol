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
using TypeCobol.Codegen;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Tools.Options_Config;

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
                AnalyticsWrapper.Telemetry.TrackException(typeCobolException);

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
                AnalyticsWrapper.Telemetry.TrackException(e);
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

            AnalyticsWrapper.Telemetry.TrackEvent("[Duration] Execution Time",
                                                    new Dictionary<string, string> { { "Duration", "Duration"} }, //Custom properties for metrics
                                                    new Dictionary<string, double> { { "ExecutionTime", stopWatch.Elapsed.Milliseconds} }); //Metrics fo duration
            
            return returnCode;
        }

        private static ReturnCode runOnce2(TypeCobolConfiguration config, AbstractErrorWriter errorWriter)
        {
            var parser = new Parser();
            bool diagDetected = false;
            EventHandler<Tools.APIHelpers.DiagnosticsErrorEvent> DiagnosticsErrorEvent = null;
            DiagnosticsErrorEvent += delegate(object sender, Tools.APIHelpers.DiagnosticsErrorEvent diagEvent)
            {
                //Delegate Event to handle diagnostics generated while loading dependencies/intrinsics
                diagDetected = true;
                var diagnostic = diagEvent.Diagnostic;
                Server.AddError(errorWriter, MessageCode.IntrinsicLoading,
                    diagnostic.ColumnStart, diagnostic.ColumnEnd, diagnostic.Line,
                    "Error while parsing " + diagEvent.Path + ": " + diagnostic, diagEvent.Path);
            };

            parser.CustomSymbols = Tools.APIHelpers.Helpers.LoadIntrinsic(config.Copies, config.Format, DiagnosticsErrorEvent); //Load of the intrinsics
            parser.CustomSymbols = Tools.APIHelpers.Helpers.LoadDependencies(config.Dependencies, config.Format, parser.CustomSymbols, config.InputFiles, DiagnosticsErrorEvent); //Load of the dependency files
           
            if (diagDetected)
                throw new CopyLoadingException("Diagnostics detected while parsing Intrinsic file", null, null, logged: false, needMail: false);

            ReturnCode returnCode = ReturnCode.Success;
            for (int c = 0; c < config.InputFiles.Count; c++)
            {
                string path = config.InputFiles[c];
                try
                {

                    var typeCobolOptions = new TypeCobolOptions
                                            {
                                                HaltOnMissingCopy = config.HaltOnMissingCopyFilePath != null,
                                                ExecToStep = config.ExecToStep,
                                            };

#if EUROINFO_RULES
                    typeCobolOptions.AutoRemarksEnable = config.AutoRemarks;
#endif

                    parser.Init(path, typeCobolOptions, config.Format, config.CopyFolders); //Init parser create CompilationProject & Compiler before parsing the given file
                }
                catch (Exception ex)
                {
                    throw new ParsingException(MessageCode.ParserInit, ex.Message, path, ex); //Make ParsingException trace back to RunOnce()
                }

                parser.Parse(path);

                
                bool copyAreMissing = false;
                if (!string.IsNullOrEmpty(config.HaltOnMissingCopyFilePath))
                {
                    if (parser.MissingCopys.Count > 0)
                    {
                        //Write in the specified file all the absent copys detected
                        File.WriteAllLines(config.HaltOnMissingCopyFilePath, parser.MissingCopys);
                        copyAreMissing = true;
                    }
                    else
                    {
                        //Delete the file
                        File.Delete(config.HaltOnMissingCopyFilePath);
                    }
                }

                var allDiags = parser.Results.AllDiagnostics();
                errorWriter.AddErrors(path, allDiags.Take(config.MaximumDiagnostics == 0 ? allDiags.Count : config.MaximumDiagnostics)); //Write diags into error file

                if (allDiags.Count > 0)
                {
                    foreach (var diag in allDiags)
                    {
                        if(diag.CatchedException != null)
                        {
                            AnalyticsWrapper.Telemetry.TrackException(diag.CatchedException);
                            AnalyticsWrapper.Telemetry.SendMail(diag.CatchedException, config.InputFiles, config.CopyFolders, config.CommandLine);
                        }
                    }

                    AnalyticsWrapper.Telemetry.TrackEvent("[Diagnostics] Detected");
                    //Exception is thrown just below
                    }

                //Copy missing is more important than diagnostics
                if (copyAreMissing) {
                    throw new MissingCopyException("Some copy are missing", path, null, logged: false, needMail: false);
                } else if (parser.Results.AllDiagnostics().Any(d => d.Info.Severity == Compiler.Diagnostics.Severity.Error)) {
                    throw new PresenceOfDiagnostics("Diagnostics Detected"); //Make ParsingException trace back to RunOnce()
                }

                if (parser.Results.CodeElementsDocumentSnapshot == null && config.ExecToStep > ExecutionStep.Preprocessor)
                {
                    throw new ParsingException(MessageCode.SyntaxErrorInParser, "File \"" + path + "\" has syntactic error(s) preventing codegen (CodeElements).", path); //Make ParsingException trace back to RunOnce()
                }
                else if (parser.Results.ProgramClassDocumentSnapshot == null && config.ExecToStep > ExecutionStep.SyntaxCheck)
                {
                    throw new ParsingException(MessageCode.SyntaxErrorInParser, "File \"" + path + "\" has semantic error(s) preventing codegen (ProgramClass).", path); //Make ParsingException trace back to RunOnce()
                }

                if (config.ExecToStep >= ExecutionStep.Generate) {

                    try
                    {
                        //Load skeletons if necessary
                        List<Skeleton> skeletons = null;
                        if (!(string.IsNullOrEmpty(config.skeletonPath))) {
                            skeletons = TypeCobol.Codegen.Config.Config.Parse(config.skeletonPath);
                        }


                        //Get Generator from specified config.OutputFormat
                        var generator = GeneratorFactoryManager.Instance.Create(config.OutputFormat.ToString(), parser.Results,
                            new StreamWriter(config.OutputFiles[c]), skeletons);

                        if (generator == null) {
                            throw new GenerationException("Unkown OutputFormat=" + config.OutputFormat + "_", path);
                        }

                        //Generate and check diagnostics
                        generator.Generate(parser.Results, ColumnsLayout.CobolReferenceFormat);
                        if (generator.Diagnostics != null) {
                            errorWriter.AddErrors(path, generator.Diagnostics); //Write diags into error file
                            throw new PresenceOfDiagnostics("Diagnostics Detected");
                            //Make ParsingException trace back to RunOnce()
                        }
                    } catch (PresenceOfDiagnostics) { 
                        throw; //Throw the same exception to let runOnce() knows there is a problem
                    } catch (GenerationException) {
                        throw; //Throw the same exception to let runOnce() knows there is a problem
                    } catch(Exception e) { //Otherwise create a new GenerationException
                        throw new GenerationException(e.Message, path, e);
                    }
                }

                if (parser.Results.AllDiagnostics().Any(d => d.Info.Severity == Compiler.Diagnostics.Severity.Warning)) {
                    returnCode = ReturnCode.Warning;
                }
            }

            return returnCode;
        }   
    }
}
