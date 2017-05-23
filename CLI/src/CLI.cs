using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Threading;
using TypeCobol.CustomExceptions;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;
using Analytics;
using System.Linq;

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
        internal static ReturnCode runOnce(Config config) {
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
            try
            {
                runOnce2(config, errorWriter);
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
               

                if (typeCobolException is ParsingException)
                    return ReturnCode.ParsingError;
                if (typeCobolException is GenerationException)
                    return ReturnCode.GenerationError;

                return ReturnCode.FatalError; //Just in case..
            }
            catch (Exception e)//Catch any other exception
            {
                AnalyticsWrapper.Telemetry.TrackException(e);
                AnalyticsWrapper.Telemetry.SendMail(e, config.InputFiles, config.CopyFolders, config.CommandLine);

                Server.AddError(errorWriter, MessageCode.SyntaxErrorInParser, e.Message + e.StackTrace, string.Empty);
                return ReturnCode.FatalError;
            }
            finally
            {
                errorWriter.Write();
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
            }

            return ReturnCode.Success;
        }

        private static void runOnce2(Config config, AbstractErrorWriter errorWriter)
        {
            var parser = new Parser();

            parser.CustomSymbols = LoadCopies(errorWriter, config.Copies, config.Format); //Load of the intrinsics
            parser.CustomSymbols = LoadDependencies(errorWriter, config.Dependencies, config.Format, parser.CustomSymbols); //Load of the dependency files

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

                if (!string.IsNullOrEmpty(config.HaltOnMissingCopyFilePath))
                {
                    if (parser.MissingCopys.Count > 0)
                    {
                        //Write in the specified file all the absent copys detected
                        File.WriteAllLines(config.HaltOnMissingCopyFilePath, parser.MissingCopys);
                    }
                    else
                    {
                        //Delete the file
                        File.Delete(config.HaltOnMissingCopyFilePath);
                    }
                }

                var allDiags = parser.Results.AllDiagnostics();
                errorWriter.AddErrors(path, allDiags); //Write diags into error file

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
                    throw new ParsingException(MessageCode.SyntaxErrorInParser, "Diagnostics Detected", null, null, false, false); //Make ParsingException trace back to RunOnce()
                }

                if (parser.Results.CodeElementsDocumentSnapshot == null && config.ExecToStep > ExecutionStep.Preprocessor)
                {
                    throw new ParsingException(MessageCode.SyntaxErrorInParser, "File \"" + path + "\" has syntactic error(s) preventing codegen (CodeElements).", path); //Make ParsingException trace back to RunOnce()
                }
                else if (parser.Results.ProgramClassDocumentSnapshot == null && config.ExecToStep > ExecutionStep.SyntaxCheck)
                {
                    throw new ParsingException(MessageCode.SyntaxErrorInParser, "File \"" + path + "\" has semantic error(s) preventing codegen (ProgramClass).", path); //Make ParsingException trace back to RunOnce()
                }

                if (config.ExecToStep >= ExecutionStep.Generate)
                {
                    var skeletons = TypeCobol.Codegen.Config.Config.Parse(config.skeletonPath);
                    var codegen = new TypeCobol.Codegen.Generators.DefaultGenerator(parser.Results, new StreamWriter(config.OutputFiles[c]), skeletons);
                    try
                    {
                        codegen.Generate(parser.Results, ColumnsLayout.CobolReferenceFormat); //TODO : Add exception management for code generation
                        if (codegen.Diagnostics != null)
                        {
                            errorWriter.AddErrors(path, codegen.Diagnostics); //Write diags into error file
                        }
                    }
                    catch (Exception e)
                    {
                        if (e is GenerationException)
                            throw e; //Throw the same exception to let runOnce() knows there is a problem
                        
                        throw new GenerationException(e.Message, path, e); //Otherwise create a new GenerationException
                    }

                }
            }
        }

        /// <summary>
        /// LoadCopies method.
        /// </summary>
        /// <param name="writer">AbstractErrorWriter</param>
        /// <param name="paths">List<string></param>
        /// <param name="copyDocumentFormat">DocumentFormat</param>
        /// <returns>SymbolTable</returns>
        private static SymbolTable LoadCopies(AbstractErrorWriter writer, List<string> paths, DocumentFormat copyDocumentFormat)
        {
            var parser = new Parser();

			var table = new SymbolTable(null, SymbolTable.Scope.Intrinsic);

			var copies = new List<string>();
			foreach(string path in paths) copies.AddRange(Tools.FileSystem.GetFiles(path, parser.Extensions, false));

			foreach(string path in copies) {
			    try {
                    parser.Init(path, new TypeCobolOptions { ExecToStep = ExecutionStep.SemanticCheck}, copyDocumentFormat);
			        parser.Parse(path);

                    var diagnostics = parser.Results.AllDiagnostics();


                    foreach (var diagnostic in diagnostics) {
                        Server.AddError(writer, MessageCode.IntrinsicLoading, 
                            diagnostic.ColumnStart, diagnostic.ColumnEnd, diagnostic.Line, 
                            "Error during parsing of " + path + ": " + diagnostic, path);
                    }
                    if (diagnostics.Count > 0)
                        throw new CopyLoadingException("Diagnostics detected while parsing Intrinsic file", path, null, true, false);


                    if (parser.Results.ProgramClassDocumentSnapshot.Root.Programs == null || parser.Results.ProgramClassDocumentSnapshot.Root.Programs.Count() == 0)
                    {
                        throw new CopyLoadingException("Error: Your Intrisic types/functions are not included into a program.", path, null, true, false);
                    }

                    foreach (var program in parser.Results.ProgramClassDocumentSnapshot.Root.Programs)
                    {
                        var symbols = program.SymbolTable.GetTableFromScope(SymbolTable.Scope.Declarations);

                        if (symbols.Types.Count == 0 && symbols.Functions.Count == 0)
                        {
                            Server.AddError(writer, MessageCode.Warning, "No types and no procedures/functions found", path);
                            continue;
                        }

                        //TODO check if types or functions are already there
                        table.CopyAllTypes(symbols.Types);
                        table.CopyAllFunctions(symbols.Functions);
                    }
                }
                catch (CopyLoadingException copyException)
                {
                    throw copyException; //Make CopyLoadingException trace back to runOnce()
                }
                catch (Exception e)
                {
                    throw new CopyLoadingException(e.Message + "\n" + e.StackTrace, path, e);
                }
               
            }
            return table;
        }

        /// <summary>
        /// LoadCopies method.
        /// </summary>
        /// <param name="writer">AbstractErrorWriter</param>
        /// <param name="paths">List<string></param>
        /// <param name="copyDocumentFormat">DocumentFormat</param>
        /// <returns>SymbolTable</returns>
        private static SymbolTable LoadDependencies(AbstractErrorWriter writer, List<string> paths, DocumentFormat format, SymbolTable intrinsicTable)
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
                try
                {
                    parser.Init(path, new TypeCobolOptions { ExecToStep = ExecutionStep.SemanticCheck }, format);
                    parser.Parse(path); //Parse the dependencie file

                    var diagnostics = parser.Results.AllDiagnostics();
                    foreach (var diagnostic in diagnostics)
                    {
                        Server.AddError(writer, MessageCode.DependenciesLoading,
                            diagnostic.ColumnStart, diagnostic.ColumnEnd, diagnostic.Line,
                            "Error while parsing " + path + ": " + diagnostic, path);
                    }
                    if (diagnostics.Count > 0)
                        throw new DepedenciesLoadingException("Diagnostics detected while parsing dependency file", path, null, true, false);

                    if (parser.Results.ProgramClassDocumentSnapshot.Root.Programs == null || parser.Results.ProgramClassDocumentSnapshot.Root.Programs.Count() == 0)
                    {
                        throw new DepedenciesLoadingException("Error: Your dependency file is not included into a program", path, null, true, false);
                    }

                    foreach (var program in parser.Results.ProgramClassDocumentSnapshot.Root.Programs)
                    {
                        table.AddProgram(program); //Add program to Namespace symbol table
                    }
                   
                }
                catch (DepedenciesLoadingException)
                {
                    throw; //Make DepedenciesLoadingException trace back to runOnce()
                }
                catch (Exception e)
                {
                    throw new DepedenciesLoadingException(e.Message + "\n" + e.StackTrace, path, e);
                }
            }
            return table;
        }

        /// <summary>
        /// CreateFormat method to get the format name.
        /// </summary>
        /// <param name="encoding">string</param>
        /// <param name="config">Config</param>
        /// <returns>DocumentFormat</returns>
        internal static Compiler.DocumentFormat CreateFormat(string encoding, ref Config config)
        {
            config.EncFormat = encoding;

            if (encoding == null) return null;
            if (encoding.ToLower().Equals("zos")) return TypeCobol.Compiler.DocumentFormat.ZOsReferenceFormat;
            if (encoding.ToLower().Equals("utf8")) return TypeCobol.Compiler.DocumentFormat.FreeUTF8Format;
            /*if (encoding.ToLower().Equals("rdz"))*/
            return TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
        }
    }
}
