using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Text;

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
        internal static void runOnce(Config config) {
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
            runOnce2(config);

            stopWatch.Stop();
            debugLine = "                         parsed in " + stopWatch.Elapsed + " ms\n";
            File.AppendAllText("TypeCobol.CLI.log", debugLine);
            Console.WriteLine(debugLine);
        }

        private static void runOnce2(Config config)
        {
            TextWriter w;
			if (config.ErrorFile == null) w = Console.Error;
			else w = File.CreateText(config.ErrorFile);
			AbstractErrorWriter errorWriter;
			if (config.IsErrorXML) errorWriter = new XMLWriter(w);
			else errorWriter = new ConsoleWriter(w);
			errorWriter.Outputs = config.OutputFiles;

			var parser = new Parser();
			parser.CustomSymbols = LoadCopies(errorWriter, config.Copies, config.Format);
            parser.CustomSymbols = LoadDependencies(errorWriter, config.Dependencies, config.Format, parser.CustomSymbols);

			for(int c=0; c<config.InputFiles.Count; c++) {
				string path = config.InputFiles[c];
                try
                {

                    var typeCobolOptions = new TypeCobolOptions
                                            {
                                                HaltOnMissingCopy = config.HaltOnMissingCopyFilePath != null,
                                                ExecToStep = config.ProcessingStep,
                                            };
#if EUROINFO_RULES
                    typeCobolOptions.AutoRemarksEnable = config.AutoRemarks;
#endif
                    parser.Init(path, typeCobolOptions, config.Format, config.CopyFolders);
                }
                catch (Exception ex) {
                    Server.AddError(errorWriter, MessageCode.ParserInit, ex.Message, path);
                    continue;
                }
				parser.Parse(path);


			    if (!string.IsNullOrEmpty(config.HaltOnMissingCopyFilePath)) {
			        if (parser.MissingCopys.Count > 0) {
			            //Write in the specified file all the absent copys detected
			            File.WriteAllLines(config.HaltOnMissingCopyFilePath, parser.MissingCopys);
			        } else {
			            //Delete the file
			            File.Delete(config.HaltOnMissingCopyFilePath);
			        }
			    }

                if (parser.Results.CodeElementsDocumentSnapshot == null && config.ProcessingStep > ProcessingStep.Preprocessor)
                {
                    Server.AddError(errorWriter, MessageCode.SyntaxErrorInParser, "File \"" + path + "\" has syntactic error(s) preventing codegen (CodeElements).", path);
                    continue;
                }
                else if (parser.Results.ProgramClassDocumentSnapshot == null && config.ProcessingStep > ProcessingStep.SyntaxCheck)
                {
                    Server.AddError(errorWriter, MessageCode.SyntaxErrorInParser, "File \"" + path + "\" has semantic error(s) preventing codegen (ProgramClass).", path);
                    continue;
                }

                var allDiags = parser.Results.AllDiagnostics();
			    int errors = allDiags.Count;
				errorWriter.AddErrors(path, allDiags);

				if (config.ProcessingStep >= ProcessingStep.Generate && errors == 0)
                {
					var skeletons = TypeCobol.Codegen.Config.Config.Parse(config.skeletonPath);
					var codegen = new TypeCobol.Codegen.Generators.DefaultGenerator(parser.Results, new StreamWriter(config.OutputFiles[c]), skeletons);
					var program = parser.Results.ProgramClassDocumentSnapshot.Program;
					codegen.Generate(program.SyntaxTree.Root, program.SymbolTable, ColumnsLayout.CobolReferenceFormat);
				}
			}
			errorWriter.Write();
			errorWriter.FlushAndClose();
            errorWriter = null;
            //as w can be a Text file created, we need to close it
            w.Close();
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
			        parser.Init(path, new TypeCobolOptions { ExecToStep = ProcessingStep.SemanticCheck}, copyDocumentFormat);
			        parser.Parse(path);
                     
			        foreach (var diagnostic in parser.Results.AllDiagnostics()) {
                        Server.AddError(writer, MessageCode.IntrinsicLoading, 
                            diagnostic.ColumnStart, diagnostic.ColumnEnd, diagnostic.Line, 
                            "Error during parsing of " + path + ": " + diagnostic, path);
			        }
			        if (parser.Results.ProgramClassDocumentSnapshot.Program == null) {
                        Server.AddError(writer, MessageCode.IntrinsicLoading, "Error: Your Intrisic types/functions are not included into a program.", path);
			            continue;
			        }

			        var symbols = parser.Results.ProgramClassDocumentSnapshot.Program.SymbolTable;
			        foreach (var types in symbols.Types)
			            foreach (var type in types.Value)
			                table.AddType(type);
			        foreach (var functions in symbols.Functions)
			            foreach (var function in functions.Value)
			                table.AddFunction(function);
                    //TODO check if types or functions are already there
                }
                catch (Exception e) {
			        Server.AddError(writer, MessageCode.IntrinsicLoading, e.Message + "\n" + e.StackTrace, path);
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
            var parser = new Parser();
            var table = new SymbolTable(intrinsicTable, SymbolTable.Scope.Namespace); //Generate a table of NameSPace containing the dependencies programs based on the previously created intrinsic table. 

            var dependencies = new List<string>();
            foreach (var path in paths)
            {
                dependencies.AddRange(Tools.FileSystem.GetFiles(path, parser.Extensions, false)); //Get FIle by name or search the directory for all files
            }

            foreach (string path in dependencies)
            {
                try
                {
                    parser.Init(path, new TypeCobolOptions { ExecToStep = ProcessingStep.SemanticCheck }, format);
                    parser.Parse(path); //Parse the dependencie file

                    foreach (var diagnostic in parser.Results.AllDiagnostics())
                    {
                        Server.AddError(writer, MessageCode.DependenciesLoading,
                            diagnostic.ColumnStart, diagnostic.ColumnEnd, diagnostic.Line,
                            "Error during parsing of " + path + ": " + diagnostic, path);
                    }
                    if (parser.Results.ProgramClassDocumentSnapshot.Program == null)
                    {
                        Server.AddError(writer, MessageCode.DependenciesLoading, "Error: Your dependency file is not included into a program.", path);
                        continue;
                    }

                    table.AddProgram(parser.Results.ProgramClassDocumentSnapshot.Program); //Add program to Namespace symbol table
                }
                catch (Exception e)
                {
                    Server.AddError(writer, MessageCode.DependenciesLoading, e.Message + "\n" + e.StackTrace, path);
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
