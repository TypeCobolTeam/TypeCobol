using System;
using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using Mono.Options;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Server {

	class Server {

		class Config {
			public TypeCobol.Compiler.DocumentFormat Format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
			public bool Codegen = false;
            public bool AutoRemarks;
            public string HaltOnMissingCopyFilePath;
            public List<string> CopyFolders = new List<string>();
			public List<string> InputFiles  = new List<string>();
			public List<string> OutputFiles = new List<string>();
			public string ErrorFile = null;
			public string skeletonPath = "";
			public bool IsErrorXML {
				get { return ErrorFile != null && ErrorFile.ToLower().EndsWith(".xml"); }
			}
			public List<string> Copies = new List<string>();
           
        }

		static int Main(string[] argv) {
			bool help = false;
			bool version = false;
			bool once = false;
			var config = new Config();
			var pipename = "TypeCobol.Server";

            var p = new OptionSet () {
				"USAGE",
				"  "+PROGNAME+" [OPTIONS]... [PIPENAME]",
				"",
				"VERSION:",
				"  "+PROGVERSION,
				"",
				"DESCRIPTION:",
				"  Run the TypeCobol parser server.",
				{ "1|once",  "Parse one set of files and exit. If present, this option does NOT launch the server.", v => once = (v!=null) },
				{ "i|input=", "{PATH} to an input file to parse. This option can be specified more than once.", v => config.InputFiles.Add(v) },
				{ "o|output=","{PATH} to an ouput file where to generate code. This option can be specified more than once.", v => config.OutputFiles.Add(v) },
				{ "g|generate",  "If present, this option generates code corresponding to each input file parsed.", v => config.Codegen = (v!=null) },
				{ "d|diagnostics=", "{PATH} to the error diagnostics file.", v => config.ErrorFile = v },
				{ "s|skeletons=", "{PATH} to the skeletons files.", v => config.skeletonPath = v },
                { "a|autoremarks=", "Enable automatic remarks creation while parsing and generating Cobol", v => config.AutoRemarks = (v!=null) },
                { "hc|HaltOnMissingCopyFilePath=", "HaltOnMissingCopyFilePath will generate a file to list all the absent copies", v => config.HaltOnMissingCopyFilePath = v },
//				{ "p|pipename=",  "{NAME} of the communication pipe to use. Default: "+pipename+".", (string v) => pipename = v },
				{ "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\"(this is the default), \"zos\", or \"utf8\". "
								+"If this option is not present, the parser will attempt to guess the {ENCODING} automatically.",
								v => config.Format = CreateFormat(v)
				},
				{ "y|intrinsic=", "{PATH} to intrinsic definitions to load.\nThis option can be specified more than once.", v => config.Copies.Add(v) },
				{ "c|copies=",  "Folder where COBOL copies can be found.\nThis option can be specified more than once.", v => config.CopyFolders.Add(v) },
				{ "h|help",  "Output a usage message and exit.", v => help = (v!=null) },
                { "V|version",  "Output the version number of "+PROGNAME+" and exit.", v => version = (v!=null) },
			};

		    try {
                List<string> args;
		        try {

                    args = p.Parse(argv);
                } catch (OptionException ex) {
                    return exit(1, ex.Message);
		        }

		        if (help) {
		            p.WriteOptionDescriptions(Console.Out);
		            return 0;
		        }
		        if (version) {
		            Console.WriteLine(PROGVERSION);
		            return 0;
		        }
		        if (config.OutputFiles.Count > 0 && config.InputFiles.Count != config.OutputFiles.Count)
		            return exit(2, "The number of output files must be equal to the number of input files.");
		        if (config.OutputFiles.Count == 0 && config.Codegen)
		            foreach(var path in config.InputFiles) config.OutputFiles.Add(path+".cee");

		        if (args.Count > 0) pipename = args[0];

		        if (once) {
		            runOnce(config);
		        } else {
		            runServer(pipename);
		        }
		    } catch (Exception e) {
                return exit(1, e.Message);
            }
		    return 0;
		}

		private static Compiler.DocumentFormat CreateFormat(string encoding) {
			if (encoding == null) return null;
			if (encoding.ToLower().Equals("zos")) return TypeCobol.Compiler.DocumentFormat.ZOsReferenceFormat;
			if (encoding.ToLower().Equals("utf8")) return TypeCobol.Compiler.DocumentFormat.FreeUTF8Format;
			/*if (encoding.ToLower().Equals("rdz"))*/ return TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
		}

		private static void runOnce(Config config)
		{
		   
			TextWriter w;
			if (config.ErrorFile == null) w = Console.Error;
			else w = File.CreateText(config.ErrorFile);
			AbstractErrorWriter writer;
			if (config.IsErrorXML) writer = new XMLWriter(w);
			else writer = new ConsoleWriter(w);
			writer.Outputs = config.OutputFiles;

			var parser = new Parser();
			parser.CustomSymbols = LoadCopies(writer, config.Copies, config.Format);

			for(int c=0; c<config.InputFiles.Count; c++) {
				string path = config.InputFiles[c];
				try { parser.Init(path, config.Format, config.CopyFolders, config.AutoRemarks, config.HaltOnMissingCopyFilePath); }
				catch(Exception ex) {
					AddError(writer, MessageCode.ParserInit, ex.Message, path);
					continue;
				}
				parser.Parse(path);

				if (parser.Results.CodeElementsDocumentSnapshot == null) {
					AddError(writer, MessageCode.SyntaxErrorInParser, "File \""+path+"\" has syntactic error(s) preventing codegen (CodeElements).", path);
					continue;
				} else if (parser.Results.ProgramClassDocumentSnapshot == null) {
					AddError(writer, MessageCode.SyntaxErrorInParser, "File \"" +path+"\" has semantic error(s) preventing codegen (ProgramClass).", path);
					continue;
				}
			    var allDiags = parser.Results.AllDiagnostics();
			    int errors = allDiags.Count;
				writer.AddErrors(path, allDiags);

				if (config.Codegen && errors == 0) {
					var skeletons = TypeCobol.Codegen.Config.Config.Parse(config.skeletonPath);
					var codegen = new TypeCobol.Codegen.Generators.DefaultGenerator(parser.Results, new StreamWriter(config.OutputFiles[c]), skeletons);
					var program = parser.Results.ProgramClassDocumentSnapshot.Program;
					codegen.Generate(program.SyntaxTree.Root, program.SymbolTable, ColumnsLayout.CobolReferenceFormat);
				}
			}
			writer.Write();
			writer.Flush();
		}

        /// <summary>
        /// Add an error message
        /// </summary>
        /// <param name="writer">Error Writer</param>
        /// <param name="messageCode">Message's code</param>
        /// <param name="message">The text message</param>
        /// <param name="path">The source file path</param>
		private static void AddError(AbstractErrorWriter writer, MessageCode messageCode, string message, string path)
		{
            AddError(writer, messageCode, 0, 0, 1, message, path);
		}

        /// <summary>
        /// Add an error message
        /// </summary>
        /// <param name="writer">Error Writer</param>
        /// <param name="messageCode">Message's code</param>
        /// <param name="columnStart">Start column in the source file</param>
        /// <param name="columnEnd">End column in the source file</param>
        /// <param name="lineNumber">Lien number in the source file</param>
        /// <param name="message">The text message</param>
        /// <param name="path">The source file path</param>
        private static void AddError(AbstractErrorWriter writer, MessageCode messageCode, int columnStart, int columnEnd, int lineNumber, string message, string path)
        {
            Diagnostic diag = new Diagnostic(messageCode, columnStart, columnEnd, lineNumber,
                message != null
                ? (path != null ? new object[2] { message, path } : new object[1] { message })
                : (path != null ? new object[1] { path } : new object[0]));
            diag.Message = message;
            writer.AddErrors(path, diag);
            Console.WriteLine(diag.Message);
        }

		private static SymbolTable LoadCopies(AbstractErrorWriter writer, List<string> paths, DocumentFormat copyDocumentFormat) {
			var parser = new Parser();
			var table = new SymbolTable(null, SymbolTable.Scope.Intrinsic);

			var copies = new List<string>();
			foreach(string path in paths) copies.AddRange(Tools.FileSystem.GetFiles(path, parser.Extensions, false));

			foreach(string path in copies) {
			    try {
			        parser.Init(path, copyDocumentFormat);
			        parser.Parse(path);
                     
			        foreach (var diagnostic in parser.Results.AllDiagnostics()) {
                        AddError(writer, MessageCode.IntrinsicLoading, 
                            diagnostic.ColumnStart, diagnostic.ColumnEnd, diagnostic.Line, 
                            "Error during parsing of " + path + ": " + diagnostic, path);
			        }
			        if (parser.Results.ProgramClassDocumentSnapshot.Program == null) {
			            AddError(writer, MessageCode.IntrinsicLoading, "Error: Your Intrisic types/functions are not included into a program.", path);
			            continue;
			        }

			        var symbols = parser.Results.ProgramClassDocumentSnapshot.Program.SymbolTable;
			        foreach (var types in symbols.Types)
			            foreach (var type in types.Value)
			                table.AddType((Compiler.Nodes.TypeDefinition) type);
			        foreach (var functions in symbols.Functions)
			            foreach (var function in functions.Value)
			                table.AddFunction((Compiler.Nodes.FunctionDeclaration) function);
			        //TODO check if types or functions are already there
			    } catch (Exception e) {
			        AddError(writer, MessageCode.IntrinsicLoading, e.Message + "\n" + e.StackTrace, path);
			    }
			}
			return table;
		}

		private static void runServer(string pipename) {
			var parser = new Parser();
			var pipe = new NamedPipeServerStream(pipename, PipeDirection.InOut, 4);
			Commands.Register(66, new Parse(parser, pipe, pipe));
			Commands.Register(67, new Initialize(parser, pipe, pipe));
			var decoder = new TypeCobol.Server.Serialization.IntegerSerializer();
			System.Console.WriteLine("NamedPipeServerStream thread created. Wait for a client to connect on " + pipename);
			while (true) {
				pipe.WaitForConnection(); // blocking
				//System.Console.WriteLine("Client connected.");
				try {
					int code = decoder.Deserialize(pipe);
					var command = Commands.Get(code);
					command.execute();
				}
				catch (IOException ex) { Console.WriteLine("Error: {0}", ex.Message); }
				catch (System.Runtime.Serialization.SerializationException ex ) { Console.WriteLine("Error: {0}", ex.Message); }
				finally { pipe.Disconnect(); pipe.Close(); }
			}
		}



		private static readonly string PROGNAME = System.AppDomain.CurrentDomain.FriendlyName;
		private static readonly string PROGVERSION = GetVersion();

		private static string GetVersion() {
			System.Reflection.Assembly assembly = System.Reflection.Assembly.GetExecutingAssembly();
			var info = System.Diagnostics.FileVersionInfo.GetVersionInfo(assembly.Location);
			return info.FileVersion;
		}

		static int exit(int code, string message) {
			string errmsg = PROGNAME+": "+message+"\n";
			errmsg += "Try "+PROGNAME+" --help for usage information.";
			Console.WriteLine(errmsg);
			return code;
		}

	}
}
