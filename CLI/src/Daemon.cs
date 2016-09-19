using System;
using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using Mono.Options;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Server
{
	class Server {

		class Config {
			public TypeCobol.Compiler.DocumentFormat Format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
			public bool Codegen = false;
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
				{ "i|input=", "{PATH} to an input file to parse. This option can be specified more than once.", (string v) => config.InputFiles.Add(v) },
				{ "o|output=","{PATH} to an ouput file where to generate code. This option can be specified more than once.", (string v) => config.OutputFiles.Add(v) },
				{ "g|generate",  "If present, this option generates code corresponding to each input file parsed.", v => config.Codegen = (v!=null) },
				{ "d|diagnostics=", "{PATH} to the error diagnostics file.", (string v) => config.ErrorFile = v },
                { "s|skeletons=", "{PATH} to the skeletons files (used to generate code).", (string v) => config.skeletonPath = v },
//				{ "p|pipename=",  "{NAME} of the communication pipe to use. Default: "+pipename+".", (string v) => pipename = v },
				{ "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\"(this is the default), \"zos\", or \"utf8\". "
								+"If this option is not present, the parser will attempt to guess the {ENCODING} automatically.",
								(string v) => config.Format = CreateFormat(v)
				},
				{ "y|copy=", "{PATH} to a copy to load. This option can be specified more than once.", (string v) => config.Copies.Add(v) },
				{ "h|help",  "Output a usage message and exit.", v => help = (v!=null) },
				{ "V|version",  "Output the version number of "+PROGNAME+" and exit.", v => version = (v!=null) },
			};
			System.Collections.Generic.List<string> args;
			try { args = p.Parse(argv); }
			catch (OptionException ex) { return exit(1, ex.Message); }

			if (help) {
				p.WriteOptionDescriptions(System.Console.Out);
				return 0;
			}
			if (version) {
				System.Console.WriteLine(PROGVERSION);
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

			return 0;
		}

		private static Compiler.DocumentFormat CreateFormat(string encoding) {
			if (encoding == null) return null;
			if (encoding.ToLower().Equals("zos")) return TypeCobol.Compiler.DocumentFormat.ZOsReferenceFormat;
			if (encoding.ToLower().Equals("utf8")) return TypeCobol.Compiler.DocumentFormat.FreeUTF8Format;
			/*if (encoding.ToLower().Equals("rdz"))*/ return TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
		}

		private static void runOnce(Config config) {
			TextWriter w;
			if (config.ErrorFile == null) w = System.Console.Error;
			else w = File.CreateText(config.ErrorFile);
			AbstractErrorWriter writer;
			if (config.IsErrorXML) writer = new XMLWriter(w);
			else writer = new ConsoleWriter(w);
			writer.Outputs = config.OutputFiles;

			var parser = new Parser();
			parser.CustomSymbols = loadCopies(config.Copies);

			for(int c=0; c<config.InputFiles.Count; c++) {
				string path = config.InputFiles[c];
				try { parser.Init(path, config.Format); }
				catch(System.IO.IOException ex) {
					AddError(writer, ex.Message, path);
					continue;
				}
				parser.Parse(path);
				if (parser.Results.CodeElementsDocumentSnapshot == null) {
					AddError(writer, "File \""+path+"\" has syntactic error(s) preventing codegen (CodeElements).", path);
					continue;
				}

				writer.AddErrors(path, parser.Converter.AsDiagnostics(parser.Results.CodeElementsDocumentSnapshot.ParserDiagnostics));
				// no need to add errors from parser.CodeElementsSnapshot.CodeElements
				// as they are on parser.CodeElementsSnapshot.CodeElements which are added below

				if (parser.Results.ProgramClassDocumentSnapshot == null) {
					AddError(writer, "File \""+path+"\" has semantic error(s) preventing codegen (ProgramClass).", path);
					continue;
				}
				writer.AddErrors(path, parser.Converter.AsDiagnostics(parser.Results.ProgramClassDocumentSnapshot.Diagnostics));
				foreach(var e in parser.Results.CodeElementsDocumentSnapshot.CodeElements) {
					if (e.Diagnostics.Count < 1) continue;
					writer.AddErrors(path, parser.Converter.GetDiagnostics(e));
				}

				if (config.Codegen) {
					var skeletons = TypeCobol.Codegen.Config.Config.Parse(config.skeletonPath);
					var codegen = new TypeCobol.Codegen.Generator(new StreamWriter(config.OutputFiles[c]), parser.Results.TokensLines, skeletons);
					var program = parser.Results.ProgramClassDocumentSnapshot.Program;
					codegen.Generate(program.SyntaxTree.Root, program.SymbolTable, ColumnsLayout.CobolReferenceFormat);
				}
			}
			writer.Write();
			writer.Flush();
		}

        private static void AddError(AbstractErrorWriter writer, string message, string path) {
			var error = new TypeCobol.Tools.Diagnostic();
			error.Message = message;
			error.Code = "codegen";
			try { error.Source = writer.Inputs[path]; }
			catch(KeyNotFoundException ex) { error.Source = writer.Count.ToString(); }
			var list = new List<TypeCobol.Tools.Diagnostic>();
			list.Add(error);
			writer.AddErrors(path, list);
			System.Console.WriteLine(error.Message);
		}

		private static Compiler.CodeModel.SymbolTable loadCopies(List<string> copies) {
			var parser = new Parser();
			var table = new SymbolTable(null, SymbolTable.Scope.Intrinsic);
			foreach(string path in copies) {
				parser.Init(path);
				parser.Parse(path);

                if (parser.Results.ProgramClassDocumentSnapshot == null) continue;
                if (parser.Results.ProgramClassDocumentSnapshot.Program == null)
                {
                    Console.WriteLine("Error: Your Intrisic types are not included into a program.");
                    continue;
                }

                foreach (var type in parser.Results.ProgramClassDocumentSnapshot.Program.SymbolTable.CustomTypes)
			    {
			        table.RegisterCustomType(type); //TODO check if already there

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
				catch (System.IO.IOException ex) { System.Console.WriteLine("Error: {0}", ex.Message); }
				catch (System.Runtime.Serialization.SerializationException ex ) { System.Console.WriteLine("Error: {0}", ex.Message); }
				finally { pipe.Disconnect(); pipe.Close(); }
			}
		}



		private static string PROGNAME = System.AppDomain.CurrentDomain.FriendlyName;
		private static string PROGVERSION = GetVersion();

		private static string GetVersion() {
			System.Reflection.Assembly assembly = System.Reflection.Assembly.GetExecutingAssembly();
			var info = System.Diagnostics.FileVersionInfo.GetVersionInfo(assembly.Location);
			return info.FileVersion;
		}

		static int exit(int code, string message) {
			string errmsg = PROGNAME+": "+message+"\n";
			errmsg += "Try "+PROGNAME+" --help for usage information.";
			System.Console.WriteLine(errmsg);
			return code;
		}

	}
}
