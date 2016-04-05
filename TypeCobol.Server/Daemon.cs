using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using Mono.Options;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.CodeModel;

namespace TypeCobol.Server
{
	class Server {

		class Config {
			public TypeCobol.Compiler.DocumentFormat Format = null;
			public bool Codegen = false;
			public List<string> InputFiles  = new List<string>();
			public List<string> OutputFiles = new List<string>();
			public string ErrorFile = null;
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
//				{ "p|pipename=",  "{NAME} of the communication pipe to use. Default: "+pipename+".", (string v) => pipename = v },
				{ "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\", \"zos\", or \"utf8\". "
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
			if (encoding.ToLower().Equals("rdz")) return TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
			if (encoding.ToLower().Equals("utf8")) return TypeCobol.Compiler.DocumentFormat.FreeUTF8Format;
			return null;
		}

		private static void runOnce(Config config) {
			TextWriter w;
			if (config.ErrorFile == null) w = System.Console.Error;
			else w = File.CreateText(config.ErrorFile);
			AbstractErrorWriter writer;
			if (config.IsErrorXML) writer = new XMLWriter(w);
			else writer = new ConsoleWriter(w);
			writer.Outputs = config.OutputFiles;

			var parser = new Parser("TypeCobol.Server");
			var table = loadCopies(config.Copies);
w.WriteLine(table.CustomTypes.Count+" types loaded.");
foreach(var type in table.CustomTypes.Values) w.WriteLine(type.Name+"{"+type.Subordinates.Count+"}");
w.WriteLine("TODO: make them usable.");
			for(int c=0; c<config.InputFiles.Count; c++) {
				string path = config.InputFiles[c];
				parser.Init(path, config.Format);
				parser.Parse(path);

				var converter = new TypeCobol.Tools.CodeElementDiagnostics(parser.CodeElementsSnapshot.Lines);
				writer.AddErrors(path, converter.AsDiagnostics(parser.CodeElementsSnapshot.ParserDiagnostics));
				writer.AddErrors(path, converter.AsDiagnostics(parser.Snapshot.Diagnostics));
				foreach(var e in parser.CodeElementsSnapshot.CodeElements) {
					if (e.Diagnostics.Count < 1) continue;
					writer.AddErrors(path, converter.GetDiagnostics(e));
				}

				if (config.Codegen) {
					var codegen = new TypeCobol.Compiler.Generator.TypeCobolGenerator(parser.Source, config.Format, parser.Snapshot);
					var stream = new StreamWriter(config.OutputFiles[c]);
					codegen.WriteCobol(stream);
				}
			}
			writer.Write();
			writer.Flush();
		}

		private static Compiler.CodeModel.SymbolTable loadCopies(List<string> copies) {
			var parser = new Parser("TypeCobol.Server.loading");
			var table = new SymbolTable(null, SymbolTable.Scope.External);
			foreach(string path in copies) {
				parser.Init(path);
				parser.Parse(path);
				foreach(var type in parser.Snapshot.Program.SymbolTable.CustomTypes.Values)
					table.RegisterCustomType(type);//TODO check if already there
			}
			return table;
		}

		private static void runServer(string pipename) {
			var parser = new Parser("TypeCobol.Server");
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
