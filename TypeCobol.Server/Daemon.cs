using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using Mono.Options;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Server
{
	class Server {

		class Config {
			public TypeCobol.Compiler.DocumentFormat Format = null;
			public new List<string> InputFiles  = new List<string>();
			public new List<string> OutputFiles = new List<string>();
			public string ErrorFile = null;
			public bool IsErrorXML {
				get { return ErrorFile != null && ErrorFile.ToLower().EndsWith(".xml"); }
			}
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
				{ "d|diagnostics=", "{PATH} to the error diagnostics file.", (string v) => config.ErrorFile = v },
//				{ "p|pipename=",  "{NAME} of the communication pipe to use. Default: "+pipename+".", (string v) => pipename = v },
				{ "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\", \"zos\", or \"utf8\". "
								+"If this option is not present, the parser will attempt to guess the {ENCODING} automatically.",
								(string v) => config.Format = CreateFormat(v)
				},
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
			if (config.OutputFiles.Count == 0)
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
			var parser = new Parser("TypeCobol.Server");
			for(int c=0; c<config.InputFiles.Count; c++) {
				parser.Init(config.InputFiles[c], config.Format);
				parser.Parse(config.InputFiles[c]);

				TextWriter writer;
				if (config.ErrorFile != null) writer = File.CreateText(config.ErrorFile);
				else writer = System.Console.Error;
				if (config.IsErrorXML) {
					writeXML(writer, config, parser.CodeElementsSnapshot.ParserDiagnostics, parser.Snapshot.Diagnostics);
				} else {
					write(writer, parser.CodeElementsSnapshot.ParserDiagnostics, parser.Snapshot.Diagnostics);
				}

				var codegen = new TypeCobol.Compiler.Generator.TypeCobolGenerator(parser.Snapshot, null);
				codegen.GenerateCobolText(config.OutputFiles[c]);
			}
		}

		private static void writeXML(TextWriter w, Config config,
								IEnumerable<Compiler.Diagnostics.Diagnostic> ceerrors,
								IList<Compiler.AntlrUtils.ParserDiagnostic> pcerrors) {
			var writer = System.Xml.XmlWriter.Create(w);
			writer.WriteStartElement("BUILD");
			writer.WriteStartElement("PACKAGE");
			writeFiles(writer, config.InputFiles);
			foreach(var error in ceerrors) writeError(writer, error);
			foreach(var error in pcerrors) writeError(writer, error);
			writeFiles(writer, config.OutputFiles, "OUT");
			writer.WriteStartElement("STATEMENTTABLE");
			writer.WriteEndElement();// STATEMENTTABLE
			writer.WriteEndElement();// PACKAGE
			writer.WriteEndElement();// BUILD
			writer.Close();
		}

		private static void writeError(System.Xml.XmlWriter writer, Compiler.Diagnostics.Diagnostic error) {
			writer.WriteStartElement("MESSAGE");
			writer.WriteElementString("MSGNUMBER", "TC"+AsIBMSuffix(error.Info.Severity));
			writer.WriteElementString("MSGLINE", "1"); //TODO
			writer.WriteElementString("MSGFILE", "1"); //TODO
			writer.WriteElementString("MSGTEXT", error.Message);
			writer.WriteEndElement();// MESSAGE
		}

		private static string AsIBMSuffix(Compiler.Diagnostics.Severity severity) {
			if (severity == Compiler.Diagnostics.Severity.Error) return "-E";
			if (severity == Compiler.Diagnostics.Severity.Warning) return "-W";
			if (severity == Compiler.Diagnostics.Severity.Info) return "-I";
			throw new System.NotImplementedException("Unsupported severity: "+severity);
		}

		private static void writeFiles(System.Xml.XmlWriter writer, IList<string> paths, string prefix = "") {
			writer.WriteStartElement(prefix+"FILEREFERENCETABLE");
			writer.WriteElementString(prefix+"FILECOUNT", paths.Count.ToString());
			int c = 1;
			foreach(var path in paths) {
				writer.WriteStartElement(prefix+"FILE");
				writer.WriteElementString(prefix+"FILENUMBER", (c++).ToString());
				writer.WriteElementString(prefix+"FILENAME", new FileInfo(path).FullName);
				writer.WriteEndElement();// FILE
			}
			writer.WriteEndElement();// FILEREFERENCETABLE
		}

		private static void write(TextWriter writer,
								IEnumerable<Compiler.Diagnostics.Diagnostic> ceerrors,
								IList<Compiler.AntlrUtils.ParserDiagnostic> pcerrors) {
			write(writer, "CodeElements", ceerrors);
			write(writer, "ProgramClass", pcerrors);
			writer.Close();
		}

		private static void write(TextWriter writer, string title, IEnumerable<Compiler.Diagnostics.Diagnostic> errors) {
			int nberrors = 0;
			foreach(var e in errors) nberrors++;
			if (nberrors==0) return;
			writer.WriteLineAsync();
			writer.WriteAsync(nberrors.ToString());
			writer.WriteAsync(' ');
			writer.WriteAsync(title);
			writer.WriteAsync(" error");
			writer.WriteLineAsync((nberrors>1)?"s:":":");
			foreach(var e in errors) writer.WriteLineAsync(e.ToString());
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
