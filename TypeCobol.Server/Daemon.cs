using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using Mono.Options;
using System.Collections.Generic;

namespace TypeCobol.Server
{
	class Server {

		class Config {
			public new List<string> InputFiles  = new List<string>();
			public new List<string> OutputFiles = new List<string>();
			public string ErrorFile = "errors.xml";
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
				{ "i|input=",   "{PATH} to an input file to parse. This option can be specified more than once.", (string v) => config.InputFiles.Add(v) },
				{ "o|output=",  "{PATH} to an ouput file where to generate code. This option can be specified more than once.", (string v) => config.OutputFiles.Add(v) },
				{ "e|errors=",  "{PATH} to the errors file.", (string v) => config.ErrorFile = v },
//				{ "p|pipename=",  "{NAME} of the communication pipe to use. Default: "+pipename+".", (string v) => pipename = v },
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

		private static void runOnce(Config config) {
			var parser = new Parser("TypeCobol.Server");
			for(int c=0; c<config.InputFiles.Count; c++) {
				parser.Parse(config.InputFiles[c]);
				var codegen = new TypeCobol.Compiler.Generator.TypeCobolGenerator(parser.Snapshot, null);
				codegen.GenerateCobolText(config.OutputFiles[c]);
			}
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
