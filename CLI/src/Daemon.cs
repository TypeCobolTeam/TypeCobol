using System;
using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using Mono.Options;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Text;
using SimpleMsgPack;
using TypeCobol.Server.Serialization;

namespace TypeCobol.Server
{

    class Server
    {
        static int Main(string[] argv)
        {
           // bool a = DiffTest();
            bool help = false;
            bool version = false;
            bool once = false;
            bool startClient = false;
            var config = new Config();
            var pipename = "TypeCobol.Server";

            var p = new OptionSet() {
				"USAGE",
				"  "+PROGNAME+" [OPTIONS]... [PIPENAME]",
				"",
				"VERSION:",
				"  "+PROGVERSION,
				"",
				"DESCRIPTION:",
				"  Run the TypeCobol parser server.",
                {"K|start server & execute commandline", "Start the server if not already started, and executes commandline.", v=>startClient = (v!=null)},
				{ "1|once",  "Parse one set of files and exit. If present, this option does NOT launch the server.", v => once = (v!=null) },
				{ "i|input=", "{PATH} to an input file to parse. This option can be specified more than once.", v => config.InputFiles.Add(v) },
				{ "o|output=","{PATH} to an ouput file where to generate code. This option can be specified more than once.", v => config.OutputFiles.Add(v) },
				{ "g|generate",  "If present, this option generates code corresponding to each input file parsed.", v => config.Codegen = (v!=null) },
				{ "d|diagnostics=", "{PATH} to the error diagnostics file.", v => config.ErrorFile = v },
				{ "s|skeletons=", "{PATH} to the skeletons files.", v => config.skeletonPath = v },
//				{ "p|pipename=",  "{NAME} of the communication pipe to use. Default: "+pipename+".", (string v) => pipename = v },
				{ "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\"(this is the default), \"zos\", or \"utf8\". "
								+"If this option is not present, the parser will attempt to guess the {ENCODING} automatically.",
								v => config.Format = CreateFormat(v, ref config)
				},
				{ "y|intrinsic=", "{PATH} to intrinsic definitions to load.\nThis option can be specified more than once.", v => config.Copies.Add(v) },
				{ "c|copies=",  "Folder where COBOL copies can be found.\nThis option can be specified more than once.", v => config.CopyFolders.Add(v) },
				{ "h|help",  "Output a usage message and exit.", v => help = (v!=null) },
                { "V|version",  "Output the version number of "+PROGNAME+" and exit.", v => version = (v!=null) },
			};

            try
            {
                List<string> args;
                try
                {

                    args = p.Parse(argv);
                }
                catch (OptionException ex)
                {
                    return exit(1, ex.Message);
                }

                if (help)
                {
                    p.WriteOptionDescriptions(Console.Out);
                    return 0;
                }
                if (version)
                {
                    Console.WriteLine(PROGVERSION);
                    return 0;
                }
                if (config.OutputFiles.Count > 0 && config.InputFiles.Count != config.OutputFiles.Count)
                    return exit(2, "The number of output files must be equal to the number of input files.");
                if (config.OutputFiles.Count == 0 && config.Codegen)
                    foreach (var path in config.InputFiles) config.OutputFiles.Add(path + ".cee");

                if (args.Count > 0) pipename = args[0];

                //"startClient" will be true when "-K" is passed as an argument in command line.
                if (startClient && once)
                {
#if DEBUG
                    var watch = System.Diagnostics.Stopwatch.StartNew();
#endif
                    using (NamedPipeClientStream namedPipeClient = new NamedPipeClientStream(pipename))
                    {
                        try
                        {
                            namedPipeClient.Connect(100);
                        }
                        catch (TimeoutException tEx)
                        {
                            System.Diagnostics.Process process = new System.Diagnostics.Process();
                            System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
                            startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
                            startInfo.FileName = "cmd.exe";
                            startInfo.Arguments = @"/c TypeCobol.CLI.exe";
                            process.StartInfo = startInfo;
                            process.Start();

                            namedPipeClient.Connect(1000);
                        }

                        namedPipeClient.WriteByte(68);
                        namedPipeClient.ReadByte();

                        ConfigSerializer configSerializer = new ConfigSerializer();
                        var configBytes = configSerializer.Serialize(config);

                        namedPipeClient.Write(configBytes, 0, configBytes.Length);

                        var readbyte = namedPipeClient.ReadByte();
                        
#if DEBUG
                        watch.Stop();
                        var elapsedTime = watch.ElapsedMilliseconds;
                        Console.WriteLine(elapsedTime);
#endif
               }
                }
                //option -1
                else if (once)
                {
#if DEBUG
                    var watch = System.Diagnostics.Stopwatch.StartNew();
#endif
                    CLI.runOnce(config);
#if DEBUG
                    watch.Stop();
                    var elapsedTime = watch.ElapsedMilliseconds;
                    Console.WriteLine(elapsedTime);
#endif
                }
                else
                {
                    runServer(pipename);
                }
            }
            catch (Exception e)
            {
                return exit(1, e.Message);
            }
            return 0;
        }

        private static Compiler.DocumentFormat CreateFormat(string encoding, ref Config config)
        {
            config.EncFormat = encoding;

            if (encoding == null) return null;
            if (encoding.ToLower().Equals("zos")) return TypeCobol.Compiler.DocumentFormat.ZOsReferenceFormat;
            if (encoding.ToLower().Equals("utf8")) return TypeCobol.Compiler.DocumentFormat.FreeUTF8Format;
            /*if (encoding.ToLower().Equals("rdz"))*/
            return TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
        }

        private static void AddError(AbstractErrorWriter writer, string message, string path, string errorCode)
        {
            var error = new TypeCobol.Tools.Diagnostic();
            error.Message = message;
            error.Code = errorCode;
            try { error.Source = writer.Inputs[path]; }
            catch (KeyNotFoundException) { error.Source = writer.Count.ToString(); }
            writer.AddErrors(path, error);
            Console.WriteLine(error.Message);
        }

        private static void runServer(string pipename)
        {
            var parser = new Parser();

            var pipe = new NamedPipeServerStream(pipename, PipeDirection.InOut, 4, PipeTransmissionMode.Message);

            Commands.Register(66, new Parse(parser, pipe, pipe));
            Commands.Register(67, new Initialize(parser, pipe, pipe));
            Commands.Register(68, new RunCommandLine(parser, pipe, pipe));
            var decoder = new TypeCobol.Server.Serialization.IntegerSerializer();
            System.Console.WriteLine("NamedPipeServerStream thread created. Wait for a client to connect on " + pipename);
            //bool a = true;
            while (true)
            {
                pipe.WaitForConnection(); // blocking
                //System.Console.WriteLine("Client connected.");
                int code = 0;
                try
                {
                    code = decoder.Deserialize(pipe);
                    var command = Commands.Get(code);

                    if (code.Equals(68))
                    {
                        pipe.WriteByte(68);
                        command.execute(pipe);
                    }
                    else
                        command.execute();
                }
                catch (IOException ex) { Console.WriteLine("Error: {0}", ex.Message); }
                catch (System.Runtime.Serialization.SerializationException ex) { Console.WriteLine("Error: {0}", ex.Message); }
                finally
                {
                    pipe.Disconnect();

                    if(code.Equals(66) || code.Equals(67))
                    {
                        pipe.Close();
                    }
                }
            }
        }

        private static readonly string PROGNAME = System.AppDomain.CurrentDomain.FriendlyName;
        private static readonly string PROGVERSION = GetVersion();

        private static string GetVersion()
        {
            System.Reflection.Assembly assembly = System.Reflection.Assembly.GetExecutingAssembly();
            var info = System.Diagnostics.FileVersionInfo.GetVersionInfo(assembly.Location);
            return info.FileVersion;
        }

        static int exit(int code, string message)
        {
            string errmsg = PROGNAME + ": " + message + "\n";
            errmsg += "Try " + PROGNAME + " --help for usage information.";
            Console.WriteLine(errmsg);
            return code;
        }

    }
}
