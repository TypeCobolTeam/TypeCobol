using System;
using System.IO.Pipes; // NamedPipeServerStream, PipeDirection
using Mono.Options;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Text;
using SimpleMsgPack;
using TypeCobol.Server.Serialization;
using Analytics;
using TypeCobol.CustomExceptions;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Server {

    class Server {
        enum StartClient {
            No, HiddenWindow, NormalWindow
        }
        static int Main(string[] argv) {
            bool help = false;
            bool version = false;
            bool once = false;
            StartClient startClient = StartClient.No;
            var config = new TypeCobolConfiguration();
            config.CommandLine = string.Join(" ", argv);
            var pipename = "TypeCobol.Server";

            var p = TypeCobolOptionSet.GetCommonTypeCobolOptions(config);

            //Add custom options for CLI
            p.Add(string.Format("USAGE\n {0} [OPTIONS]... [PIPENAME]\n VERSION:\n {1} \n DESCRIPTION: \n Run the TypeCObol parser server", PROGNAME, PROGVERSION));
            p.Add("k|startServer:",
                "Start the server if not already started, and executes commandline.\n" + "By default the server is started in window mode\n" + "'{hidden}' hide the window.",
                v =>
                {
                    if ("hidden".Equals(v, StringComparison.InvariantCultureIgnoreCase))
                    {
                        startClient = StartClient.HiddenWindow;
                    }
                    else
                    {
                        startClient = StartClient.NormalWindow;
                    }
                }); 
            p.Add("1|once",  "Parse one set of files and exit. If present, this option does NOT launch the server.", v => once = (v != null));
            p.Add("h|help", "Output a usage message and exit.", v => help = (v != null));
            p.Add("V|version", "Output the version number of " + PROGNAME + " and exit.", v => version = (v != null));


            //Add DefaultCopies to running session
            var folder = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName);
            config.CopyFolders.Add(folder + @"\DefaultCopies\");

            try {
                List<string> args;
		        try {

		            args = p.Parse(argv);

                } catch (OptionException ex) {
                    return exit(ReturnCode.FatalError, ex.Message);
		        }

		        if (help) {
		            p.WriteOptionDescriptions(Console.Out);
		            return 0;
		        }
		        if (version) {
		            Console.WriteLine(PROGVERSION);
		            return 0;
		        }
                if(config.Telemetry)
                {
                    AnalyticsWrapper.Telemetry.DisableTelemetry = false; //If telemetry arg is passed enable telemetry
                }

                if (config.OutputFiles.Count == 0 && config.ExecToStep >= ExecutionStep.Generate)
                    config.ExecToStep = ExecutionStep.SemanticCheck; //If there is no given output file, we can't run generation, fallback to SemanticCheck

		        if (config.OutputFiles.Count > 0 && config.InputFiles.Count != config.OutputFiles.Count)
		            return exit(ReturnCode.OutputFileError, "The number of output files must be equal to the number of input files.");

		        if (args.Count > 0) pipename = args[0];


                //"startClient" will be true when "-K" is passed as an argument in command line.
                if (startClient != StartClient.No && once) {
                    pipename= "TypeCobol.Server";
                    using (NamedPipeClientStream namedPipeClient = new NamedPipeClientStream(pipename))
                    {
                        try {
                            namedPipeClient.Connect(100);
		                } catch (TimeoutException tEx) {
                            System.Diagnostics.Process process = new System.Diagnostics.Process();
                            System.Diagnostics.ProcessStartInfo startInfo = new System.Diagnostics.ProcessStartInfo();
		                    if (startClient == StartClient.NormalWindow) {
		                        startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Normal;
		                    } else {
                                startInfo.WindowStyle = System.Diagnostics.ProcessWindowStyle.Hidden;
                            }
		                    startInfo.FileName = "cmd.exe";
                            startInfo.Arguments = @"/c " + folder + Path.DirectorySeparatorChar+ "TypeCobol.CLI.exe";
                            process.StartInfo = startInfo;
                            process.Start();
                            
                            namedPipeClient.Connect(1000);
		                }

                        namedPipeClient.WriteByte(68);

                        ConfigSerializer configSerializer = new ConfigSerializer();
                        var configBytes = configSerializer.Serialize(config);

                        namedPipeClient.Write(configBytes, 0, configBytes.Length);
                        //Wait for the response "job is done"
                        var returnCode = namedPipeClient.ReadByte(); //Get running server ReturnCode
                        return exit((ReturnCode)returnCode, "");
                    }
				}
                
                //option -1
                else if (once) {
                    var returnCode = CLI.runOnce(config);
                    if (returnCode != ReturnCode.Success)
                        return exit(returnCode, "Operation failled");
                } else {
                    runServer(pipename);
                }
			}
            catch (Exception e) {
                AnalyticsWrapper.Telemetry.TrackException(e);
                return exit(ReturnCode.FatalError, e.Message);
			}

            return exit((int)ReturnCode.Success, "Success");
		}

        /// <summary>
        /// Add an error message
        /// </summary>
        /// <param name="writer">Error Writer</param>
        /// <param name="messageCode">Message's code</param>
        /// <param name="message">The text message</param>
        /// <param name="path">The source file path</param>
		internal static void AddError(AbstractErrorWriter writer, MessageCode messageCode, string message, string path)
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
        internal static void AddError(AbstractErrorWriter writer, MessageCode messageCode, int columnStart, int columnEnd, int lineNumber, string message, string path)
        {
            Diagnostic diag = new Diagnostic(messageCode, columnStart, columnEnd, lineNumber,
                message != null
                ? (path != null ? new object[2] { message, path } : new object[1] { message })
                : (path != null ? new object[1] { path } : new object[0]));
            diag.Message = message;
            writer.AddErrors(path, diag);
            Console.WriteLine(string.Format("Code : {0} - Message : {1}", messageCode , message));
		}

        private static void runServer(string pipename) {
			var parser = new Parser();

            var pipe = new NamedPipeServerStream(pipename, PipeDirection.InOut, 4, PipeTransmissionMode.Message);

			Commands.Register(66, new Parse(parser, pipe, pipe));
			Commands.Register(67, new Initialize(parser, pipe, pipe));
            Commands.Register(68, new RunCommandLine(parser, pipe, pipe));
			var decoder = new TypeCobol.Server.Serialization.IntegerSerializer();
			System.Console.WriteLine("NamedPipeServerStream thread created. Wait for a client to connect on " + pipename);
			while (true) {
				pipe.WaitForConnection(); // blocking
                //System.Console.WriteLine("Client connected.");
			    int code = 0;
                try {
					code = decoder.Deserialize(pipe);
					var command = Commands.Get(code);
                    command.execute(pipe);
				}
				catch (IOException ex) { Console.WriteLine("Error: {0}", ex.Message); }
                catch (System.Runtime.Serialization.SerializationException ex) { Console.WriteLine("Error: {0}", ex.Message); }
                finally {
                    pipe.Disconnect();
                    //68 is a server that need to stay alive
                    if(code.Equals(66) || code.Equals(67)) {
                        pipe.Close();
                    }
                }
			}
		}
        


		private static readonly string PROGNAME = System.AppDomain.CurrentDomain.FriendlyName;
		private static readonly string PROGVERSION = GetVersion();

		private static string GetVersion() {
			System.Reflection.Assembly assembly = System.Reflection.Assembly.GetExecutingAssembly();
			var info = System.Diagnostics.FileVersionInfo.GetVersionInfo(assembly.Location);
			return info.FileVersion;
		}

		static int exit(ReturnCode code, string message) {
			string errmsg = PROGNAME+": "+message+"\n";
			errmsg += "Try "+PROGNAME+" --help for usage information.";
			Console.WriteLine(errmsg);

            AnalyticsWrapper.Telemetry.TrackEvent(string.Format("[ReturnCode] {0} : {1}", code.ToString(), message));
            AnalyticsWrapper.Telemetry.EndSession(); //End Telemetry session and force data sending
            return (int)code;
		}

    }

    /// <summary>
    /// Categories of ReturnCode:
    /// * 0000 : Everything is ok
    ///         Output files are generated
    /// * 0001 to 0999 : Ok but there are warnings.
    ///         Output files are generated
    ///         Diagnostic file should contains warnings
    /// * >= 1000 : Errors
    ///         Output files are NOT generated
    ///         Diagnostic file should contains errors and warnings
    /// </summary>
    public enum ReturnCode
    {
        Success = 0,
        
        //Warnings
        Warning = 1,            //Warning(s) issued during parsing of input file


        //Errors
        ParsingDiagnostics = 1000,  //Syntax or semantic error in one or more input file
        OutputFileError = 1001,     //CLI parameters error
        MissingCopy = 1002,         //Use of option --hastonmissingcopy and at least one COPY is missing
        GenerationError = 1003,     //Error during Code generation
        FatalError = 1004,          //Not managed exception
    }
}
