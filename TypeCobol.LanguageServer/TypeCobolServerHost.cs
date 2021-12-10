using Mono.Options;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading;
using TypeCobol.LanguageServer.JsonRPC;
using TypeCobol.LanguageServer.StdioHttp;
using TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol;
using TypeCobol.LanguageServer.Utilities;
using TypeCobol.Tools;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// Main executable hosting the language server
    /// </summary>
    class TypeCobolServerHost
    {
        public static Queue<MessageActionWrapper> MessagesActionQueue { get; set; }
        public static bool LsrMode { get; set; }
        /// <summary>
        /// Program name from Assembly name
        /// </summary>
        public static string ProgName
        {
            get
            {
                return Assembly.GetExecutingAssembly().GetName().Name;
            }
        }
        /// <summary>
        /// Assembly version
        /// </summary>
        public static string Version
        {
            get
            {
                return Assembly.GetExecutingAssembly().GetName().Version.ToString();
            }
        }

        /// <summary>
        /// The Lsr path
        /// </summary>
        public static string LsrPath { get; set; }

        /// <summary>
        /// The Lsr Script.
        /// </summary>
        public static string LsrScript { get; set; }

        /// <summary>
        /// The Lsr Options.
        /// </summary>
        public static string LsrOptions { get; set; }
   
        /// <summary>
        /// The Log file
        /// </summary>
        public static string LogFile { get; set; }

        /// <summary>
        /// The Log level
        /// </summary>
        public static ServerLogLevel LogLevel { get; set; }

        /// <summary>
        /// Lsr Testing Source document
        /// </summary>
        public static bool LsrSourceTesting { get; set; }
        /// <summary>
        /// Lsr Testing Scanning of the document.
        /// </summary>
        public static bool LsrScannerTesting { get; set; }
        /// <summary>
        /// Lsr Testing preprocessed Source document
        /// </summary>
        public static bool LsrPreprocessTesting { get; set; }
        /// <summary>
        /// Lsr Testing parsing
        /// </summary>
        public static bool LsrParserTesting { get; set; }
        /// <summary>
        /// Lsr Testing semantic phase
        /// </summary>
        public static bool LsrSemanticTesting { get; set; }
        /// <summary>
        /// Lsr code quality check phase
        /// </summary>
        public static bool LsrCodeAnalysisTesting { get; set; }

        /// <summary>
        /// Timer Disabled for TypeCobol.LanguageServer.
        /// </summary>
        public static bool TimerDisabledOption { get; set; }

        /// <summary>
        /// Are Log message notifications enabled ? false if yes, true otherwise.
        /// </summary>
        public static bool NoLogsMessageNotification { get; set; }

        /// <summary>
        /// True to use ANTLR for parsing a program
        /// </summary>
        public static bool UseAntlrProgramParsing { get; set; }

        /// <summary>
        /// True to use Euro-Information replacement rules
        /// </summary>
        public static bool UseEuroInformationLegacyReplacingSyntax { get; set; }

        /// <summary>
        /// Are we supporting Syntax Coloring Notifications.    
        /// </summary>
        public static bool UseSyntaxColoring { get; set; }

        /// <summary>
        /// Are we supporting Outline Refresh Notifications.    
        /// </summary>
        public static bool UseOutlineRefresh { get; set; }

        /// <summary>
        /// No Copy and Dependency files watchers.
        /// </summary>
        public static bool NoCopyDependencyWatchers { get; set; }

#if EUROINFO_RULES
        /// <summary>
        /// A Path to a file of CPY Copy names
        /// </summary>
        private static string CpyCopyNamesMapFilePath { get; set; }
#endif

        /// <summary>
        /// Are we supporting CFG/DFA Refresh Notifications.
        /// </summary>
        public static TypeCobolCustomLanguageServer.UseCfgMode UseCfg { get; set; }

        public static System.Diagnostics.Process Process;

        /// <summary>
        /// Custom extensions Dll Paths
        /// </summary>
        public static List<string> Extensions = new List<string>();

        /// <summary>
        /// Run the Lsr Process
        /// </summary>
        /// <param name="fullPath">full path of the process</param>
        /// <param name="arguments">process arguments</param>
        /// <returns>true if the process has been run, false otherwise.</returns>
        protected static bool StartLsr(string fullPath, string arguments)
        {
            Process = new System.Diagnostics.Process();
            Process.StartInfo.FileName = fullPath;
            if (arguments != null)
                Process.StartInfo.Arguments = arguments;
            Process.StartInfo.UseShellExecute = false;
            Process.StartInfo.RedirectStandardOutput = true;
            Process.StartInfo.RedirectStandardInput = true;
            //Start the process
            try
            {
                if (!Process.Start())
                {
                    return false;
                }
                else
                {
                    return true;
                }
            }
            catch 
            {
                return false;
            }
        }

        static int Main(string[] args)
        {
            bool help = false;
            bool version = false;

            var p = new OptionSet()
            {
                "USAGE",
                "  "+ProgName+" [OPTIONS]",
                "",
                "VERSION:",
                "  "+Version,
                "",
                "DESCRIPTION:",
                "  Run the Language Server Robot.",
                { "l|loglevel=",  "Logging level (1=Lifecycle, 2=Message, 3=Protocol).", (string v) =>
                    {
                        if (v != null)
                        {
                            try
                            {
                                // args[0] : Trace level
                                LogLevel = (ServerLogLevel)Int32.Parse(v);
                                if (!System.Enum.IsDefined(typeof(ServerLogLevel), (Int32)LogLevel))
                                {
                                    LogLevel = ServerLogLevel.Protocol;
                                }
                            }
                            catch (Exception e)
                            {
                                System.Console.Error.WriteLine(e.Message);
                            }
                        }
                    }
                },
                { "v|version","Show version", _ => version = true },
                { "h|help","Show help", _ => help = true },
                { "lf|logfile=","{PATH} the target log file", (string v) => LogFile = v },
                { "nologs","No log message notifications", _ => NoLogsMessageNotification = true },
                { "r|robot",  "Robot Client mode.", _ => LsrMode = true },
                { "lsr=","{PATH} the lsr path", (string v) => LsrPath = v },
                { "s|script=","{PATH} script path in lsr", (string v) => LsrScript = v },
                { "td|timerdisabled","Disable the delay that handle the automatic launch of Node Phase analyze", _ => TimerDisabledOption = true },
                { "ro|roptions=","LSR options", (string v) => LsrOptions = v + " " },
                { "tsource",  "Source document testing mode.", _ => LsrSourceTesting = true},
                { "tscanner",  "Scanner testing mode.", _ => LsrScannerTesting = true},
                { "tpreprocess",  "Preprocessing testing mode.", _ => LsrPreprocessTesting = true},
                { "tparser",  "parsing testing mode.", _ => LsrParserTesting = true},
                { "tsemantic",  "Semantic analysis testing mode.", _ => LsrSemanticTesting = true},
                { "tcodeanalysis",  "Code quality analysis testing mode.", _ => LsrCodeAnalysisTesting = true},
                { "antlrp",  "Use ANTLR to parse a Program.", _ => UseAntlrProgramParsing = true},
                { "dcs|disablecopysuffixing", "Deactictivate Euro Information suffixing", v => UseEuroInformationLegacyReplacingSyntax = false },
                { "sc|syntaxcolor",  "Syntax Coloring Support.", _ => UseSyntaxColoring = true},
                { "ol|outlineRefresh",  "Outline Support.", _ => UseOutlineRefresh = true},
#if EUROINFO_RULES
                { "ycpl|ycopylist=", "{PATH} to a file of CPY copy names uppercase sorted.", v => CpyCopyNamesMapFilePath = v },
#endif
                { "cfg=",  "{dot output mode} Control Flow Graph support and Dot Output mode: No/0, AsFile/1 or AsContent/2.",
                    (String m) => {TypeCobolCustomLanguageServer.UseCfgMode ucm = TypeCobolCustomLanguageServer.UseCfgMode.No;
                        Enum.TryParse(m, out ucm); UseCfg = ucm; }  },
                { "ext|extension=", "{PATH} to a custom DLL file containing parser extension(s). This option can be specified more than once.", v => Extensions.Add(v) },
                { "now|nowatchers",  "No Copy and Dependency files watchers.", _ => NoCopyDependencyWatchers = true}
            };

            System.Collections.Generic.List<string> arguments;
            try { arguments = p.Parse(args); }
            catch (OptionException ex) { return exit(1, ex.Message); }

            if (help)
            {
                p.WriteOptionDescriptions(System.Console.Out);
                return 0;
            }
            if (version)
            {
                System.Console.WriteLine(Version);
                return 0;
            }

            //TODO #2091 Register loggers according to config (FileLogger or DebugLogger, external loggers)

            TextWriter logWriter = null;
            if (LogFile != null)
            {
                try
                {
                    StreamWriter sw = new StreamWriter(LogFile);
                    sw.AutoFlush = true;
                    logWriter = sw;
                }
                catch (Exception e)
                {
                    if (!LsrMode)
                    {
                        System.Console.Error.WriteLine(e.Message);
                    }
                }
            }
            if (logWriter == null)
            {
                logWriter = new DebugTextWriter();
            }
            if (LsrMode && LsrPath != null && LsrScript != null)
            {
                if (!StartLsr(LsrPath, (LsrOptions ?? "") + "-ioc -c -script=" + LsrScript))
                {
                    System.Console.Error.WriteLine("Fail to run LSR process");
                    return -1;
                }
            }
            //Run this server
            try
            {
                //Queue storing messages coming from client, this queue is read by readingThread
                MessagesActionQueue = new Queue<MessageActionWrapper>();

                // Configure the protocols stack
                var httpServer = new StdioHttpServer(Encoding.UTF8, LogLevel, logWriter, MessagesActionQueue);
                httpServer.IsLsrTdMode = TimerDisabledOption;
                if (Process != null)
                {
                    httpServer.RedirectedInputStream = Process.StandardOutput.BaseStream;
                    httpServer.RedirectedOutpuStream = Process.StandardInput;
                }
                var jsonRPCServer = new JsonRPCServer(httpServer);
                var typeCobolServer = new TypeCobolCustomLanguageServer(jsonRPCServer, MessagesActionQueue);

                typeCobolServer.NoLogsMessageNotification = NoLogsMessageNotification;

                if (LsrSourceTesting) typeCobolServer.LsrTestingLevel = LsrTestingOptions.LsrSourceDocumentTesting;
                if (LsrScannerTesting) typeCobolServer.LsrTestingLevel = LsrTestingOptions.LsrScanningPhaseTesting;
                if (LsrPreprocessTesting) typeCobolServer.LsrTestingLevel = LsrTestingOptions.LsrPreprocessingPhaseTesting;
                if (LsrParserTesting) typeCobolServer.LsrTestingLevel = LsrTestingOptions.LsrParsingPhaseTesting;
                if (LsrSemanticTesting) typeCobolServer.LsrTestingLevel = LsrTestingOptions.LsrSemanticPhaseTesting;
                if (LsrCodeAnalysisTesting) typeCobolServer.LsrTestingLevel = LsrTestingOptions.LsrCodeAnalysisPhaseTesting;
                typeCobolServer.TimerDisabledOption = TimerDisabledOption;
                typeCobolServer.UseAntlrProgramParsing = UseAntlrProgramParsing;
                typeCobolServer.UseEuroInformationLegacyReplacingSyntax = UseEuroInformationLegacyReplacingSyntax;
                typeCobolServer.UseSyntaxColoring = UseSyntaxColoring;
                typeCobolServer.UseOutlineRefresh = UseOutlineRefresh;
                typeCobolServer.UseCfgDfaDataRefresh = UseCfg;
                typeCobolServer.ExtensionManager = new ExtensionManager(Extensions); //TODO #2091 errors happening here won't be traced until some loggers are registered
                typeCobolServer.NoCopyDependencyWatchers = NoCopyDependencyWatchers;
#if EUROINFO_RULES
                typeCobolServer.CpyCopyNamesMapFilePath = CpyCopyNamesMapFilePath;
#endif
                //Creating the thread that will read mesages and handle them 
                var backgroundExecutionThread = new Thread(() => { MessageHandler(jsonRPCServer, typeCobolServer); }) { IsBackground = true };
                backgroundExecutionThread.Start();

                // Start listening to incoming request (block, infinite loop)
                httpServer.StartReceivingMessagesFor(jsonRPCServer);
            }
            finally
            {
                //TODO #2091 LoggingSystem.Shutdown()
                if (logWriter != System.Console.Error)
                {
                    logWriter.Close();
                }
            }
            return 0;
        }

        /// <summary>
        /// Command Line Option Set
        /// </summary>
        public static OptionSet Options
        {
            get;
            internal set;
        }

        static int exit(int code, string message)
        {
            string errmsg = ProgName + ": " + message + "\n";
            errmsg += "Try " + ProgName + " --help for usage information.";
            System.Console.WriteLine(errmsg);
            return code;
        }

        /// <summary>
        /// Method of backgroundExecutionThread. It will Loop until the end of main Thread.
        /// This method is going to read the MessagesActionQueue and do associated action.
        /// In case of JsonRPC message, it will be pass to a MessageHandler.
        /// In case of an action, it will execute it. 
        /// </summary>
        /// <param name="messageHandler"></param>
        /// <param name="typeCobolServer"></param>
        static void MessageHandler(IMessageHandler messageHandler, TypeCobolServer typeCobolServer)
        {
            while (true)
            {
                Thread.Sleep(1); //To preserve processor use

                MessageActionWrapper messageActionWrapper = null;
                lock (MessagesActionQueue)
                {
                    if (MessagesActionQueue.Any())
                    {
                        messageActionWrapper = MessagesActionQueue.Dequeue(); //Pop out message from queue
                    }
                }
                if (messageActionWrapper == null)
                    continue;

                if (messageActionWrapper.MessageKind == MessageKind.JSonMessage)
                    messageHandler.HandleMessage(messageActionWrapper.Message, messageActionWrapper.MessageServer); //Give this mesage to the real handler
                else if (messageActionWrapper.MessageKind == MessageKind.Action)
                {
                    try
                    {
                        messageActionWrapper.Action(); //Execute queued action
                    }
                    catch (Exception e)
                    {
                        typeCobolServer.NotifyException(e);
                    }
                }
            }
        }
    }
}
