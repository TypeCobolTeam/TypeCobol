using System;
using Mono.Options;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using TypeCobol.Compiler.Diagnostics;
using Analytics;
using TypeCobol.Logging;
using TypeCobol.Tools;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Server {

    class Server {
        static int Main(string[] argv)
        {
            bool help = false;
            bool version = false;

            var config = new TypeCobolConfiguration();
            config.CommandLine = string.Join(" ", argv);

            var p = new OptionSet();

            //Add custom options for CLI
            p.Add(string.Format("USAGE\n {0} [OPTIONS]... [PIPENAME]\n VERSION:\n {1} \n DESCRIPTION: \n Run the TypeCobol parser server", PROGNAME, PROGVERSION));
            p.Add("h|help", "Output a usage message and exit.", v => help = (v != null));
            p.Add("V|version", "Output the version number of " + PROGNAME + " and exit.", v => version = (v != null));
            p.Add("1|once", "Parse one set of files and exit. DEPRECATED : CLI always uses Once mode so the option is not evaluated.", v => {});
            p.Add("ca|customanalyzer=", "OBSOLETE - Use 'ext' option instead.", v => config.Extensions.Add(v));
            p.Add("ext|extension=", "{PATH} to a custom DLL file containing parser extension(s). This option can be specified more than once.", v => config.Extensions.Add(v));
#if EUROINFO_RULES
            string cpyCopyNamesMapFilePath = null;
            p.Add("ycpl|ycopylist=", "{PATH} to a file of CPY copy names uppercase sorted.", v => cpyCopyNamesMapFilePath = v);
#endif

            p.AddRange(TypeCobolOptionSet.GetCommonTypeCobolOptions(config));

            //Add DefaultCopies to running session
            var folder = Path.GetDirectoryName(Process.GetCurrentProcess().MainModule.FileName);
            config.CopyFolders.Add(folder + @"\DefaultCopies\");

            try
            {
                var errors = TypeCobolOptionSet.InitializeCobolOptions(config, argv, p);

                if (help)
                {
                    p.WriteOptionDescriptions(Console.Out);
                    return 0;
                }

                if (errors.Any())
                    return exit(errors);

                if (version)
                {
                    Console.WriteLine(PROGVERSION);
                    return 0;
                }

                //TODO #2091 Add ConsoleLogger and FileLogger

                //External loggers if extensions have been provided
                var extensionManager = new ExtensionManager(config.Extensions);
                foreach (var externalLogger in extensionManager.Activate<ILogger>())
                {
                    LoggingSystem.RegisterLogger(externalLogger);
                }

                if (config.Telemetry)
                {
                    AnalyticsWrapper.Telemetry.TelemetryVerboseLevel = TelemetryVerboseLevel.CodeGeneration; //If telemetry arg is passed enable telemetry
                }

#if EUROINFO_RULES
                config.LoadCpyCopyNameMap(cpyCopyNamesMapFilePath);
#endif

                if (config.OutputFiles.Count == 0 && config.ExecToStep >= ExecutionStep.Generate)
                    config.ExecToStep = ExecutionStep.QualityCheck; //If there is no given output file, we can't run generation, fallback to QualityCheck

                var returnCode = CLI.runOnce(config, extensionManager);
                if (returnCode != ReturnCode.Success)
                    return exit(returnCode, "Operation failed");
            }
            catch (Exception e)
            {
                AnalyticsWrapper.Telemetry.TrackException(e, null);
                return exit(ReturnCode.FatalError, e.Message);
            }

            return exit((int)ReturnCode.Success, "Success");
		}

        internal static void AddError(AbstractErrorWriter writer, string path, Diagnostic diagnostic)
        {
            writer.AddErrors(path, diagnostic);
            Console.WriteLine(diagnostic);
        }

		private static readonly string PROGNAME = System.AppDomain.CurrentDomain.FriendlyName;
		private static readonly string PROGVERSION = GetVersion();

		private static string GetVersion() {
			System.Reflection.Assembly assembly = System.Reflection.Assembly.GetExecutingAssembly();
			var info = System.Diagnostics.FileVersionInfo.GetVersionInfo(assembly.Location);
			return info.FileVersion;
		}

        static int exit(ReturnCode code, string message)
        {
            string errmsg = "Code: " + (int)code + " " + PROGNAME + ": " + message + Environment.NewLine;
            errmsg += "Try " + PROGNAME + " --help for usage information.";
            Console.WriteLine(errmsg);

            AnalyticsWrapper.Telemetry.TrackEvent(EventType.ReturnCode, string.Format("{0} : {1}", code.ToString(), message), LogType.Genration);
            return (int)code;
        }
        static int exit(Dictionary<ReturnCode, string> errors)
        {
            string errmsg = Environment.NewLine;
            foreach (var error in errors)
            {
                errmsg += "Code: " + (int)error.Key + " " + PROGNAME + ": " + error.Value + Environment.NewLine;
                AnalyticsWrapper.Telemetry.TrackEvent(EventType.ReturnCode, string.Format("{0} : {1}", error.Key.ToString(), error.Value), LogType.Genration);
            }
            errmsg += "Try " + PROGNAME + " --help for usage information.";
            Console.WriteLine(errmsg);
            return errors.Count > 1 ? (int)ReturnCode.MultipleErrors : (int)errors.Keys.First();
        }
    }
}
