using System.Text;
using Mono.Options;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
#if EUROINFO_RULES
using TypeCobol.Compiler.Preprocessor;
#endif

namespace TypeCobol.Tools.Options_Config
{
    /// <summary>
    /// TypeCobolConfiguration class holds all the argument information like input files, output files, error file etc.
    /// </summary>
    public class TypeCobolConfiguration : ITypeCobolCheckOptions
    {
        public DocumentFormat Format;
        public string HaltOnMissingCopyFilePath;
        public string ExpandingCopyFilePath;
        public string ExtractedCopiesFilePath;
        public bool UseAntlrProgramParsing;
        public string ReportCopyMoveInitializeFilePath;
        public string ReportZCallFilePath;
        public List<string> CopyFolders = new List<string>();
        public List<string> InputFiles = new List<string>();
        public List<string> OutputFiles = new List<string>();
        public List<string> LineMapFiles = new List<string>();
        public ExecutionStep ExecToStep = ExecutionStep.Generate; //Default value is Generate
        public string ErrorFile;
        public string LogFile;

        //Log file name
        public const string DefaultLogFileName = "TypeCobol.CLI.log";

#if EUROINFO_RULES
        public bool EILegacy_RemoveFirst01Level = true;
        public bool EILegacy_ApplyCopySuffixing = true;
        public string ReportUsedCopyNamesPath;
#else
        public bool EILegacy_RemoveFirst01Level = false;
        public bool EILegacy_ApplyCopySuffixing = false;
#endif
        // Checks
        public TypeCobolCheckOption CheckEndAlignment { get; set; }
        public TypeCobolCheckOption CheckPerformPrematureExits { get; set; }
        public TypeCobolCheckOption CheckPerformThruOrder { get; set; }
        public TypeCobolCheckOption CheckRecursivePerforms { get; set; }
        public TypeCobolCheckOption CheckCodeElementMixedDebugType { get; set; }

        public List<string> Copies = new List<string>();
        public List<string> Dependencies = new List<string>();
        public int MaximumDiagnostics;
        public OutputFormat OutputFormat = OutputFormat.Cobol85;
        public CfgBuildingMode CfgBuildingMode = CfgBuildingMode.None;
        public List<string> Extensions = new List<string>();

        // Raw values (it has to be verified)
        public string RawFormat;
        public string RawInputCodepageOrEncoding;
        public string RawExecToStep = "6";
        public string RawMaximumDiagnostics;
        public string RawOutputFormat = "0";
        public string RawCfgBuildingMode = "0";
        public bool IsCobolLanguage;

#if EUROINFO_RULES
        /// <summary>
        /// Default location for CPY copy name map. Expects a file 'COPIES_CPY.txt' next to the current executable.
        /// </summary>
        private static readonly string _DefaultCpyCopyNameMapFilePath = Path.Combine(Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location), "COPIES_CPY.txt");
        
        /// <summary>
        /// The CPY copy name map loaded in memory.
        /// </summary>
        public CopyNameMapFile CpyCopyNameMap;

        /// <summary>
        /// Attempts loading of the CPY copy name map. If no path is given, default location is used.
        /// </summary>
        /// <param name="cpyCopyNameMapFilePath">Relative or absolute path to a CPY copy name map file on disk</param>
        /// <remarks>Must be called after <see cref="TypeCobolOptionSet.InitializeCobolOptions"/>, <see cref="LogFile"/> must be writable</remarks>
        public void LoadCpyCopyNameMap(string cpyCopyNameMapFilePath)
        {
            var path = string.IsNullOrWhiteSpace(cpyCopyNameMapFilePath) ? _DefaultCpyCopyNameMapFilePath : cpyCopyNameMapFilePath;
            if (File.Exists(path))
            {
                CpyCopyNameMap = new CopyNameMapFile(path);
            }
            else
            {
                File.AppendAllText(LogFile ?? DefaultLogFileName, $"CPY Copy name map file '{path}' is unreachable.");
            }
        }
#endif

        public static Dictionary<ReturnCode, string> ErrorMessages = new Dictionary<ReturnCode, string>()
        {
            // Warnings
            { ReturnCode.Warning,                 ""},
            // Errors   
            { ReturnCode.ParsingDiagnostics,      "Syntax or semantic error in one or more input file."},
            { ReturnCode.OutputFileError,         "The number of output files must be equal to the number of input files."},
            { ReturnCode.MissingCopy,             "Use of option --haltonmissingcopy and at least one COPY is missing."},
            { ReturnCode.GenerationError,         "Error during Code generation."},
            { ReturnCode.FatalError,              "Unhandled error occurs."},
            { ReturnCode.UnexpectedParamError,    "Unexpected parameter given."},
            // Missing Parameters
            { ReturnCode.InputFileMissing,        "Input file(s) are required." },
            { ReturnCode.OutputFileMissing,       "Output are required in execution to generate step." },
            // Wrong parameter
            { ReturnCode.InputFileError,           "Input files given are unreachable." },
            { ReturnCode.OutputPathError,          "Output paths given are unreachable." },
            { ReturnCode.ErrorFileError,           "Error diagnostics path is unreachable." },
            { ReturnCode.HaltOnMissingCopyError,   "Missing copy path given is unreachable." },
            { ReturnCode.ExecToStepError,          "Unexpected parameter given for ExecToStep. Accepted parameters are \"Scanner\"/0, \"Preprocessor\"/1, \"CodeElement\"/2, \"AST\"/3, \"SemanticCrossCheck\"/4, \"Generate\"/5(default)." },
            { ReturnCode.EncodingError,            "Unexpected parameter given for encoding option. Accepted parameters are \"rdz\"(default), \"zos\", \"utf8\"." },
            { ReturnCode.IntrinsicError,           "Intrinsic files given are unreachable." },
            { ReturnCode.CopiesError,              "Copies files given are unreachable." },
            { ReturnCode.DependenciesError,        "Dependencies files given are unreachable: " },
            { ReturnCode.MaxDiagnosticsError,      "Maximum diagnostics have to be an integer." },
            { ReturnCode.OutputFormatError,        "Unexpected parameter given for Output format option. Accepted parameters are Cobol85/0(default), PublicSignature/1." },
            { ReturnCode.ExpandingCopyError,       "Expanding copy path given is unreachable." },
            { ReturnCode.ExtractUsedCopyError,     "Extract used copy path given is unreachable." },
            { ReturnCode.LogFileError,             "Log file path is unreachable." },
            { ReturnCode.ParserExtensionFileError, "Custom extension assembly files are unreachable." },
            { ReturnCode.CfgOptionError,           "CFG building mode is not supported. Accepted values are None/0, Standard/1, Extended/2, WithDfa/3." }
        };

        public TypeCobolConfiguration()
        {
            // default values for checks
            CheckEndAlignment = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckEndAlignmentSeverity);
            CheckPerformPrematureExits = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckPerformPrematureExitsSeverity);
            CheckPerformThruOrder = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckPerformThruOrderSeverity);
            CheckRecursivePerforms = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckRecursivePerformsSeverity);
            CheckCodeElementMixedDebugType = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckCodeElementMixedDebugTypeSeverity);
        }
    }

    /// <summary>
    /// Categories of ReturnCode.
    /// ReturnCode ranges from 0 to 255 as follows:
    /// * 000 : Everything is ok
    ///         Output files are generated
    /// * 001 to 099 : Ok but there are warnings.
    ///         Output files are generated
    ///         Diagnostic file should contain warnings
    /// * >= 100 : Errors
    ///         Output files are NOT generated
    ///         Diagnostic file should contain errors and warnings
    /// * 255 : Multiple errors, check logs and/or console messages
    /// </summary>
    public enum ReturnCode
    {
        Success = 0,

        //Warnings
        Warning = 1,                    // Warning(s) issued during parsing of input file

        //Errors
        ParsingDiagnostics = 100,      // Syntax or semantic error in one or more input file
        OutputFileError = 101,         // CLI parameters error
        MissingCopy = 102,             // Use of option --haltonmissingcopy and at least one COPY is missing
        GenerationError = 103,         // Error during Code generation
        FatalError = 104,              // Not managed exception
        UnexpectedParamError = 105,     // Unexpected token given

        // Missing parameter           
        InputFileMissing = 110,        // Missing input file parameter
        OutputFileMissing = 111,       // Missing output files and ExecToStep set to "Generate"

        // Wrong parameter
        InputFileError = 120,           // Wrong input file(s) given
        OutputPathError = 121,          // Output paths given are unreachable.
        ErrorFileError = 122,           // Wrong error path given
        HaltOnMissingCopyError = 124,   // Missing copy path given is unreachable.
        ExecToStepError = 125,          // Unexpected user input for exectostep option
        EncodingError = 126,            // Unexpected user input for encoding option
        IntrinsicError = 127,           // Wrong intrinsic file(s) given
        CopiesError = 128,              // Wrong copies folder(s) given
        DependenciesError = 129,        // Wrong dependencies folder given
        MaxDiagnosticsError = 130,      // Unexpected user input for maximumdiagnostics option (not an int)
        OutputFormatError = 131,        // Unexpected user input for outputFormat option
        ExpandingCopyError = 132,       // Expanding copy path given is unreachable.
        ExtractUsedCopyError = 133,     // Extract used copy path given is unreachable.
        LogFileError = 134,             // Wrong log path given
        ParserExtensionFileError = 135, // Invalid path to a custom extension DLL file
        CfgOptionError = 136,           // Invalid CFG building mode

        MultipleErrors = 255
    }

    public enum OutputFormat {
        Cobol85,
        PublicSignatures,
        ExpandingCopy,
        Cobol85Mixed,
        Cobol85Nested,
        Documentation
    }

    public class TypeCobolCheckOption
    {
        public static TypeCobolCheckOption Parse(string argument)
        {
            if (Enum.TryParse(argument, true, out Severity diagnosticLevel))
            {
                return new TypeCobolCheckOption(diagnosticLevel);
            }

            if (string.Equals(argument, "ignore", StringComparison.OrdinalIgnoreCase))
            {
                return new TypeCobolCheckOption(null);
            }

            throw new ArgumentException();
        }

        private readonly Severity? _diagnosticLevel;

        public TypeCobolCheckOption(Severity? diagnosticLevel)
        {
            _diagnosticLevel = diagnosticLevel;
        }

        public bool IsActive => _diagnosticLevel.HasValue;

        public MessageCode GetMessageCode()
        {
            switch (_diagnosticLevel)
            {
                case Severity.Error:
                    return MessageCode.SyntaxErrorInParser;
                case Severity.Info:
                    return MessageCode.Info;
                case Severity.Warning:
                    return MessageCode.Warning;
                default:
                    // invalid Severity or not set
                    throw new InvalidOperationException("The considered check is not active!");
            }
        }
    }

    public interface ITypeCobolCheckOptions
    {
        const Severity DefaultCheckEndAlignmentSeverity = Severity.Warning;
        const Severity DefaultCheckPerformPrematureExitsSeverity = Severity.Warning;
        const Severity DefaultCheckPerformThruOrderSeverity = Severity.Warning;
        const Severity DefaultCheckRecursivePerformsSeverity = Severity.Warning;
        const Severity DefaultCheckCodeElementMixedDebugTypeSeverity = Severity.Warning;

        TypeCobolCheckOption CheckEndAlignment { get; set; }
        TypeCobolCheckOption CheckPerformPrematureExits { get; set; }
        TypeCobolCheckOption CheckPerformThruOrder { get; set; }
        TypeCobolCheckOption CheckRecursivePerforms { get; set; }
        TypeCobolCheckOption CheckCodeElementMixedDebugType { get; set; }
    }

    public static class TypeCobolOptionSet
    {
        public static OptionSet GetCommonTypeCobolOptions(TypeCobolConfiguration typeCobolConfig)
        {
            var commonOptions = new OptionSet() {
                { "i|input=", "{PATH} to an input file to parse. This option can be specified more than once.", v => typeCobolConfig.InputFiles.Add(v) },
                { "o|output=","{PATH} to an output file where to generate code. This option can be specified more than once.", v => typeCobolConfig.OutputFiles.Add(v) },
                { "d|diagnostics=", "{PATH} to the error diagnostics file.", v => typeCobolConfig.ErrorFile = v },
                { "s|skeletons=", "{PATH} to the skeletons file. DEPRECATED : generation using dynamic skeleton is no longer supported.", v => {}},
                { "a|autoremarks", "Enable automatic remarks creation while parsing and generating Cobol. DEPRECATED, this option is always activated.", v => {}},
                { "hc|haltonmissingcopy=", "HaltOnMissingCopy will generate a file to list all the absent copies.", v => typeCobolConfig.HaltOnMissingCopyFilePath = v },
                { "ets|exectostep=", "ExecToStep will execute TypeCobol Compiler until the included given step (Scanner/0, Preprocessor/1, CodeElement/2, AST/3, SemanticCrossCheck/4, CodeAnalysis/5, Generate/6).", v => typeCobolConfig.RawExecToStep = v},
                { "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\"(this is the default), \"zos\", or \"utf8\". "+"If this option is not present, the parser will attempt to guess the {ENCODING} automatically.",
                    v => typeCobolConfig.RawFormat = v},
                { "ie|inputencoding=", "Allows to change default encoding when used with RDZ format. Specify a valid encoding name, example : -ie \"Windows-1252\".", v => typeCobolConfig.RawInputCodepageOrEncoding = v },
                { "y|intrinsic=", "{PATH} to intrinsic definitions to load.\nThis option can be specified more than once.", v => typeCobolConfig.Copies.Add(v) },
                { "c|copies=",  "Folder where COBOL copies can be found.\nThis option can be specified more than once.", v => typeCobolConfig.CopyFolders.Add(v) },
                { "dp|dependencies=", "Path to folder containing programs to load and to use for parsing a generating the input program.", v => typeCobolConfig.Dependencies.Add(v) },
                { "t|telemetry", "If set to true telemetry will send automatic email in case of bug and it will provide to TypeCobol Team data on your usage. DEPRECATED, mail sending capability has been removed.", v => {}},
                { "md|maximumdiagnostics=", "Wait for an int value that will represent the maximum number of diagnostics that TypeCobol have to return.", v =>  typeCobolConfig.RawMaximumDiagnostics = v},
                { "f|outputFormat=", "Output format (default is Cobol 85). (Cobol85/0, PublicSignature/1, Cobol85Mixed/3, Cobol85Nested/4, Documentation/5).", v =>typeCobolConfig.RawOutputFormat = v},
                { "ec|expandingcopy=", "Generate a file with all COPY directives expanded in the source code. This option will be executed if the Preprocessor step is enabled.", v => typeCobolConfig.ExpandingCopyFilePath = v },
                { "exc|extractusedcopy=", "Generate a file with all COPIES detected by the parser.", v => typeCobolConfig.ExtractedCopiesFilePath = v },
                { "alr|antlrprogparse", "Use ANTLR to parse a program.", v => typeCobolConfig.UseAntlrProgramParsing = true},
                { "cmr|copymovereport=", "{PATH} to Report all Move and Initialize statements that target a COPY.", v => typeCobolConfig.ReportCopyMoveInitializeFilePath = v },
                { "zcr|zcallreport=", "{PATH} to report of all program called by zcallpgm.", v => typeCobolConfig.ReportZCallFilePath = v },
                { "dcs|disablecopysuffixing", "OBSOLETE - Use 'drfl' and 'dcsm' options instead.'.", v => { typeCobolConfig.EILegacy_RemoveFirst01Level = false; typeCobolConfig.EILegacy_ApplyCopySuffixing = false; } },
                { "drfl|disableremovefirst01level", "Disable EI Legacy automatic removal of first 01 level from CPY copies.", v => typeCobolConfig.EILegacy_RemoveFirst01Level = false },
                { "dcsm|disablecopysuffixingmechanism", "Disable EI Legacy automatic suffixing of data names from CPY copies.", v => typeCobolConfig.EILegacy_ApplyCopySuffixing = false },
                { "glm|genlinemap=", "{PATH} to an output file where line mapping will be generated.", v => typeCobolConfig.LineMapFiles.Add(v) },
                { "diag.cea|diagnostic.checkEndAlignment=", "Indicate level of check end aligment: warning, error, info, ignore.", v => typeCobolConfig.CheckEndAlignment = TypeCobolCheckOption.Parse(v) },
                { "diag.cppe|diagnostic.checkPerformPrematureExits=", "Indicate level of check perform premature exits: warning, error, info, ignore.", v => typeCobolConfig.CheckPerformPrematureExits = TypeCobolCheckOption.Parse(v) },
                { "diag.cpto|diagnostic.checkPerformThruOrder=", "Indicate level of check perform thru procedure order: warning, error, info, ignore.", v => typeCobolConfig.CheckPerformThruOrder = TypeCobolCheckOption.Parse(v) },
                { "diag.crp|diagnostic.checkRecursivePerforms=", "Indicate level of check recursive performs: warning, error, info, ignore.", v => typeCobolConfig.CheckRecursivePerforms = TypeCobolCheckOption.Parse(v) },
                { "diag.ccemdt|diagnostic.checkCodeElementMixedDebugType=", "Indicate level of check for code elements mixing debug and non-debug lines: warning, error, info, ignore.", v => typeCobolConfig.CheckCodeElementMixedDebugType = TypeCobolCheckOption.Parse(v) },
                { "log|logfilepath=", "{PATH} to TypeCobol.CLI.log log file", v => typeCobolConfig.LogFile = Path.Combine(v, TypeCobolConfiguration.DefaultLogFileName)},
                { "cfg|cfgbuild=", "CFG build option, recognized values are: None/0, Standard/1, Extended/2, WithDfa/3.", v => typeCobolConfig.RawCfgBuildingMode = v },
                { "cob|cobol", "Indicate that it's a pure Cobol85 input file.", v => typeCobolConfig.IsCobolLanguage = true },
#if EUROINFO_RULES
                { "cpyr|cpyreport=", "{PATH} to report of all COPY names used by a program.", v => typeCobolConfig.ReportUsedCopyNamesPath = v }
#endif
            };
            return commonOptions;
        }

        public static Dictionary<ReturnCode, string> InitializeCobolOptions(TypeCobolConfiguration config, IEnumerable<string> args, OptionSet options)
        {
            Dictionary<ReturnCode, string> errorStack = new Dictionary<ReturnCode, string>();
            List<string> unexpectedStrings = new List<string>();
            
            try
            {
                unexpectedStrings = options.Parse(args);
            }
            catch (OptionException ex)
            {
                // Last parameter doesn't have any value
                errorStack.Add(ReturnCode.FatalError, ex.Message);
            }

            // ExecToStepError
            if (!Enum.TryParse(config.RawExecToStep, true, out config.ExecToStep))
                errorStack.Add(ReturnCode.ExecToStepError, TypeCobolConfiguration.ErrorMessages[ReturnCode.ExecToStepError]);

            //// Check required options
            int? inputFilesCount = config.InputFiles?.Count;
            int? outputFilesCount = config.OutputFiles?.Count;
            //InputFileMissing
            if (inputFilesCount == null || inputFilesCount == 0)
            {
                inputFilesCount = 0;
                errorStack.Add(ReturnCode.InputFileMissing, TypeCobolConfiguration.ErrorMessages[ReturnCode.InputFileMissing]);
            }
            if (config.ExecToStep == ExecutionStep.Generate && !errorStack.ContainsKey(ReturnCode.ExecToStepError))
            {
                //OutputFileMissing
                if (outputFilesCount == null || outputFilesCount == 0)
                {
                    outputFilesCount = 0;
                    errorStack.Add(ReturnCode.OutputFileMissing, TypeCobolConfiguration.ErrorMessages[ReturnCode.OutputFileMissing]);
                }
                //OutputFileError
                if (inputFilesCount != outputFilesCount)
                {
                    errorStack.Add(ReturnCode.OutputFileError, TypeCobolConfiguration.ErrorMessages[ReturnCode.OutputFileError]);
                }
            }
            
            //// Check unexpected token
            if (unexpectedStrings.Count != 0)
                errorStack.Add(ReturnCode.UnexpectedParamError, TypeCobolConfiguration.ErrorMessages[ReturnCode.UnexpectedParamError]);

            //// Options values verification
            //InputFileError
            VerifFiles(config.InputFiles, ReturnCode.InputFileError, errorStack);

            //outputFilePathsWrong
            if (config.OutputFiles != null)
            {
                foreach (var path in config.OutputFiles)
                {
                    if (!CanCreateFile(path) && !errorStack.ContainsKey(ReturnCode.OutputPathError))
                    {
                        errorStack.Add(ReturnCode.OutputPathError, TypeCobolConfiguration.ErrorMessages[ReturnCode.OutputPathError]);
                    }
                }
            }

            //ErrorFilePathError
            if (!CanCreateFile(config.ErrorFile) && !string.IsNullOrEmpty(config.ErrorFile))
                errorStack.Add(ReturnCode.ErrorFileError, TypeCobolConfiguration.ErrorMessages[ReturnCode.ErrorFileError]);

            //HaltOnMissingCopyFilePathError
            if (!CanCreateFile(config.HaltOnMissingCopyFilePath) && !string.IsNullOrEmpty(config.HaltOnMissingCopyFilePath))
                errorStack.Add(ReturnCode.HaltOnMissingCopyError, TypeCobolConfiguration.ErrorMessages[ReturnCode.HaltOnMissingCopyError]);

            // EncodingError
            config.Format = CreateFormat(config.RawFormat, config.RawInputCodepageOrEncoding, errorStack);

            //IntrinsicError
            VerifFiles(config.Copies, ReturnCode.IntrinsicError, errorStack);

            //CopiesError
            VerifFiles(config.CopyFolders, ReturnCode.CopiesError, errorStack, true);

            ////DependencyFolderMissing
            if (config.ExecToStep == ExecutionStep.Generate && !errorStack.ContainsKey(ReturnCode.ExecToStepError))
            {
                foreach (string dependency in config.Dependencies)
                {
                    string directory = Path.GetDirectoryName(dependency);
                    string file = Path.GetFileName(dependency);

                    if (file?.Contains("*") == true)
                        file = string.Empty;

                    if ((!Directory.Exists(directory) || !string.IsNullOrEmpty(file) && !File.Exists(dependency)) && !errorStack.ContainsKey(ReturnCode.DependenciesError))
                        errorStack.Add(ReturnCode.DependenciesError, TypeCobolConfiguration.ErrorMessages[ReturnCode.DependenciesError] + dependency);

                }
            }

            // MaxDiagnosticsError
            if (!string.IsNullOrEmpty(config.RawMaximumDiagnostics))
            {
                if (!int.TryParse(config.RawMaximumDiagnostics, out config.MaximumDiagnostics))
                    errorStack.Add(ReturnCode.MaxDiagnosticsError, TypeCobolConfiguration.ErrorMessages[ReturnCode.MaxDiagnosticsError]);
            }

            // OutputFormatError
            if (!Enum.TryParse(config.RawOutputFormat, true, out config.OutputFormat))
                errorStack.Add(ReturnCode.OutputFormatError, TypeCobolConfiguration.ErrorMessages[ReturnCode.OutputFormatError]);

            //ExpandingCopyFilePathError
            if (!CanCreateFile(config.ExpandingCopyFilePath) && !string.IsNullOrEmpty(config.ExpandingCopyFilePath))
                errorStack.Add(ReturnCode.ExpandingCopyError, TypeCobolConfiguration.ErrorMessages[ReturnCode.ExpandingCopyError]);

            //HaltOnMissingCopyFilePathError
            if (!CanCreateFile(config.ExtractedCopiesFilePath) && !string.IsNullOrEmpty(config.ExtractedCopiesFilePath))
                errorStack.Add(ReturnCode.ExtractUsedCopyError, TypeCobolConfiguration.ErrorMessages[ReturnCode.ExtractUsedCopyError]);

            //LogFilePathError
            if (!CanCreateFile(config.LogFile) && !string.IsNullOrEmpty(config.LogFile))
                errorStack.Add(ReturnCode.LogFileError, TypeCobolConfiguration.ErrorMessages[ReturnCode.LogFileError]);

            //CustomAnalyzers
            VerifFiles(config.Extensions, ReturnCode.ParserExtensionFileError, errorStack);

            //CFG Building mode
            if (!Enum.TryParse(config.RawCfgBuildingMode, true, out config.CfgBuildingMode))
                errorStack.Add(ReturnCode.CfgOptionError, TypeCobolConfiguration.ErrorMessages[ReturnCode.CfgOptionError]);

            return errorStack;
        }


        public static void VerifFiles(List<string> paths, ReturnCode errorCode, Dictionary<ReturnCode, string> errorStack, bool isFolder = false)
        {
            foreach (var path in paths)
            {
                if ((isFolder ? !Directory.Exists(path) : FileSystem.GetFiles(path, recursive: false).Count == 0) && !errorStack.ContainsKey(errorCode))
                {
                    errorStack.Add(errorCode, TypeCobolConfiguration.ErrorMessages[errorCode]);
                }
            }
        }

        public static bool CanCreateFile(string path)
        {
            if (!File.Exists(path))
            {
                if (!Directory.Exists(Path.GetDirectoryName(path)))
                    return false;
            }
            return true;
        }

        /// <summary>
        /// Create a new DocumentFormat instance for input documents
        /// according to the specified format and encoding.
        /// </summary>
        /// <param name="format">Recognized format values are "zos", "utf8", "rdz" (default is "rdz").</param>
        /// <param name="inputEncoding">Recognized input encoding values are those supported by System.Text.Encoding.GetEncoding(string) method.
        /// Only applied when used with "rdz" format, ignored otherwise.</param>
        /// <param name="errorStack">Dictionary of errors accumulated so far.</param>
        /// <returns>instance of DocumentFormat.</returns>
        private static Compiler.DocumentFormat CreateFormat(string format, string inputEncoding, Dictionary<ReturnCode, string> errorStack)
        {
            string errorMessage = null;
            switch (format?.ToLower())
            {
                case "zos":
                    return DocumentFormat.ZOsReferenceFormat;
                case "utf8":
                    return DocumentFormat.FreeUTF8Format;
                case null:
                case "rdz":
                    break;
                default:
                    errorMessage = $"The format '{format}' is not supported.";
                    break;
            }

            var documentFormat = DocumentFormat.RDZReferenceFormat;
            if (inputEncoding != null)
            {
                try
                {
                    var substitutionEncoding = Encoding.GetEncoding(inputEncoding);
                    documentFormat = new DocumentFormat(substitutionEncoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength, documentFormat.ColumnsLayout);
                }
                catch
                {
                    //Could not find the desired encoding, complete the error message if any.
                    if (errorMessage != null) errorMessage += " ";
                    errorMessage += $"The input encoding '{inputEncoding}' could not be found.";
                }
            }

            if (errorMessage != null)
            {
                errorStack.Add(ReturnCode.EncodingError, errorMessage);
            }
            return documentFormat;
        }
    }
}
