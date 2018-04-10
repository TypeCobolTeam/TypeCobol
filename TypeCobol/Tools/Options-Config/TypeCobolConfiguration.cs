﻿using System;
using System.Collections.Generic;
using System.IO;
using Castle.Core.Internal;
using Mono.Options;
using TypeCobol.Compiler;

namespace TypeCobol.Tools.Options_Config
{
    /// <summary>
    /// TypeCobolConfiguration class holds all the argument information like input files, output files, error file etc.
    /// </summary>
    public class TypeCobolConfiguration
    {
        public string CommandLine { get; set; }
        public DocumentFormat Format = DocumentFormat.RDZReferenceFormat;
        public bool AutoRemarks;
        public string HaltOnMissingCopyFilePath;
        public string ExpandingCopyFilePath;
        public string ExtractedCopiesFilePath;
        public List<string> CopyFolders = new List<string>();
        public List<string> InputFiles = new List<string>();
        public List<string> OutputFiles = new List<string>();
        public ExecutionStep ExecToStep = ExecutionStep.Generate; //Default value is Generate
        public string ErrorFile = null;
        public string skeletonPath = "";
        public bool IsErrorXML
        {
            get { return ErrorFile != null && ErrorFile.ToLower().EndsWith(".xml"); }
        }
        public List<string> Copies = new List<string>();
        public List<string> Dependencies = new List<string>();
        public string EncFormat = null;
        public bool Telemetry;
        public int MaximumDiagnostics;
        public OutputFormat OutputFormat = OutputFormat.Cobol85;


        // Raw values (it have to be verified)
        public string RawFormat = "rdz";
        public string RawExecToStep = "5";
        public string RawMaximumDiagnostics;
        public string RawOutputFormat = "0";


        public static Dictionary<ReturnCode, string> ErrorMessages = new Dictionary<ReturnCode, string>()
        {
            // Warnings
            { ReturnCode.Warning,                "test"},
            // Errors   
            { ReturnCode.ParsingDiagnostics,     "Syntax or semantic error in one or more input file."},
            { ReturnCode.OutputFileError,        "The number of output files must be equal to the number of input files."},
            { ReturnCode.MissingCopy,            "Use of option --haltonmissingcopy and at least one COPY is missing."},
            { ReturnCode.GenerationError,        "Error during Code generatio."},
            { ReturnCode.FatalError,             "Not managed error."},
            { ReturnCode.UnexpectedParamError,   "Unexpected parameter given."},
            // Missing Parameters
            { ReturnCode.InputFileMissing,       "Input file(s) are required." },
            { ReturnCode.OutputFileMissing,      "Output are required in execution to generate step." },
            { ReturnCode.SkeletonMissing,        "Skeleton file is required in execution to generate step." }, 
            // Wrong parameter
            { ReturnCode.InputFileError,         "Input files given are unreachable." },
            { ReturnCode.OutputFileErrorBis,     "Output files given are unreachable." },
            { ReturnCode.ErrorFileError,         "Error diagnostics file is unreachable." },
            { ReturnCode.SkeletonFileError,      "Skeleton file given is unreachable." },
            { ReturnCode.HaltOnMissingCopyError, "Missing copy file given are unreachable." },
            { ReturnCode.ExecToStepError,        "Unexpected parameter given for ExecToStep. Accepted parameters are \"Scanner\"/0, \"Preprocessor\"/1, \"SyntaxCheck\"/2, \"SemanticCheck\"/3, \"CrossCheck\"/4, \"Generate\"/5(default)." },
            { ReturnCode.EncodingError,          "Unexpected parameter given for encoding option. Accepted parameters are \"rdz\"(default), \"zos\", \"utf8\"." },
            { ReturnCode.IntrinsicError,         "Intrinsic files given are unreachable." },
            { ReturnCode.CopiesError,            "Copies files given are unreachable." },
            { ReturnCode.DependenciesError,      "Dependencies files given are unreachable." },
            { ReturnCode.MaxDiagnosticsError,    "Maximum diagnostics have to be an integer." },
            { ReturnCode.OutputFormatError,      "Unexpected parameter given for Output format option. Accepted parameters are Cobol85/0(default), PublicSignature/1." },
            { ReturnCode.ExpandingCopyError,     "Expanding copy file given are unreachable." },
            { ReturnCode.ExtractusedCopyError,   "Extractused copy file given are unreachable." },
            
        };
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
        Warning = 1,                    // Warning(s) issued during parsing of input file


        //Errors
        ParsingDiagnostics = 1000,      // Syntax or semantic error in one or more input file
        OutputFileError = 1001,         // CLI parameters error
        MissingCopy = 1002,             // Use of option --haltonmissingcopy and at least one COPY is missing
        GenerationError = 1003,         // Error during Code generation
        FatalError = 1004,              // Not managed exception
        UnexpectedParamError = 1005,     // Unexpected token given

        // Missing parameter           
        InputFileMissing = 1010,        // Missing input file parameter
        OutputFileMissing = 1011,       // Missing output files and ExecToStep set to "Generate"
        SkeletonMissing = 1012,         // Missing skeleton file and ExecToStep set to "Generate"

        // Wrong parameter
        InputFileError = 1020,          // Wrong input file(s) given
        OutputFileErrorBis = 1021,      // Wrong output file(s) given
        ErrorFileError = 1022,          // Wrong error file given
        SkeletonFileError = 1023,       // Wrong skeleton file given
        HaltOnMissingCopyError = 1024,  // Wrong MissingCopy file
        ExecToStepError = 1025,         // Unexpected user input for exectostep option
        EncodingError = 1026,           // Unexpected user input for encoding option
        IntrinsicError = 1027,          // Wrong intrinsic file(s) given
        CopiesError = 1028,             // Wrong copies folder(s) given
        DependenciesError = 1029,       // Wrong dependencies folder given
        MaxDiagnosticsError = 1030,     // Unexpected user input for maximundiagnostics option (not an int)
        OutputFormatError = 1031,       // Unexpected user input for outputFormat option
        ExpandingCopyError = 1032,      // Wrong ExpandingCopy file given
        ExtractusedCopyError = 1033,    // Wrong ExtractusedCopy file given

        MultipleErrors = 9999

    }

    public enum OutputFormat {
        Cobol85,
        PublicSignatures,
        ExpandingCopy,
    }
    public static class TypeCobolOptionSet
    {
        public static OptionSet GetCommonTypeCobolOptions(TypeCobolConfiguration typeCobolConfig)
        {
            var commonOptions = new OptionSet() {
                { "i|input=", "{PATH} to an input file to parse. This option can be specified more than once.", v => typeCobolConfig.InputFiles.Add(v) },
                { "o|output=","{PATH} to an output file where to generate code. This option can be specified more than once.", v => typeCobolConfig.OutputFiles.Add(v) },
                { "d|diagnostics=", "{PATH} to the error diagnostics file.", v => typeCobolConfig.ErrorFile = v },
                { "s|skeletons=", "{PATH} to the skeletons file.", v => typeCobolConfig.skeletonPath = v },
                { "a|autoremarks", "Enable automatic remarks creation while parsing and generating Cobol", v => typeCobolConfig.AutoRemarks = true },
                { "hc|haltonmissingcopy=", "HaltOnMissingCopy will generate a file to list all the absent copies", v => typeCobolConfig.HaltOnMissingCopyFilePath = v },
                { "ets|exectostep=", "ExecToStep will execute TypeCobol Compiler until the included given step (Scanner/0, Preprocessor/1, SyntaxCheck/2, SemanticCheck/3, CrossCheck/4, Generate/5)", v => typeCobolConfig.RawExecToStep = v},
                { "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\"(this is the default), \"zos\", or \"utf8\". "+"If this option is not present, the parser will attempt to guess the {ENCODING} automatically.",
                    v => typeCobolConfig.RawFormat = v},
                { "y|intrinsic=", "{PATH} to intrinsic definitions to load.\nThis option can be specified more than once.", v => typeCobolConfig.Copies.Add(v) },
                { "c|copies=",  "Folder where COBOL copies can be found.\nThis option can be specified more than once.", v => typeCobolConfig.CopyFolders.Add(v) },
                { "dp|dependencies=", "Path to folder containing programs to load and to use for parsing a generating the input program.", v => typeCobolConfig.Dependencies.Add(v) },
                { "t|telemetry", "If set to true telemetry will send automatic email in case of bug and it will provide to TypeCobol Team data on your usage.", v => typeCobolConfig.Telemetry = true },
                { "md|maximumdiagnostics=", "Wait for an int value that will represent the maximum number of diagnostics that TypeCobol have to return.", v =>  typeCobolConfig.RawMaximumDiagnostics = v},
                { "f|outputFormat=", "Output format (default is Cobol 85). (Cobol85/0, PublicSignature/1)", v =>typeCobolConfig.RawOutputFormat = v},
                { "ec|expandingcopy=", "Generate a file with all COPY directives expanded in the source code. This option will be executed if the Preprocessor step is enabled.", v => typeCobolConfig.ExpandingCopyFilePath = v },
                { "exc|extractusedcopy=", "Generate a file with all COPIES detected by the parser", v => typeCobolConfig.ExtractedCopiesFilePath = v },

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
            switch (config.RawExecToStep.ToLower())
            {
                case "0":
                case "scanner":
                    config.ExecToStep = ExecutionStep.Scanner;
                    break;
                case "1":
                case "preprocessor":
                    config.ExecToStep = ExecutionStep.Preprocessor;
                    break;
                case "2":
                case "syntaxcheck":
                    config.ExecToStep = ExecutionStep.SyntaxCheck;
                    break;
                case "3":
                case "semanticcheck":
                    config.ExecToStep = ExecutionStep.SemanticCheck;
                    break;
                case "4":
                case "crosscheck":
                    config.ExecToStep = ExecutionStep.CrossCheck;
                    break;
                case "5":
                case "generate":
                    config.ExecToStep = ExecutionStep.Generate;
                    break;
                default:
                    errorStack.Add(ReturnCode.ExecToStepError, TypeCobolConfiguration.ErrorMessages[ReturnCode.ExecToStepError]);
                    break;
            }

            //// Check required options
            //InputFileMissing
            if (config.InputFiles.IsNullOrEmpty())
                errorStack.Add(ReturnCode.InputFileMissing, TypeCobolConfiguration.ErrorMessages[ReturnCode.InputFileMissing]);
            //OutputFileMissing/OutputFileError
            if (config.ExecToStep == ExecutionStep.Generate && !errorStack.ContainsKey(ReturnCode.ExecToStepError))
            {
                if (config.OutputFiles.IsNullOrEmpty())
                    errorStack.Add(ReturnCode.OutputFileMissing, TypeCobolConfiguration.ErrorMessages[ReturnCode.OutputFileMissing]);
                else if (config.InputFiles.Count != config.OutputFiles.Count)
                    errorStack.Add(ReturnCode.OutputFileError, TypeCobolConfiguration.ErrorMessages[ReturnCode.OutputFileError]);
            }
            //SkeletonMissing
            if (config.ExecToStep == ExecutionStep.Generate && config.skeletonPath.IsNullOrEmpty() && !errorStack.ContainsKey(ReturnCode.ExecToStepError))
                errorStack.Add(ReturnCode.SkeletonMissing, TypeCobolConfiguration.ErrorMessages[ReturnCode.SkeletonMissing]);


            //// Check unexpected token
            if (unexpectedStrings.Count != 0)
                errorStack.Add(ReturnCode.UnexpectedParamError, TypeCobolConfiguration.ErrorMessages[ReturnCode.UnexpectedParamError]);

            //// Options values verification
            //InputFileError
            foreach (var path in config.InputFiles)
            {
                if (FileSystem.GetFiles(path, recursive: false).IsNullOrEmpty() && !errorStack.ContainsKey(ReturnCode.InputFileError))
                {
                    errorStack.Add(ReturnCode.InputFileError, TypeCobolConfiguration.ErrorMessages[ReturnCode.InputFileError]);
                }
            }
            //SkeletonFileError
            if (config.ExecToStep == ExecutionStep.Generate && !config.skeletonPath.IsNullOrEmpty() &&  !errorStack.ContainsKey(ReturnCode.ExecToStepError))
            {
                if (!File.Exists(config.skeletonPath))
                {
                    errorStack.Add(ReturnCode.SkeletonFileError, TypeCobolConfiguration.ErrorMessages[ReturnCode.SkeletonFileError]);
                }
            }
            // EncodingError
            switch (config.RawFormat.ToLower())
            {
                case "rdz":
                    config.EncFormat = "rdz";
                    config.Format = DocumentFormat.RDZReferenceFormat;
                    break;
                case "zos":
                    config.EncFormat = "zos";
                    config.Format = DocumentFormat.ZOsReferenceFormat;
                    break;
                case "utf8":
                    config.EncFormat = "utf8";
                    config.Format = DocumentFormat.FreeUTF8Format;
                    break;
                default:
                    errorStack.Add(ReturnCode.EncodingError, TypeCobolConfiguration.ErrorMessages[ReturnCode.EncodingError]);
                    break;
            }
            //IntrinsicError
            foreach (var path in config.Copies)
            {
                if (FileSystem.GetFiles(path, recursive: false).IsNullOrEmpty() && !errorStack.ContainsKey(ReturnCode.IntrinsicError))
                {
                    errorStack.Add(ReturnCode.IntrinsicError, TypeCobolConfiguration.ErrorMessages[ReturnCode.IntrinsicError]);
                }
            }
            //CopiesError
            foreach (var path in config.CopyFolders)
            {
                if (!Directory.Exists(path) && !errorStack.ContainsKey(ReturnCode.CopiesError))
                {
                    errorStack.Add(ReturnCode.CopiesError, TypeCobolConfiguration.ErrorMessages[ReturnCode.CopiesError]);
                }
            }
            //DependenciesError
            foreach (var path in config.Dependencies)
            {
                if (FileSystem.GetFiles(path, recursive: false).IsNullOrEmpty() && !errorStack.ContainsKey(ReturnCode.DependenciesError))
                {
                    errorStack.Add(ReturnCode.DependenciesError, TypeCobolConfiguration.ErrorMessages[ReturnCode.DependenciesError]);
                }
            }
            // MaxDiagnosticsError
            if (!config.RawMaximumDiagnostics.IsNullOrEmpty())
            {
                if (!int.TryParse(config.RawMaximumDiagnostics, out config.MaximumDiagnostics))
                    errorStack.Add(ReturnCode.MaxDiagnosticsError, TypeCobolConfiguration.ErrorMessages[ReturnCode.MaxDiagnosticsError]);
            }

            // OutputFormatError
            switch (config.RawOutputFormat.ToLower())
            {
                case "0":
                case "cobol85":
                    config.OutputFormat = OutputFormat.Cobol85;
                    break;
                case "1":
                case "publicsignature":
                case "publicsignatures":
                    config.OutputFormat = OutputFormat.PublicSignatures;
                    break;
                default:
                    errorStack.Add(ReturnCode.OutputFormatError, TypeCobolConfiguration.ErrorMessages[ReturnCode.OutputFormatError]);
                    break;
            }

            return errorStack;
        }

        /// <summary>
        /// CreateFormat method to get the format name.
        /// </summary>
        /// <param name="encoding">string</param>
        /// <param name="config">Config</param>
        /// <returns>DocumentFormat</returns>
        public static Compiler.DocumentFormat CreateFormat(string encoding, ref TypeCobolConfiguration config)
        {
            config.EncFormat = encoding;

            if (encoding == null) return null;
            if (encoding.ToLower().Equals("zos")) return TypeCobol.Compiler.DocumentFormat.ZOsReferenceFormat;
            if (encoding.ToLower().Equals("utf8")) return TypeCobol.Compiler.DocumentFormat.FreeUTF8Format;
            /*if (encoding.ToLower().Equals("rdz"))*/
            return TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
        }
    }
}
