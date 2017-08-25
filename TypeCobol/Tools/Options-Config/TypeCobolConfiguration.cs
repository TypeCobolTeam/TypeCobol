using System;
using System.Collections.Generic;
using Mono.Options;

namespace TypeCobol.Tools.Options_Config
{
    /// <summary>
    /// TypeCobolConfiguration class holds all the argument information like input files, output files, error file etc.
    /// </summary>
    public class TypeCobolConfiguration
    {
        public string CommandLine { get; set; }
        public Compiler.DocumentFormat Format = Compiler.DocumentFormat.RDZReferenceFormat;
        public bool AutoRemarks;
        public string HaltOnMissingCopyFilePath;
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
    }

    public enum OutputFormat {
        Cobol85,
        PublicSignatures
    }
    public static class TypeCobolOptionSet
    {
        public static OptionSet GetCommonTypeCobolOptions(TypeCobolConfiguration typeCobolConfig)
        {
            var commonOptions = new OptionSet() {
                { "i|input=", "{PATH} to an input file to parse. This option can be specified more than once.", v => typeCobolConfig.InputFiles.Add(v) },
                { "o|output=","{PATH} to an ouput file where to generate code. This option can be specified more than once.", v => typeCobolConfig.OutputFiles.Add(v) },
                { "d|diagnostics=", "{PATH} to the error diagnostics file.", v => typeCobolConfig.ErrorFile = v },
                { "s|skeletons=", "{PATH} to the skeletons files.", v => typeCobolConfig.skeletonPath = v },
                { "a|autoremarks", "Enable automatic remarks creation while parsing and generating Cobol", v => typeCobolConfig.AutoRemarks = true },
                { "hc|haltonmissingcopy=", "HaltOnMissingCopy will generate a file to list all the absent copies", v => typeCobolConfig.HaltOnMissingCopyFilePath = v },
                { "ets|exectostep=", "ExecToStep will execute TypeCobol Compiler until the included given step (Scanner/0, Preprocessor/1, SyntaxCheck/2, SemanticCheck/3, Generate/4)", v => Enum.TryParse(v.ToString(), true, out typeCobolConfig.ExecToStep)},
				{ "e|encoding=", "{ENCODING} of the file(s) to parse. It can be one of \"rdz\"(this is the default), \"zos\", or \"utf8\". "
                                +"If this option is not present, the parser will attempt to guess the {ENCODING} automatically.",
                                v => typeCobolConfig.Format = CreateFormat(v, ref typeCobolConfig)
                },
                { "y|intrinsic=", "{PATH} to intrinsic definitions to load.\nThis option can be specified more than once.", v => typeCobolConfig.Copies.Add(v) },
                { "c|copies=",  "Folder where COBOL copies can be found.\nThis option can be specified more than once.", v => typeCobolConfig.CopyFolders.Add(v) },
                { "dp|dependencies=", "Path to folder containing programs to load and to use for parsing a generating the input program.", v => typeCobolConfig.Dependencies.Add(v) },
                { "t|telemetry=", "If set to true telemrty will send automatic email in case of bug and it will provide to TypeCobol Team data on your usage.", v => typeCobolConfig.Telemetry = true },
                { "md|maximumdiagnostics=", "Wait for an int value that will represent the maximum number of diagnostics that TypeCobol have to return.", v =>  typeCobolConfig.MaximumDiagnostics = int.Parse(v.ToString())},
                { "f|outputFormat=", "Output format (default is Cobol 85). (Cobol85/0, PublicSignature/1)", v => Enum.TryParse(v.ToString(), true, out typeCobolConfig.OutputFormat)}
            };

            return commonOptions;
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
