using TypeCobol.Tools.Options_Config;
#if EUROINFO_RULES
using TypeCobol.Compiler.Preprocessor;
#endif

namespace TypeCobol.Compiler.Directives
{
    /// <summary>
    /// TypeCobol compiler options (superset of the IBM Enterprise Cobol compiler options)
    /// </summary>
    public class TypeCobolOptions : IBMCompilerOptions, ITypeCobolCheckOptions
    {
        // insert options specific to TypeCobol here ...
#if EUROINFO_RULES
        public bool AutoRemarksEnable { get; set; }
        /// <summary>
        /// Path to generate collected used copy names
        /// </summary>
        public string ReportUsedCopyNamesPath { get; set; }
#endif
        /// <summary>
        /// Option to create a missing copy file, it will stop the execution before semantic phase if a copy is missing
        /// </summary>
        public bool HaltOnMissingCopy { get; set; }

        /// <summary>
        /// Option used to defined the maximum processing step. (See FileCompiler for usage)
        /// </summary>
        public ExecutionStep? ExecToStep { get; set; }

        /// <summary>
        /// Shall we use Antlr for Parsing the Program.
        /// </summary>
        public bool UseAntlrProgramParsing { get; set; }

        /// <summary>
        /// Shall we use EUROINFO_LEGACY_REPLACING_SYNTAX snippets for Parsing the Program.
        /// </summary>
        public bool UseEuroInformationLegacyReplacingSyntax
        {
            get { return _useEuroInformationLegacyReplacingSyntax; }
            set { _useEuroInformationLegacyReplacingSyntax = value; }
        }

#if EUROINFO_RULES
        private bool _useEuroInformationLegacyReplacingSyntax = true;

        /// <summary>
        /// Instance of the CPY Copy name map
        /// </summary>
        public CopyNameMapFile CpyCopyNameMap { get; set; }

        /// <summary>
        /// Check if using the current Instance, the Given name corresponds to a CPY copy name. 
        /// </summary>
        /// <param name="name">The Copy's name</param>
        /// <returns>true if the name is CPY Copys name, false otherwise.</returns>
        public bool HasCpyCopy(string name) => CpyCopyNameMap?.Contains(name) ?? false;
#else
        private bool _useEuroInformationLegacyReplacingSyntax = false;
#endif

        /// <summary>
        /// True if we are strictly parsing Cobol source code and not TypeCobol.
        /// </summary>
        public bool IsCobolLanguage { get; set; }

        /// <summary>
        /// Internal optimization setting: it disables the creation of SpaceSeparator tokens
        /// to gain speed/memory at scanning time. This option is by default set to True but
        /// must be disabled when using Codegen.
        /// </summary>
        public bool OptimizeWhitespaceScanning { get; set; } = true;

        /// <summary>
        /// Check if a End statement is aligned with the matching opening statement.
        /// </summary>
        public TypeCobolCheckOption CheckEndAlignment { get; set; }

        /// <summary>
        /// Check if END PROGRAM have a program name associated and this name exists
        /// </summary>
        public TypeCobolCheckOption CheckEndProgram { get; set; }

        /// <summary>
        /// Check that perform statements always return to caller, requires CFG.
        /// </summary>
        public TypeCobolCheckOption CheckPerformPrematureExits { get; set; }

        /// <summary>
        /// Check procedure order of declaration against order of call in PERFORM THRU statements, requires CFG. 
        /// </summary>
        public TypeCobolCheckOption CheckPerformThruOrder { get; set; }

        /// <summary>
        /// Check that perform statement are not recursive, requires CFG.
        /// </summary>
        public TypeCobolCheckOption CheckRecursivePerforms { get; set; }

        public TypeCobolOptions(TypeCobolConfiguration config)
        {
            HaltOnMissingCopy = config.HaltOnMissingCopyFilePath != null;
            ExecToStep = config.ExecToStep;
            UseAntlrProgramParsing = config.UseAntlrProgramParsing;
            UseEuroInformationLegacyReplacingSyntax = config.UseEuroInformationLegacyReplacingSyntax;

#if EUROINFO_RULES
            AutoRemarksEnable = config.AutoRemarks;
            CpyCopyNameMap = config.CpyCopyNameMap;
            ReportUsedCopyNamesPath = config.ReportUsedCopyNamesPath;
#endif

            IsCobolLanguage = config.IsCobolLanguage;

            CheckEndAlignment = config.CheckEndAlignment;
            CheckEndProgram = config.CheckEndProgram;
            CheckPerformPrematureExits = config.CheckPerformPrematureExits;
            CheckPerformThruOrder = config.CheckPerformThruOrder;
            CheckRecursivePerforms = config.CheckRecursivePerforms;
        }

        public TypeCobolOptions()
        {
            // default values for checks
            TypeCobolCheckOptionsInitializer.SetDefaultValues(this);
        }

        /// <summary>
        /// Clone the compiler options to enable specific parameters for each file
        /// </summary>
        public TypeCobolOptions Clone()
        {
            TypeCobolOptions newOptions = new TypeCobolOptions();
            this.CopyIBMOptionsTo(newOptions);
            return newOptions;
        }
    }
}
