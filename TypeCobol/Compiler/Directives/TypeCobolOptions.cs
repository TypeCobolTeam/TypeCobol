using System;
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
        /// The Instance of the Cpy Copy names Map
        /// </summary>
        private CopyNameMapFile _cpyCopyNamesMap;

        private string _cpyCopyNamesMapFilePath;
        /// <summary>
        /// Path to the CpyCopyNames file.
        /// </summary>
        public string CpyCopyNamesMapFilePath
        {
            get
            {
                return _cpyCopyNamesMapFilePath; 
            }
            set
            {
                if (value == null)
                {
                    _cpyCopyNamesMapFilePath = null;
                    _cpyCopyNamesMap = null;
                }
                else if (!value.Equals(_cpyCopyNamesMapFilePath, StringComparison.OrdinalIgnoreCase))
                {
                    _cpyCopyNamesMapFilePath = value;
                    _cpyCopyNamesMap = new CopyNameMapFile(value);
                }                
            }
        }
        /// <summary>
        /// Check if using the current Instance, the Given name corresponds to a CPY copy name. 
        /// </summary>
        /// <param name="name">The Copy's name</param>
        /// <returns>true if the name is CPY Copys name, false otherwise.</returns>
        public bool HasCpyCopy(string name) => _cpyCopyNamesMap?.HasCpyCopy(name) ?? false;
#else
        private bool _useEuroInformationLegacyReplacingSyntax = false;
#endif

        /// <summary>
        /// Check if a End statement is aligned with the matching opening statement.
        /// </summary>
        public TypeCobolCheckOption CheckEndAlignment { get; set; }

        public TypeCobolOptions(TypeCobolConfiguration config)
        {
            HaltOnMissingCopy = config.HaltOnMissingCopyFilePath != null;
            ExecToStep = config.ExecToStep;
            UseAntlrProgramParsing = config.UseAntlrProgramParsing;
            UseEuroInformationLegacyReplacingSyntax = config.UseEuroInformationLegacyReplacingSyntax;
            CheckEndAlignment = config.CheckEndAlignment;
#if EUROINFO_RULES
            AutoRemarksEnable = config.AutoRemarks;
            try {
                CpyCopyNamesMapFilePath = config.CpyCopyNamesMapFilePath;
            } catch(System.Exception e) {
                //Fail to read the Copy File Name, Log
                System.IO.File.AppendAllText(config.LogFile != null ? config.LogFile : TypeCobolConfiguration.DefaultLogFileName, e.ToString());
            }
#endif
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
