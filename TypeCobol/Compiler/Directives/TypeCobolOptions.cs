using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Compiler.Directives
{
    /// <summary>
    /// TypeCobol compiler options (superset of the IBM Enterprise Cobol compiler options)
    /// </summary>
    public class TypeCobolOptions : IBMCompilerOptions
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
        private bool _checkProgramName = true;
        private bool _useCheckProgramName = true;
#else
        private bool _useEuroInformationLegacyReplacingSyntax;
        private bool _checkProgramName = false;
        private bool _useCheckProgramName = false;
#endif
        /// <summary>
        /// Check if program name matches the file name.
        /// </summary>
        public bool CheckProgramName
        {
            get { return _checkProgramName && UseCheckProgramName; }
            set { _checkProgramName = value; }
        }
        /// <summary>
        /// Inhib CheckProgramName action.
        /// </summary>
        public bool UseCheckProgramName
        {
            get { return _useCheckProgramName; }
            set { _useCheckProgramName = value; }
        }


        public TypeCobolOptions(TypeCobolConfiguration config)
        {
            HaltOnMissingCopy = config.HaltOnMissingCopyFilePath != null;
            ExecToStep = config.ExecToStep;
            UseAntlrProgramParsing = config.UseAntlrProgramParsing;
            UseEuroInformationLegacyReplacingSyntax = config.UseEuroInformationLegacyReplacingSyntax;
            CheckProgramName = config.CheckProgramName;
            UseCheckProgramName = config.UseCheckProgramName;
        }

        public TypeCobolOptions()
        {
                
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
