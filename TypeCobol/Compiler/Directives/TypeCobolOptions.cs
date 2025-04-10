#nullable enable

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
        /// <summary>
        /// Path to generate collected used copy names
        /// </summary>
        public string? ReportUsedCopyNamesPath { get; set; }
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

#if EUROINFO_RULES
        /// <summary>
        /// Euro-Information Legacy REPLACING syntax: automatically remove first 01 level from included CPY copies.
        /// </summary>
        public bool EILegacy_RemoveFirst01Level { get; set; } = true;

        /// <summary>
        /// Euro-Information Legacy REPLACING syntax: automatically suffix data items coming
        /// from CPY copies when they are included using an additional char at the end of their text name.
        /// </summary>
        public bool EILegacy_ApplyCopySuffixing { get; set; } = true;

        /// <summary>
        /// Instance of the CPY Copy name map
        /// </summary>
        public CopyNameMapFile? CpyCopyNameMap { get; set; }
#else
        public bool EILegacy_RemoveFirst01Level { get; set; } = false;
        public bool EILegacy_ApplyCopySuffixing { get; set; } = false;
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
        /// Enable parsing of SQL code embedded into EXEC SQL [...] END-EXEC blocks.
        /// </summary>
        public bool EnableSqlParsing { get; set; } = false;

        /// <summary>
        /// Check if a End statement is aligned with the matching opening statement.
        /// </summary>
        public TypeCobolCheckOption CheckEndAlignment { get; set; }

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

        /// <summary>
        /// Check that CodeElement do not mix debug and non-debug lines.
        /// </summary>
        public TypeCobolCheckOption CheckCodeElementMixedDebugType { get; set; }

        public TypeCobolOptions(TypeCobolConfiguration config)
        {
            HaltOnMissingCopy = config.HaltOnMissingCopyFilePath != null;
            ExecToStep = config.ExecToStep;
            UseAntlrProgramParsing = config.UseAntlrProgramParsing;
            EILegacy_RemoveFirst01Level = config.EILegacy_RemoveFirst01Level;
            EILegacy_ApplyCopySuffixing = config.EILegacy_ApplyCopySuffixing;

#if EUROINFO_RULES
            CpyCopyNameMap = config.CpyCopyNameMap;
            ReportUsedCopyNamesPath = config.ReportUsedCopyNamesPath;
#endif

            IsCobolLanguage = config.IsCobolLanguage;

            CheckEndAlignment = config.CheckEndAlignment;
            CheckPerformPrematureExits = config.CheckPerformPrematureExits;
            CheckPerformThruOrder = config.CheckPerformThruOrder;
            CheckRecursivePerforms = config.CheckRecursivePerforms;
            CheckCodeElementMixedDebugType = config.CheckCodeElementMixedDebugType;
        }

        public TypeCobolOptions()
        {
            // default values for checks
            CheckEndAlignment = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckEndAlignmentSeverity);
            CheckPerformPrematureExits = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckPerformPrematureExitsSeverity);
            CheckPerformThruOrder = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckPerformThruOrderSeverity);
            CheckRecursivePerforms = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckRecursivePerformsSeverity);
            CheckCodeElementMixedDebugType = new TypeCobolCheckOption(ITypeCobolCheckOptions.DefaultCheckCodeElementMixedDebugTypeSeverity);
        }
    }
}
