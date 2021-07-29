using System;
using TypeCobol.Analysis.Cfg;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Factory to help create analyzers for CFG/DFA.
    /// </summary>
    public static class CfgDfaAnalyzerFactory
    {
        /// <summary>
        /// Return the identifier of the associated analyzer for the given CFG building mode.
        /// </summary>
        /// <param name="mode">CFG building mode.</param>
        /// <returns>String identifier of the CFG builder for the given mode.</returns>
        /// <remarks>Returns null for default mode None.</remarks>
        public static string GetIdForMode(CfgBuildingMode mode)
        {
            switch (mode)
            {
                case CfgBuildingMode.None:
                    return null;
                case CfgBuildingMode.Standard:
                case CfgBuildingMode.Extended:
                case CfgBuildingMode.WithDfa:
                    return "cfg-" + mode;
                default:
                    throw new NotSupportedException($"Unsupported CFG building mode '{mode}'.");
            }
        }

        /// <summary>
        /// Create the appropriate CFG analyzer for the supplied CFG building mode.
        /// </summary>
        /// <param name="mode">CFG building mode.</param>
        /// <param name="compilerOptions">Compiler options.</param>
        /// <returns>New instance of ISyntaxDrivenAnalyzer. Maybe null if the mode is not supported.</returns>
        public static ISyntaxDrivenAnalyzer CreateCfgAnalyzer(CfgBuildingMode mode, TypeCobolOptions compilerOptions)
        {
            string identifier = GetIdForMode(mode);
            switch (mode)
            {
                case CfgBuildingMode.Standard:
                    return new DefaultControlFlowGraphBuilder<object>(identifier, compilerOptions);
                case CfgBuildingMode.Extended:
                    return new DefaultControlFlowGraphBuilder<object>(identifier, compilerOptions, true);
                case CfgBuildingMode.WithDfa:
                    return new DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<VariableSymbol>>(identifier, compilerOptions, true);
            }

            return null;
        }
    }
}
