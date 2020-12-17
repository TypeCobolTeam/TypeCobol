using TypeCobol.Analysis.Cfg;
using TypeCobol.Analysis.Dfa;
using TypeCobol.Compiler.Symbols;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Factory to help create analyzers for CFG/DFA.
    /// </summary>
    public static class CfgDfaAnalyzerFactory
    {
        /// <summary>
        /// Create the appropriate CFG analyzer for the supplied CFG building mode.
        /// </summary>
        /// <param name="identifier">Identifier of the analyzer.</param>
        /// <param name="mode">CFG building mode.</param>
        /// <returns>New instance of ISyntaxDrivenAnalyzer. Maybe null if the mode is not supported.</returns>
        public static ISyntaxDrivenAnalyzer CreateCfgAnalyzer(string identifier, CfgBuildingMode mode)
        {
            switch (mode)
            {
                case CfgBuildingMode.Standard:
                    return new DefaultControlFlowGraphBuilder<object>(identifier);
                case CfgBuildingMode.Extended:
                    return new DefaultControlFlowGraphBuilder<object>(identifier, true);
                case CfgBuildingMode.WithDfa:
                    return new DefaultControlFlowGraphBuilder<DfaBasicBlockInfo<VariableSymbol>>(identifier, true);
            }

            return null;
        }
    }
}
