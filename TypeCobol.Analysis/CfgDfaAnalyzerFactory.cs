using TypeCobol.Analysis.Cfg;

namespace TypeCobol.Analysis
{
    /// <summary>
    /// Factory to help create analyzers for CFG/DFA.
    /// </summary>
    public static class CfgDfaAnalyzerFactory
    {
        /// <summary>
        /// Create the appropriate analyzer for the supplied CFG building mode.
        /// </summary>
        /// <param name="identifier">Identifier of the analyzer.</param>
        /// <param name="mode">CFG building mode.</param>
        /// <returns>New instance of ISyntaxDrivenAnalyzer. Maybe null if the mode is not supported.</returns>
        public static ISyntaxDrivenAnalyzer CreateCfgDfaAnalyzer(string identifier, CfgBuildingMode mode)
        {
            switch (mode)
            {
                case CfgBuildingMode.Standard:
                    return new DefaultControlFlowGraphBuilder(identifier);
                case CfgBuildingMode.Extended:
                    return new DefaultControlFlowGraphBuilder(identifier, true);
                //TODO DFA
            }

            return null;
        }
    }
}
