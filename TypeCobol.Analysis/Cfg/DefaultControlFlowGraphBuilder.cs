namespace TypeCobol.Analysis.Cfg
{
    /// <summary>
    /// Default Control Flow Graph Builder with any object has Data.
    /// </summary>
    public class DefaultControlFlowGraphBuilder : ControlFlowGraphBuilder<object>
    {
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="mode">CFG building mode.</param>
        /// <param name="useEvaluateCascade">True to convert EVALUATE statements into cascaded-IFs.</param>
        /// <param name="useSearchCascade">True to convert SEARCH statements into cascaded-IFs.</param>
        public DefaultControlFlowGraphBuilder(CfgMode mode = CfgMode.Normal, bool useEvaluateCascade = true, bool useSearchCascade = true)
            : base(mode, useEvaluateCascade, useSearchCascade)
        {

        }
    }
}
