namespace TypeCobol.Analysis.Graph
{
    /// <summary>
    /// Defines a Control Flow Graph transformer.
    /// </summary>
    /// <typeparam name="N">Type used to represent instructions on the graph.</typeparam>
    /// <typeparam name="D">Type used for custom Data on basic blocks.</typeparam>
    public interface ICfgTransform<N, D>
    {
        /// <summary>
        /// Apply a transformation on a CFG and return the resulting CFG.
        /// </summary>
        /// <param name="graph">Graph to be transformed.</param>
        /// <returns>The transformed graph. It may be the same instance or a new one depending on the implementation.</returns>
        ControlFlowGraph<N, D> Transform(ControlFlowGraph<N, D> graph);
    }
}
