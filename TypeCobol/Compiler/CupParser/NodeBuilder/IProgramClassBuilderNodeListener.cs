using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.CupParser.NodeBuilder
{
    /// <summary>
    /// A Node Listener that can also dispatch Program Class Builder events.
    /// </summary>
    public interface IProgramClassBuilderNodeListener : IProgramClassBuilder
    {
        /// <summary>
        /// Called when a node is entered.
        /// </summary>
        /// <param name="node">The entered node.</param>
        void Enter(Node node);

        /// <summary>
        /// Called when a Node is fully built but before exiting.
        /// </summary>
        /// <param name="node">The built node.</param>
        /// <param name="program">The owner program of the node.</param>
        void OnNode(Node node, Program program);

        /// <summary>
        /// Called when a node is exited.
        /// </summary>
        /// <param name="node">The exited node.</param>
        void Exit(Node node);

        /// <summary>
        /// Tracks level-01 data definitions.
        /// </summary>
        /// <param name="level1Node">Level-01 data definition node.</param>
        void OnLevel1Definition(DataDefinition level1Node);
    }
}
