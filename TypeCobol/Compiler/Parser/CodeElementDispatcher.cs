using System.Collections.Generic;
using TypeCobol.Compiler.CupParser.NodeBuilder;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// A delegate for Factories used to create Node Listener
    /// </summary>
    public delegate IProgramClassBuilderNodeListener NodeListenerFactory();

    public static class NodeDispatcher
    {
        /// <summary>
        /// List of Static NodeListener Factories
        /// </summary>
        private static List<NodeListenerFactory> _NodeListenerFactories;

        /// <summary>
        /// Private lock object to protect collection of factories against concurrency
        /// </summary>
        private static readonly object _NodeListenerFactoriesLockObject = new object();

        /// <summary>
        /// Add Static NodeListenerFactory instance
        /// </summary>
        /// <param name="listenerFactory">The instance to be added</param>
        public static void RegisterStaticNodeListenerFactory(NodeListenerFactory listenerFactory)
        {
            if (listenerFactory == null) return;

            lock (_NodeListenerFactoriesLockObject)
            {
                if (_NodeListenerFactories == null)
                {
                    _NodeListenerFactories = new List<NodeListenerFactory>();
                }
                _NodeListenerFactories.Add(listenerFactory);
            }
        }

        /// <summary>
        /// Remove Static NodeListenerFactory instance
        /// </summary>
        /// <param name="listenerFactory">The instance to be removed</param>
        public static void RemoveStaticNodeListenerFactory(NodeListenerFactory listenerFactory)
        {
            if (listenerFactory == null) return;

            lock (_NodeListenerFactoriesLockObject)
            {
                if (_NodeListenerFactories != null)
                {
                    _NodeListenerFactories.Remove(listenerFactory);
                }
            }
        }

        public static IEnumerable<IProgramClassBuilderNodeListener> CreateListeners()
        {
            if (_NodeListenerFactories == null) yield break;

            lock (_NodeListenerFactoriesLockObject)
            {
                foreach (NodeListenerFactory factory in _NodeListenerFactories)
                {
                    //Allocate listeners from static factories.
                    IProgramClassBuilderNodeListener listener = factory();
                    if (listener != null)
                    {
                        yield return listener;
                    }
                }
            }
        }
    }
}
