using System.Collections.Generic;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// A delegate for Factories used to create Node Listener
    /// </summary>
    public delegate INodeListener NodeListenerFactory();

    /// <summary>
    /// Node Listener with a parsing context
    /// </summary>
    public interface INodeListener
    {
        /// <summary>
        /// Called when a CodeElement is created during ProgramClassParserStep,
        /// if the CodeElement type equals one of those returned by GetCodeElements.
        /// </summary>
        /// <param name="node"></param>
        /// <param name="program">Current scope program</param>
        /// <param name="ce">CodeElement created</param>
        void OnNode(Node node, Program program);
    }

    public class NodeDispatcher : INodeListener
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

        /// <summary>
        /// Notifies listeners about the creation of a new CodeElement.
        /// </summary>
        public void OnNode(Node node, Program program)
        {
            System.Diagnostics.Debug.Assert(_listeners != null);
            foreach (var listener in _listeners)
            {
                listener.OnNode(node, program);
            }
        }

        private IList<INodeListener> _listeners;

        /// <summary>
        /// Add a listener
        /// </summary>
        /// <param name="listener">The listener to be added</param>
        protected virtual void AddListener(INodeListener listener)
        {
            System.Diagnostics.Debug.Assert(_listeners != null);
            _listeners.Add(listener);
        }

        /// <summary>
        /// Adds to listeners one instance of each type implementing CodeElementListener interface
        /// and defined in namespace TypeCobol.Compiler.Diagnostics.
        /// </summary>
        internal void CreateListeners()
        {
            //Do nothing if listeners already exist
            if (_listeners != null)
            {
                return;
            }

            _listeners = new List<INodeListener>();

            //Return if no _NodeListenerFactories exist
            if (_NodeListenerFactories == null)
            {
                return;
            }

            lock (_NodeListenerFactoriesLockObject)
            {
                foreach (NodeListenerFactory factory in _NodeListenerFactories)
                {
                    //Allocate listeners from static factories.
                    INodeListener listener = factory();
                    if (listener != null)
                    {
                        AddListener(listener);
                    }
                }
            }
        }
    }
}
