using Antlr4.Runtime;
using System.Collections.Generic;
using System.Reflection;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Tools;

namespace TypeCobol.Compiler.Parser
{
    /// <summary>
    /// An delegate for Factories used to create Node Listener
    /// </summary>
    public delegate NodeListener NodeListenerFactory();

    /// <summary>
    /// Node Listener with a parsing context
    /// </summary>
    public interface NodeListener
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

    public class NodeDispatcher : NodeListener {
        /// <summary>
        /// List of Static NodeListener Factory
        /// </summary>
        private static List<NodeListenerFactory> StaticNodeListenerFactory = null;
        /// <summary>
        /// Add Static NodeListenerFactory instance
        /// </summary>
        /// <param name="listener">The instance to be added</param>
        public static void RegisterStaticNodeListenerFactory(NodeListenerFactory listener)
        {
            lock (typeof(NodeDispatcher))
            {
                if (StaticNodeListenerFactory == null && listener != null)
                {
                    StaticNodeListenerFactory = new List<NodeListenerFactory>();
                }
                StaticNodeListenerFactory.Add(listener);
            }
        }
        /// <summary>
        /// Remove Static NodeListenerFactory instance
        /// </summary>
        /// <param name="listener">The instance to be removed</param>
        public static void RemoveStaticNodeListenerFactory(NodeListenerFactory listener)
        {
            lock (typeof(NodeDispatcher))
            {
                if (StaticNodeListenerFactory != null && listener != null)
                {
                    StaticNodeListenerFactory.Remove(listener);
                }
            }
        }
        

        /// <summary>Notifies listeners about the creation of a new CodeElement.</summary>
        public void OnNode(Node node, Program program)
        {
            foreach (var listener in _listeners)
            {
                listener.OnNode(node, program);
            }
        }

        private IList<NodeListener> _listeners = null;

        /// <summary>
        /// Adds to listeners one instance of each type implementing CodeElementListener interface
        /// and defined in namespace TypeCobol.Compiler.Diagnostics.
        /// TODO: the list of namespace where CodeElementListeners are searched for should not be hard-coded
        /// </summary>
        internal void CreateListeners()
        {
            //Do nothing if listeners already exist
            if (_listeners != null)
            {
                return;
            }
            _listeners = new List<NodeListener>();
            //Return if no StaticNodeListenerFactory exist
            if (StaticNodeListenerFactory == null)
            {
                return;
            }
            lock (typeof(NodeDispatcher))
            {
                foreach (NodeListenerFactory factory in StaticNodeListenerFactory)
                {
                    //Allocate listeners from static factories.
                    NodeListener listener = factory();
                    if (listener != null)
                    {
                        _listeners.Add(listener);
                    }
                }
            }
        }
    }
}
