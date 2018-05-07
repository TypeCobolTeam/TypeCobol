using Antlr4.Runtime;
using System.Collections.Generic;
using System.Reflection;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Tools;

namespace TypeCobol.Compiler.Parser
{
    public interface CodeElementListener {
        /// <summary>
        /// Called when a CodeElement is created during CodeElementParserStep,
        /// if the CodeElement type equals one of those returned by GetCodeElements.
        /// </summary>
        /// <param name="ce">CodeElement created</param>
        /// <param name="context">Context associated to ce's creation</param>
        void OnCodeElement(CodeElement ce, ParserRuleContext context);
    }

    public class CodeElementDispatcher: CodeElementListener {
        public IList<System.Type> GetCodeElements() { return null; }

        /// <summary>Notifies listeners about the creation of a new CodeElement.</summary>
        public void OnCodeElement(CodeElement e, ParserRuleContext context) {
            foreach(var listener in _listeners) {
                listener.OnCodeElement(e, context);
            }
        }

        private IList<CodeElementListener> _listeners = null;

        /// <summary>
        /// Adds to listeners one instance of each type implementing CodeElementListener interface
        /// and defined in namespace TypeCobol.Compiler.Diagnostics.
        /// TODO: the list of namespace where CodeElementListeners are searched for should not be hard-coded
        /// </summary>
        internal void CreateListeners() {
            if (_listeners == null) {
                _listeners = new List<CodeElementListener>();
                var namespaces = new[] { "TypeCobol.Compiler.Diagnostics", };
                var assembly = Assembly.GetExecutingAssembly();
                foreach (var names in namespaces) {
                    var instances = Reflection.GetInstances<CodeElementListener>(assembly, names);
                    foreach (var checker in instances) _listeners.Add(checker);
                }
            }
        }
    }

    /// <summary>
    /// An delegate for Factories used to create Node Listener
    /// </summary>
    public delegate NodeListener<TCtx> NodeListenerFactory<TCtx>() where TCtx : class;

    /// <summary>
    /// Node Listener with a parsing context
    /// </summary>
    /// <typeparam name="TCtx">Parsing context</typeparam>
	public interface NodeListener<TCtx> where TCtx : class
    {
        /// <summary>
        /// Called when a CodeElement is created during ProgramClassParserStep,
        /// if the CodeElement type equals one of those returned by GetCodeElements.
        /// </summary>
        /// <param name="ce">CodeElement created</param>
        /// <param name="context">Context associated to ce's creation</param>
        /// <param name="program">Current scope program</param>
        void OnNode(Node node, TCtx context, CodeModel.Program program);
    }

    public class NodeDispatcher<TCtx> : NodeListener<TCtx> where TCtx : class {
        /// <summary>
        /// List of Static NodeListener Factory
        /// </summary>
        private static List<NodeListenerFactory<TCtx>> StaticNodeListenerFactory = null;
        /// <summary>
        /// Add Static NodeListenerFactory instance
        /// </summary>
        /// <param name="listener">The instance to be added</param>
        public static void RegisterStaticNodeListenerFactory(NodeListenerFactory<TCtx> listener)
        {
            lock (typeof(NodeDispatcher<TCtx>))
            {
                if (StaticNodeListenerFactory == null && listener != null)
                {
                    StaticNodeListenerFactory = new List<NodeListenerFactory<TCtx>>();
                }
                StaticNodeListenerFactory.Add(listener);
            }
        }
        /// <summary>
        /// Remove Static NodeListenerFactory instance
        /// </summary>
        /// <param name="listener">The instance to be removed</param>
        public static void RemoveStaticNodeListener(NodeListenerFactory<TCtx> listener)
        {
            lock (typeof(NodeDispatcher<TCtx>))
            {
                if (StaticNodeListenerFactory != null && listener != null)
                {
                    StaticNodeListenerFactory.Remove(listener);
                }
            }
        }

        public IList<System.Type> GetNodes() { return null; }

        /// <summary>Notifies listeners about the creation of a new CodeElement.</summary>
        public void OnNode(Node node, TCtx context, CodeModel.Program program)
        {
            foreach (var listener in _listeners)
            {
                listener.OnNode(node, context, program);
            }
        }

        private IList<NodeListener<TCtx>> _listeners = null;

        /// <summary>
        /// Adds to listeners one instance of each type implementing CodeElementListener interface
        /// and defined in namespace TypeCobol.Compiler.Diagnostics.
        /// TODO: the list of namespace where CodeElementListeners are searched for should not be hard-coded
        /// </summary>
        internal void CreateListeners()
        {
            if (_listeners == null)
            {
                _listeners = new List<NodeListener<TCtx>>();
                var namespaces = new[] { "TypeCobol.Compiler.Diagnostics", };
                var assembly = Assembly.GetExecutingAssembly();
                foreach (var names in namespaces)
                {
                    var instances = Reflection.GetInstances<NodeListener<TCtx>>(assembly, names);
                    foreach (var checker in instances)
                    {
                        _listeners.Add(checker);
                    }
                    //Allocate listeners from static factories.
                    if (StaticNodeListenerFactory != null)
                    {
                        foreach (NodeListenerFactory<TCtx> factory in StaticNodeListenerFactory)
                        {
                            NodeListener<TCtx> listener = factory();
                            if (listener != null)
                            {
                                _listeners.Add(listener);
                            }
                        }
                    }
                }
            }
        }
    }
}
