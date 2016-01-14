using Antlr4.Runtime;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;

namespace TypeCobol.Compiler.Parser
{
    interface CodeElementListener {
        /// <summary>
        /// Returns the type(s) of CodeElement the listener is interested in.
        /// The listener will be notified about the creation of a CodeElement
        /// only if the CodeElement type matches at least one of the Types
        /// returned by this method.
        /// </summary>
        /// <returns>IList of CodeElement Types</returns>
        IList<System.Type> GetCodeElements();

        /// <summary>
        /// Called when a CodeElement is created, but only if the CodeElement
        /// type equals one of those returned by GetCodeElements.
        /// </summary>
        /// <param name="ce">CodeElement created</param>
        void OnCodeElement(CodeElement ce, ParserRuleContext context);
    }

    class CodeElementDispatcher: CodeElementListener {
        public IList<System.Type> GetCodeElements() { return null; }

        /// <summary>
        /// Notifies listeners about the creation of a new CodeElement.
        /// </summary>
        /// <param name="ce">CodeElement created.</param>
        public void OnCodeElement(CodeElement e, ParserRuleContext context) {
            foreach(var listener in listeners) {
                var types = listener.GetCodeElements();
                foreach (var expected in types) {
                    if (TypeCobol.Tools.Reflection.IsTypeOf(e.GetType(), expected)) {
                        listener.OnCodeElement(e, context);
                        break; // only notify each listener once for a given CodeElement
                    }
                }
            }
        }

        private IList<CodeElementListener> listeners = new List<CodeElementListener>();

        /// <summary>
        /// Adds to listeners one instance of each type implementing CodeElementListener interface
        /// and defined in namespace TypeCobol.Compiler.Diagnostics.
        /// TODO: the list of namespace where CodeElementListeners are searched for should not be hard-coded
        /// </summary>
        internal void CreateListeners() {
            var namespaces = new string[] { "TypeCobol.Compiler.Diagnostics", };
            var assembly = System.Reflection.Assembly.GetExecutingAssembly();
            foreach (var names in namespaces) {
                var instances = TypeCobol.Tools.Reflection.GetInstances<CodeElementListener>(assembly, names);
                foreach (var checker in instances) listeners.Add(checker);
            }
        }
    }
}
