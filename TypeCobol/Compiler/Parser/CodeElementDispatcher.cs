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
                if (types == null || types.Contains(e.GetType()))
                    listener.OnCodeElement(e, context);
            }
        }

        private IList<CodeElementListener> listeners = new List<CodeElementListener>();

        internal void CreateListeners() {
            listeners.Add(new DataDescriptionChecker());
            listeners.Add(new AddStatementChecker());
            listeners.Add(new StartStatementChecker());
            listeners.Add(new StopStatementChecker());
        }
    }
}
