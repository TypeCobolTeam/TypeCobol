using Antlr4.Runtime;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;

namespace TypeCobol.Compiler.Parser
{
    public interface CodeElementListener {
        /// <summary>
        /// Returns the type(s) of CodeElement the listener is interested in.
        /// The listener will be notified about the creation of a CodeElement
        /// only if the CodeElement type matches at least one of the Types
        /// returned by this method.
        /// </summary>
        /// <returns>IList of CodeElement Types</returns>
        IList<System.Type> GetCodeElements();

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



	public interface NodeListener {
		/// <summary>
		/// Returns the type(s) of Node the listener is interested in.
		/// The listener will be notified about the creation of a Node
		/// only if the Node's CodeElement member type matches at least
		/// one of the Types returned by this method.
		/// </summary>
		/// <returns>IList of CodeElement Types</returns>
		IList<System.Type> GetCodeElements();

		/// <summary>
		/// Called when a CodeElement is created during ProgramClassParserStep,
		/// if the CodeElement type equals one of those returned by GetCodeElements.
		/// </summary>
		/// <param name="ce">CodeElement created</param>
		/// <param name="context">Context associated to ce's creation</param>
		/// <param name="program">Current scope program</param>
		void OnNode(Node node, ParserRuleContext context, CodeModel.Program program);
	}

	public class NodeDispatcher: NodeListener {
		public IList<System.Type> GetCodeElements() { return null; }

		/// <summary>Notifies listeners about the creation of a new CodeElement.</summary>
		public void OnNode(Node node, ParserRuleContext context, CodeModel.Program program) {
			foreach(var listener in listeners) {
				var types = listener.GetCodeElements();
				foreach (var expected in types) {
					if (TypeCobol.Tools.Reflection.IsTypeOf(node.CodeElement!=null? node.CodeElement.GetType():null, expected)) {
						listener.OnNode(node, context, program);
						break; // only notify each listener once for a given CodeElement
					}
				}
			}
		}

		private IList<NodeListener> listeners = new List<NodeListener>();

		/// <summary>
		/// Adds to listeners one instance of each type implementing CodeElementListener interface
		/// and defined in namespace TypeCobol.Compiler.Diagnostics.
		/// TODO: the list of namespace where CodeElementListeners are searched for should not be hard-coded
		/// </summary>
		internal void CreateListeners() {
			var namespaces = new string[] { "TypeCobol.Compiler.Diagnostics", };
			var assembly = System.Reflection.Assembly.GetExecutingAssembly();
			foreach (var names in namespaces) {
				var instances = TypeCobol.Tools.Reflection.GetInstances<NodeListener>(assembly, names);
				foreach (var checker in instances) listeners.Add(checker);
			}
		}
	}
}
