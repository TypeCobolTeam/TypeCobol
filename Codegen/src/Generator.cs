using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Source;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// The Second Code Generator which can Handle correctly all preprocessor directives
    /// during the Code Generation Phase.
    /// </summary>
    public abstract class Generator : IGenerator, NodeVisitor
    {
        /// <summary>
        /// The Parser that contains Parser results.
        /// </summary>
        public Parser Parser
        {
            get;
            private set;
        }

        /// <summary>
        /// The Destination.
        /// </summary>
        public TextWriter Destination
        {
            get;
            private set;
        }

        /// <summary>
        /// The Target Generated Document
        /// </summary>
        public TypeCobol.Compiler.Source.SourceDocument TargetDocument
        {
            get;
            private set;
        }

        /// <summary>
        /// Generator Actions
        /// </summary>
        public GeneratorActions Actions
        {
            get;
            private set;
        }

        /// <summary>
        /// The Generator Current Root Node
        /// </summary>
        public Root RootNode
        {
            get;
            private set;
        }

        /// <summary>
        /// Tghe Generator current symbol table
        /// </summary>
        public SymbolTable SymTable
        {
            get;
            private set;
        }

        /// <summary>
        /// The Generator current Columns Layout
        /// </summary>
        public ColumnsLayout Layout
        {
            get;
            private set;
        }

        /// <summary>
        /// The source LineMap;
        /// </summary>
        internal Dictionary<TypeCobol.Compiler.Scanner.ITokensLine, SourceDocument.SourceLine> SourceLineMap
        {
            get;
            set;
        }

        /// <summary>
        /// Lines of Erased Nodes.
        /// </summary>
        public List<Node> ErasedNodes
        {
            get;
            private set;
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="parser"> The Parser which contains parse results </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public Generator(Parser parser, TextWriter destination, List<Skeleton> skeletons)
        {
            this.Parser = parser;
            Destination = destination;
            Actions = new GeneratorActions(skeletons);
            //To Store Erased Nodes by the Erase Action.
            ErasedNodes = new List<Node>();
            //The After Action Listener
            Actions.AfterAction += OnAfterAction;
        }

        /// <summary>
        /// Handler called after an Action has beeen executed.
        /// </summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The action in fact</param>
        protected virtual void OnAfterAction(object sender, EventArgs e)
        {
            Codegen.Actions.Action action = (Codegen.Actions.Action)e;
            //Collect erased nodes.
            if (action is TypeCobol.Codegen.Actions.IEraseAction)
            {
                TypeCobol.Codegen.Actions.IEraseAction erase = (TypeCobol.Codegen.Actions.IEraseAction)action;
                IList<Node> nodes = erase.ErasedNodes;
                if (nodes != null)
                {
                    ErasedNodes.AddRange(nodes);
                }
            }
        }

		/// <summary>Generates code</summary>
		/// <param name="tree">Root of a syntax tree</param>
		/// <param name="table">Table of symbols</param>
		/// <param name="columns">Columns layout</param>
        public void Generate(TypeCobol.Compiler.Nodes.Root tree, TypeCobol.Compiler.CodeModel.SymbolTable table, TypeCobol.Compiler.Text.ColumnsLayout columns = TypeCobol.Compiler.Text.ColumnsLayout.FreeTextFormat)
        {
            // STEP 0: Initialize the global values.
            RootNode = tree;
            SymTable = table;
            Layout = columns;            
            // STEP 1: modify tree to adapt it to destination language
            Actions.Perform(tree);
            // STEP 2: convert tree to destination language code
            //Create the Initial target document.
            CreateTargetDocument();
            TreeToCode();
        }

        /// <summary>
        /// Create the Target Document.
        /// </summary>
        private void CreateTargetDocument()
        {
            TargetDocument = new Compiler.Source.SourceDocument(/*new StringSourceText()*/);
            //Insert all input lines
            StringWriter sw = new StringWriter();
            foreach (TypeCobol.Compiler.Scanner.ITokensLine line in this.Parser.Results.TokensLines)
            {
                sw.WriteLine(line.Text);
            }
            //Load the Original source code
            TargetDocument.LoadSourceText(sw.ToString());
            //TargetDocument.Dump();
            SourceLineMap = new Dictionary<TypeCobol.Compiler.Scanner.ITokensLine, SourceDocument.SourceLine>();            
            var iter = this.Parser.Results.TokensLines.GetEnumerator();
            for (int i = 0; i < TargetDocument.LineCount && iter.MoveNext(); i++)
            {
                SourceLineMap[iter.Current] = TargetDocument[i];
            }
        }

        /// <summary>
        /// Perform Tree to Code generation
        /// </summary>
        protected virtual void TreeToCode()
        {
            RootNode.Accept(this);
        }
    
        /// <summary>
        /// Visit Method over tree node visitation
        /// </summary>
        /// <param name="node">The Node to visit</param>
        public void Visit(Node node)
        {
            bool doVisitChildren = Process(node);
            if (doVisitChildren)
            {
                foreach (var child in node.Children)
                    child.Accept(this);
            }
        }

        /// <summary>
        /// Abstract method to Process a node
        /// </summary>
        /// <param name="node">The node to process</param>
        /// <returns>true if child nodes must visited for acceptation, false otherwise.</returns>
        protected abstract bool Process(Node node);
    }
}
