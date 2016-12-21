using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// The Second Code Generator which can Handle correctly all preprocessor directives
    /// during the Code Generation Phase.
    /// </summary>
    public class Generator2
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
        /// Constructor
        /// </summary>
        /// <param name="parser"> The Parser which contains parse results </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public Generator2(Parser parser, TextWriter destination, List<Skeleton> skeletons)
        {
            this.Parser = parser;
            Destination = destination;
            Actions = new GeneratorActions(skeletons);
        }

		/// <summary>Generates code</summary>
		/// <param name="tree">Root of a syntax tree</param>
		/// <param name="table">Table of symbols</param>
		/// <param name="columns">Columns layout</param>
        public void Generate(TypeCobol.Compiler.Nodes.Root tree, TypeCobol.Compiler.CodeModel.SymbolTable table, TypeCobol.Compiler.Text.ColumnsLayout columns = TypeCobol.Compiler.Text.ColumnsLayout.FreeTextFormat)
        {
            // STEP 1: modify tree to adapt it to destination language
            Actions.Perform(tree);
            // STEP 2: convert tree to destination language code
            //Create the Initial target document.
            CreateTargetDocument();
        }

        /// <summary>
        /// Create the Target Document.
        /// </summary>
        private void CreateTargetDocument()
        {
            TargetDocument = new Compiler.Source.SourceDocument();
            //Insert all input lines
            StringWriter sw = new StringWriter();
            foreach (var line in this.Parser.Results.TokensLines)
            {
                sw.WriteLine(line.Text);
            }
            //Load the Original source code
            TargetDocument.LoadSourceText(sw.ToString());
            TargetDocument.Dump();
        }
    }
}
