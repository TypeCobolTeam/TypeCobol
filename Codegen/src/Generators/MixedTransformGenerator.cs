using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Codegen.Generators
{
    public class MixedTransformGenerator : Generator
    {
        /// <summary>
        /// Generator used to generate Cobol code (Could be Cobol85, Cobol2002 ...)
        /// </summary>
        private Generator _usedGenerator;
        public MixedTransformGenerator(CompilationDocument document, StringBuilder destination, Generator generator) 
            : base(document, destination, null)
        {
            _usedGenerator = generator;
        }

        /// <summary>
        /// Lines of Erased Nodes.
        /// </summary>
        public new List<Node> ErasedNodes
        {
            get;
            private set;
        }

        public override List<Diagnostic> Diagnostics => _usedGenerator.Diagnostics;

        protected override bool Process(Node node)
        {
            throw new NotImplementedException();
        }

        public override void Generate(CompilationUnit compilationUnit, ColumnsLayout columns = ColumnsLayout.FreeTextFormat)
        {
            _usedGenerator.Generate(compilationUnit, columns);

            //After generation get the generated cobol code and mix it with TypeCobol source using Transform project
            var mixedContent = new StringBuilder();
            Transform.Decoder.Encode(compilationUnit.CobolTextLines.Select(l => l.Text).ToArray(), Destination.ToString().Split(new string[] {Environment.NewLine, "\n", "\r"}, StringSplitOptions.None), mixedContent);
            this.Destination.Clear();
            this.Destination.Append(mixedContent);
        }

        public override void GenerateLineMapFile(Stream stream)
        {
            _usedGenerator.GenerateLineMapFile(stream);
        }
        public override bool HasLineMapData => _usedGenerator.HasLineMapData;

    }
}
