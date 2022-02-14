using System;
using System.IO;
using System.Text;

namespace TypeCobol.Codegen.Generators
{
    /// <summary>
    /// The TypeCobol Generator
    /// </summary>
    public class TypeCobolGenerator : Generator
    {
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="typeCobolVersion">Current version of TypeCobol parser/codegen</param>
        public TypeCobolGenerator(TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, string typeCobolVersion)
            : base(document, destination, typeCobolVersion)
        {
        }

        protected override bool Process(Compiler.Nodes.Node node)
        {
            throw new NotImplementedException();
        }

        public override void GenerateLineMapFile(Stream stream)
        {
        }

        public override bool HasLineMapData => false;

    }
}
