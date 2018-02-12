using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;
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
        /// <param name="Document"> The compilation document </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public TypeCobolGenerator(TypeCobol.Compiler.CompilationDocument document, TextWriter destination, List<Skeleton> skeletons, string typeCobolVersion)
            : base(document, destination, skeletons, typeCobolVersion)
        {
        }

        protected override bool Process(Compiler.Nodes.Node node)
        {
            throw new NotImplementedException();
        }
    }
}
