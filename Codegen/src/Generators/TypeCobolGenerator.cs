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
        /// <param name="parser"> The Parser which contains parse results </param>
        /// <param name="destination">The Output stream for the generated code</param>
        /// <param name="skeletons">All skeletons pattern for code generation </param>
        public TypeCobolGenerator(Parser parser, TextWriter destination, List<Skeleton> skeletons)
            : base(parser, destination, skeletons)
        {
        }

        protected override bool Process(Compiler.Nodes.Node node)
        {
            throw new NotImplementedException();
        }
    }
}
