using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// GeneratorFactory delegation method
    /// </summary>
    /// <param name="ID">The unique ID of the Generator to create</param>
    /// <param name="Document"> The compilation document </param>
    /// <param name="destination">The Output stream for the generated code</param>
    /// <param name="skeletons">All skeletons pattern for code generation </param>
    /// <returns>An instance of the IGenerator interface if one is created, null otherwise</returns>
    public delegate IGenerator GeneratorFactory (String ID, TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, List<Skeleton> skeletons, string typeCobolVersion);
}
