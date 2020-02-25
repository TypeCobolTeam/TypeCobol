using System;
using System.Text;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// GeneratorFactory delegation method
    /// </summary>
    /// <param name="ID">The unique ID of the Generator to create</param>
    /// <param name="document"> The compilation document </param>
    /// <param name="destination">The Output stream for the generated code</param>
    /// <param name="typeCobolVersion">Current version of TypeCobol tools</param>
    /// <param name="bWithLineMap">True if Line Map can be generated</param>
    /// <returns>An instance of the IGenerator interface if one is created, null otherwise</returns>
    public delegate IGenerator GeneratorFactory (String ID, TypeCobol.Compiler.CompilationDocument document, StringBuilder destination, string typeCobolVersion, bool bWithLineMap);
}
