using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen
{
    /// <summary>
    /// GeneratorFactory delegation method
    /// </summary>
    /// <param name="ID">The unique ID of the Generator to create</param>
    /// <param name="parser"> The Parser which contains parse results </param>
    /// <param name="destination">The Output stream for the generated code</param>
    /// <param name="skeletons">All skeletons pattern for code generation </param>
    /// <returns>An instance of the IGenerator interface if one is created, null otherwise</returns>
    public delegate IGenerator GeneratorFactory (String ID, Parser parser, TextWriter destination, List<Skeleton> skeletons);
}
