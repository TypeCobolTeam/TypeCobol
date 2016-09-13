using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// The build system is free to implement the most efficient strategy to build
    /// and cache ProcessedTokenDocuments for all the files imported by COPY directives
    /// </summary>
    public interface IProcessedTokensDocumentProvider
    {
        /// <summary>
        /// Load, scan and process the text file referenced by a COPY compiler directive :
        /// COPY textName ((OF | IN) libraryName)?
        /// </summary>
        ProcessedTokensDocument GetProcessedTokensDocument(string libraryName, string textName);
    }
}
