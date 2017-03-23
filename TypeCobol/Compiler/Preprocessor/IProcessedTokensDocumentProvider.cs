using System.Collections.Generic;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Scanner;

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

        /// <summary>
        /// Load, scan and process the text file referenced by a COPY compiler directive :
        /// COPY textName ((OF | IN) libraryName)?
        /// </summary>
        /// <param name="scanState">Provide an initial MultilineScanState (from the enclosing program) to reuse information about special names paragraph or enclosing context (such as: is the copy inside a data division?)</param>
        ProcessedTokensDocument GetProcessedTokensDocument(string libraryName, string textName, MultilineScanState scanState, List<RemarksDirective.TextNameVariation> copyTextNameVariations);
    }
}
