using System.Collections.Generic;
using JetBrains.Annotations;
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
        /// <param name="scanState">Provide an initial MultilineScanState (from the enclosing program) to reuse information about special names paragraph or enclosing context (such as: is the copy inside a data division?)</param>
        ProcessedTokensDocument GetProcessedTokensDocument(string libraryName, [NotNull] string textName, [NotNull] MultilineScanState scanState, [CanBeNull] List<RemarksDirective.TextNameVariation> copyTextNameVariations, out PerfStatsForImportedDocument perfStats);
    }

    /// <summary>
    /// Performance metrics for compilation documents retrieved in cache
    /// </summary>
    public class PerfStatsForImportedDocument
    {
        /// <summary>
        /// True if the compilation document was instantly retrieved from an in-memory cache in the DocumentsProvider
        /// </summary>
        public bool WasRetrievedFromCache { get; set; }

        /// <summary>
        /// Number of milliseconds to locate the source file in the source library
        /// </summary>
        public int SourceFileSearchTime { get; set; }

        /// <summary>
        /// Number of milliseconds to load the source file in memory
        /// </summary>
        public int SourceFileLoadTime { get; set; }
    }
}
