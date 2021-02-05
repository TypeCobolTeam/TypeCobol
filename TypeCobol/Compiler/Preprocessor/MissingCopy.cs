using TypeCobol.Compiler.Directives;

namespace TypeCobol.Compiler.Preprocessor
{
    /// <summary>
    /// Describes a copybook that could not be found while parsing.
    /// </summary>
    public class MissingCopy
    {
        /// <summary>
        /// The CompilationDocument requesting the copy
        /// </summary>
        public CompilationDocument IncludingDocument { get; }

        /// <summary>
        /// The CopyDirective that failed to be resolved
        /// </summary>
        public CopyDirective FaultyDirective { get; }

        internal MissingCopy(CompilationDocument includingDocument, CopyDirective faultyDirective)
        {
            IncludingDocument = includingDocument;
            FaultyDirective = faultyDirective;
        }
    }
}
