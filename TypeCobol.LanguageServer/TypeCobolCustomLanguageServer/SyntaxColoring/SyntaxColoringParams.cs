using Microsoft.VisualStudio.LanguageServer.Protocol;

using Range = Microsoft.VisualStudio.LanguageServer.Protocol.Range;

namespace TypeCobol.LanguageServer.TypeCobolCustomLanguageServerProtocol.SyntaxColoring
{
    /// <summary>
    /// Syntax Coloring Parameters
    /// </summary>
    public class SyntaxColoringParams
    {
        /// <summary>
        /// The Text Document Identifier
        /// </summary>
        public TextDocumentItem textDocument;

        /// <summary>
        /// Range in the document concerned by this Syntax Coloring.
        /// If this parameter in null, then the whole document must be rescanned. 
        /// </summary>
        public Range DocumentRange;

        /// <summary>
        /// The List of token concerned
        /// </summary>
        public List<Token> Tokens;

        /// <summary>
        /// Constructor. All document to be rescanned.
        /// </summary>
        /// <param name="document">The Document's identifier</param>
        /// <param name="tokens">The tokens</param>
        public SyntaxColoringParams(TextDocumentItem document, List<Token> tokens) : this(document, null, tokens)
        {
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="document">The Document's identifier</param>
        /// <param name="docRange">Document's range to refresh</param>
        /// <param name="tokens">The List of concerned tokens in the range</param>
        public SyntaxColoringParams(TextDocumentItem document, Range docRange, List<Token> tokens)
        {
            this.textDocument = document;
            this.DocumentRange = docRange;
            this.Tokens = tokens;
        }
    }
}
