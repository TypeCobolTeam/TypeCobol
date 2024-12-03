using TypeCobol.LanguageServer.VsCodeProtocol;

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
        public TextDocumentIdentifier textDocument;

        /// <summary>
        /// Range in the document concerned by this Syntax Coloring.
        /// If this parameter in null, then the whole document must be rescanned. 
        /// </summary>
        public VsCodeProtocol.Range DocumentRange;

        /// <summary>
        /// The List of token concerned
        /// </summary>
        public List<Token> Tokens;
    }
}
