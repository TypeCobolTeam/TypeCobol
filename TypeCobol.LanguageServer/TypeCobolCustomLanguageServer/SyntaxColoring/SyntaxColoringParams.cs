using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.WebSockets;
using System.Text;
using System.Threading.Tasks;
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
        public TextDocumentItem Document;

        /// <summary>
        /// Range in the document concerned by this Syntax Coloring.
        /// If this parameter in null, then the whole document must be rescanned. 
        /// </summary>
        public VsCodeProtocol.Range DocumentRange;

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
        public SyntaxColoringParams(TextDocumentItem document, VsCodeProtocol.Range docRange, List<Token> tokens)
        {
            this.Document = document;
            this.DocumentRange = docRange;
            this.Tokens = tokens;
        }
    }
}
