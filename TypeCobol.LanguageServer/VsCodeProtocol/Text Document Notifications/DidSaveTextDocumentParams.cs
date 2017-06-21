using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The document save notification is sent from the client to the server when the document for saved in the client.
    /// </summary>
    public class DidSaveTextDocumentParams
    {
  
        /// <summary>
        /// The document that was saved.
        /// </summary>
        public TextDocumentIdentifier textDocument;

        /// <summary>
        /// Optional the content when saved. Depends on the includeText value when the save notification was requested.
        /// </summary>
        public String text;

        /// <summary>
        /// Document constructor
        /// </summary>
        /// <param name="textDocument">The document being saved</param>
        /// <param name="text">The text of the document</param>
        public DidSaveTextDocumentParams(TextDocumentIdentifier textDocument, String text = null)
        {
            this.textDocument = textDocument;
            this.text = text;
        }
    }
}
