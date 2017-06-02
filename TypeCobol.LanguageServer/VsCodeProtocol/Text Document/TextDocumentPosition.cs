/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A literal to define the position in a text document.
    /// </summary>
    public class TextDocumentPosition
    {
        /// <summary>
        /// The Text Document
        /// </summary>
        public TextDocumentIdentifier textDocument { get; set; }

        /// <summary>
        /// Legacy property to support protocol version 1.0 requests.
        /// </summary>
        public string uri {get; set;}

        /// <summary>
        /// The position inside the text document.
        /// </summary>
        public Position position { get; set; }

        /// <summary>
        /// Creates a new TextDocumentPosition
        /// @param uri The document's uri.
        /// @param position The position inside the document.
        /// </summary>
        public TextDocumentPosition(TextDocumentIdentifier textDocument, string uri, Position position)
        {
            this.textDocument = textDocument;
            this.uri = uri;
            this.position = position;
        }
    }
}
