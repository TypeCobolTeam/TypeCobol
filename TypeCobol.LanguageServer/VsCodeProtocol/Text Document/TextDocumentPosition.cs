/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A literal to define the position in a text document.
    /// </summary>
    public class TextDocumentPosition : TextDocumentIdentifier
    {
        /// <summary>
        /// The position inside the text document.
        /// </summary>
        public Position position { get; set; }

        /// <summary>
        /// Creates a new TextDocumentPosition
        /// @param uri The document's uri.
        /// @param position The position inside the document.
        /// </summary>
        public TextDocumentPosition(string uri, Position position) : base(uri)
        {
            this.position = position;
        }
    }
}
