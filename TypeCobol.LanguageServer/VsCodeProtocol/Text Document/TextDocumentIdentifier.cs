/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A literal to identify a text document in the client.
    /// </summary>
    public class TextDocumentIdentifier
    {
        public static TextDocumentIdentifier BuildFrom(Uri uri)
        {
            // Get the original URI (which was set by the client)
            // DON'T use ToString() as it returns the canonically unescaped form of the URI
            // (it may cause issue if the path contains some blanks which need to be escaped)
            return new TextDocumentIdentifier() { uri = uri.OriginalString };
        }

        /// <summary>
        /// The text document's uri.
        /// </summary>
        public string uri { get; set; }
    }
}
