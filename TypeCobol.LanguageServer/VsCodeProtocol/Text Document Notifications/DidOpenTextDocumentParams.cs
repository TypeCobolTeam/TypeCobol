/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The parameters send in a open text document notification
    /// </summary>
    public class DidOpenTextDocumentParams
    {
        /// <summary>
        /// The document that was opened.
        /// </summary>
        public TextDocumentItem textDocument;

        /// <summary>
        /// Legacy property to support protocol version 1.0 requests.
        /// </summary>
        public string text { get; set; }
    }
}
