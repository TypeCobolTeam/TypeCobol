/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The parameters send in a open text document notification
    /// </summary>
    public class DidOpenTextDocumentParams : TextDocumentIdentifier
    {
        /// <summary>
        /// The content of the opened  text document.
        /// </summary>
        public string text { get; set; }

        public DidOpenTextDocumentParams(string uri) : base(uri) { }
    }
}
