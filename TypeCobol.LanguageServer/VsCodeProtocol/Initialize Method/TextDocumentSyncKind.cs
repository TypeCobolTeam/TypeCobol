/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Defines how the host (editor) should sync
    /// document changes to the language server.
    /// </summary>
    public enum TextDocumentSyncKind
    {
        /// <summary>
        /// Documents should not be synced at all.
        /// </summary>
        None = 0,

        /// <summary>
        /// Documents are synced by always sending the full content
        /// of the document.
        /// </summary>
        Full = 1,

        /// <summary>
        /// Documents are synced by sending the full content on open.
        /// After that only incremental updates to the document are
        /// send.
        /// </summary>
        Incremental = 2
    }
}
