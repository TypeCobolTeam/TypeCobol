/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The document open notification is sent from the client to the server to signal
    /// newly opened text documents.The document's truth is now managed by the client
    /// and the server must not try to read the document's truth using the document's
    /// uri.
    /// </summary>
    class DidOpenTextDocumentNotification
    {
        public static readonly NotificationType Type = new NotificationType("textDocument/didOpen", typeof(DidOpenTextDocumentParams));
    }
}
