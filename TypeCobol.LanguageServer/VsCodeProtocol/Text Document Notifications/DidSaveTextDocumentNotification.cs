/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The document save notification is sent from the client to the server when the document got saved in the client.
    /// </summary>
    class DidSaveTextDocumentNotification
    {
        public static readonly NotificationType Type = new NotificationType("textDocument/didSave", typeof(DidSaveTextDocumentParams));
    }
}
