/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The document close notification is sent from the client to the server when
    /// the document got closed in the client.The document's truth now exists
    /// where the document's uri points to (e.g. if the document's uri is a file uri
    /// the truth now exists on disk).
    /// </summary>
    class DidCloseTextDocumentNotification
    {
        public static readonly NotificationType Type = new NotificationType("textDocument/didClose", typeof(DidCloseTextDocumentParams));
    }
}
