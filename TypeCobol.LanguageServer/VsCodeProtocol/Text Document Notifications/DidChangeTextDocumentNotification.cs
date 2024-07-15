/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The document change notification is sent from the client to the server to signal
    /// changes to a text document.
    /// </summary>
    class DidChangeTextDocumentNotification
    {
        public static readonly NotificationType Type = new NotificationType("textDocument/didChange", typeof(DidChangeTextDocumentParams));
    }
}
