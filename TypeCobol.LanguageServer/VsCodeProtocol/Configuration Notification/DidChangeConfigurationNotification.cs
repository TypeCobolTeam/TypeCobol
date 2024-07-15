/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The configuration change notification is sent from the client to the server
    /// when the client's configuration has changed. The notification contains
    /// the changed configuration as defined by the language client.
    /// </summary>
    class DidChangeConfigurationNotification
    {
        public static readonly NotificationType Type = new NotificationType("workspace/didChangeConfiguration", typeof(DidChangeConfigurationParams));
    }
}
