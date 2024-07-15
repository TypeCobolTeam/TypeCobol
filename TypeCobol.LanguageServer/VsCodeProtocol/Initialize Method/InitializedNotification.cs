/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The initialized notification is sent from the client to the server
    /// after the client received the result of the initialize request
    /// but before the client is sending any other request or notification to the server.
    /// The server can use the initialized notification for example to dynamically
    /// register capabilities. The initialized notification may only be sent once.
    /// </summary>
    class InitializedNotification
    {
        public static readonly NotificationType Type = new NotificationType("initialized", typeof(InitializedParams));
    }
}
