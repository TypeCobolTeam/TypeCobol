/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The log message notification is send from the server to the client to ask
    /// the client to log a particular message.
    /// </summary>
    class LogMessageNotification
    {
        public static readonly NotificationType Type = new NotificationType("window/logMessage", typeof(LogMessageParams));
    }
}
