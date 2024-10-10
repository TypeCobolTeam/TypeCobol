/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A shutdown request is sent from the client to the server.
    /// It is send once when the client descides to shutdown the
    /// server.The only notification that is sent after a shudown request
    /// is the exit event.
    /// </summary>
    class ShutdownRequest
    {
        public static readonly RequestType Type = new RequestType("shutdown", null, null, null);
    }
}
