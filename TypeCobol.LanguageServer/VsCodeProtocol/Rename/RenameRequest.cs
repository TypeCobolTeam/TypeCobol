/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A request to rename a symbol.
    /// </summary>
    class RenameRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/rename", typeof(RenameParams), typeof(WorkspaceEdit), null);
    }
}
