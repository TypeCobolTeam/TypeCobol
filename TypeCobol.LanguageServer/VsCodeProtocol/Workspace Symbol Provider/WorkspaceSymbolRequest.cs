/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using System.Collections.Generic;
using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A request to list project-wide symbols matching the query string given
    /// by the[WorkspaceSymbolParams](#WorkspaceSymbolParams). The response is
    /// of type[SymbolInformation[]](#SymbolInformation) or a Thenable that
    /// resolves to such.
    /// </summary>
    class WorkspaceSymbolRequest
    {
        public static readonly RequestType Type = new RequestType("workspace/symbol", typeof(WorkspaceSymbolParams), typeof(List<SymbolInformation>), null);
    }
}
