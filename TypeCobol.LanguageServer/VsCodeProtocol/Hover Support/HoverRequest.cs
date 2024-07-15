/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Request to request hover information at a given text document position. The request's
    /// parameter is of type[TextDocumentPosition](#TextDocumentPosition) the response is of
    /// type[Hover](#Hover) or a Thenable that resolves to such.
    /// </summary>
    class HoverRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/hover", typeof(TextDocumentPositionParams), typeof(Hover), null);
    }
}
