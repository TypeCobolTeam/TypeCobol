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
    /// Request to resolve a [DocumentHighlight](#DocumentHighlight) for a given
    /// text document position.The request's parameter is of type [TextDocumentPosition]
    /// (#TextDocumentPosition) the request reponse is of type [DocumentHighlight[]]
    /// (#DocumentHighlight) or a Thenable that resolves to such.
    /// </summary>
    class DocumentHighlightRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/documentHighlight", typeof(TextDocumentPositionParams), typeof(List<DocumentHighlight>), null);
    }
}
