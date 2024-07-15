/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A request to resolve the defintion location of a symbol at a given text
    /// document position.The request's parameter is of type [TextDocumentPosition]
    /// (#TextDocumentPosition) the response is of type [Definition](#Definition) or a
    /// Thenable that resolves to such.
    /// </summary>
    class DefinitionRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/definition", typeof(TextDocumentPositionParams), typeof(Location), null);
    }
}
