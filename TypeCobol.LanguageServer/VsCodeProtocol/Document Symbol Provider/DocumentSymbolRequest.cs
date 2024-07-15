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
    /// A request to list all symbols found in a given text document. The request's
    /// parameter is of type[TextDocumentIdentifier](#TextDocumentIdentifier) the
    /// response is of type[SymbolInformation[]](#SymbolInformation) or a Thenable
    /// that resolves to such.
    /// </summary>
    class DocumentSymbolRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/documentSymbol", typeof(TextDocumentIdentifier), typeof(List<SymbolInformation>), null);
    }
}
