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
    /// A request to resolve project-wide references for the symbol denoted
    /// by the given text document position.The request's parameter is of
    /// type[ReferenceParams](#ReferenceParams) the response is of type
    /// [Location[]](#Location) or a Thenable that resolves to such.
    /// </summary>
    class ReferencesRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/references", typeof(ReferenceParams), typeof(List<Location>), null);
    }
}
