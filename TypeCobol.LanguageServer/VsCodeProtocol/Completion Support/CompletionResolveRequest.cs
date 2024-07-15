/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Request to resolve additional information for a given completion item.The request's
    /// parameter is of type[CompletionItem](#CompletionItem) the response
    /// is of type[CompletionItem](#CompletionItem) or a Thenable that resolves to such.
    /// </summary>
    class CompletionResolveRequest
    {
        public static readonly RequestType Type = new RequestType("completionItem/resolve", typeof(CompletionItem), typeof(CompletionItem), null);
    }
}
