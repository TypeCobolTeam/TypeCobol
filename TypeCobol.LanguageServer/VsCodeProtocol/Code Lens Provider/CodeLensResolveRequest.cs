/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using TypeCobol.LanguageServer.JsonRPC;
using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A request to resolve a command for a given code lens.
    /// </summary>
    class CodeLensResolveRequest
    {
        public static readonly RequestType Type = new RequestType("codeLens/resolve", typeof(CodeLens), typeof(CodeLens), null);
    }
}
