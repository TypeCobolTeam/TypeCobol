/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using System.Collections.Generic;
using TypeCobol.LanguageServer.JsonRPC;
using Microsoft.VisualStudio.LanguageServer.Protocol;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A request to provide commands for the given text document and range.
    /// </summary>
    class CodeActionRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/codeAction", typeof(CodeActionParams), typeof(List<Command>), null);
    }
}
