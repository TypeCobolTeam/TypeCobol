/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using System.Collections.Generic;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A request to provide code lens for the given text document.
    /// </summary>
    class CodeLensRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/codeLens", typeof(TextDocumentIdentifier), typeof(List<CodeLens>), null);
    }
}
