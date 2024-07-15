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
    /// A request to to format a range in a document.
    /// </summary>
    class DocumentRangeFormattingRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/rangeFormatting", typeof(DocumentRangeFormattingParams), typeof(List<TextEdit>), null);
    }
}
