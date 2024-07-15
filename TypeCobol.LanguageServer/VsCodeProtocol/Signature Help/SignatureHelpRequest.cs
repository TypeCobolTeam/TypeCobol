/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using Microsoft.VisualStudio.LanguageServer.Protocol;
using TypeCobol.LanguageServer.JsonRPC;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Signature help represents the signature of something
    /// callable. There can be multiple signature but only one
    /// active and only one active parameter.
    /// </summary>
    class SignatureHelpRequest
    {
        public static readonly RequestType Type = new RequestType("textDocument/signatureHelp", typeof(TextDocumentPositionParams), typeof(SignatureHelp), null);
    }
}
