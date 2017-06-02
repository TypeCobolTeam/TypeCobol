/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Parameters for a [ReferencesRequest](#ReferencesRequest).
    /// </summary>
    public class ReferenceParams : TextDocumentPosition
    {
        public ReferenceContext context { get; set; }

        public ReferenceParams(TextDocumentIdentifier textDocument, string uri, Position position) : base(textDocument, uri, position) { }
    }
}
