/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The change text document notification's parameters.
    /// </summary>
    public class DidChangeTextDocumentParams
    {
        public VersionedTextDocumentIdentifier textDocument { get; set; }

        public TextDocumentContentChangeEvent[] contentChanges { get; set; }
    }
}
