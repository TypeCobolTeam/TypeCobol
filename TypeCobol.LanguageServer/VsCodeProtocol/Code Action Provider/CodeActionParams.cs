/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Params for the CodeActionRequest
    /// </summary>
    public class CodeActionParams
    {
        /// <summary>
        /// The document in which the command was invoked.
        /// </summary>
        public TextDocumentIdentifier textDocument  { get; set; }

        /// <summary>
        /// The range for which the command was invoked.
        /// </summary>
        public Range range { get; set; }

        /// <summary>
        /// Context carrying additional information.
        /// </summary>
        public CodeActionContext context { get; set; }
    }
}
