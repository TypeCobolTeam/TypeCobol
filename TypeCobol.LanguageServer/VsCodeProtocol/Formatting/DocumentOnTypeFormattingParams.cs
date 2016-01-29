/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    public class DocumentOnTypeFormattingParams
    {
        /// <summary>
        /// The document to format.
        /// </summary>
        public TextDocumentIdentifier textDocument { get; set; }

        /// <summary>
        /// The position at which this request was send.
        /// </summary>
        public Position position { get; set; }

        /// <summary>
        /// The character that has been typed.
        /// </summary>
        public string ch { get; set; }

        /// <summary>
        /// The format options.
        /// </summary>
        public FormattingOptions options { get; set; }
    }
}
