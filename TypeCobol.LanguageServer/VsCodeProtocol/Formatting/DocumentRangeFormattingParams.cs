/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    public class DocumentRangeFormattingParams
    {
        /// <summary>
        /// The document to format.
        /// </summary>
        TextDocumentIdentifier textDocument { get; set; }

        /// <summary>
        /// The range to format
        /// </summary>
        Range range { get; set; }

        /// <summary>
        /// The format options
        /// </summary>
        FormattingOptions options { get; set; }
    }
}
