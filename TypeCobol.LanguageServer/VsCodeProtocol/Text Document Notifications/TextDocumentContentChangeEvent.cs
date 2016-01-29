/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// An event describing a change to a text document. If range and rangeLength are omitted
    /// the new text is considered to be the full content of the document.
    /// </summary>
    public class TextDocumentContentChangeEvent
    {
        /// <summary>
        /// The range of the document that changed.
        /// </summary>
        public Range range { get; set; }

        /// <summary>
        /// The length of the range that got replaced.
        /// </summary>
        public int? rangeLength { get; set; }

        /// <summary>
        /// The new text of the document.
        /// </summary>
        public string text { get; set; }
    }
}
