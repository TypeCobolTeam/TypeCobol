/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A document highlight is a range inside a text document which deserves
    /// special attention.Usually a document highlight is visualized by changing
    /// the background color of its range.
    /// </summary>
    public class DocumentHighlight
    {
        /// <summary>
        /// The range this highlight applies to.
        /// </summary>
        public Range range { get; set; }

        /// <summary>
        /// The highlight kind, default is [text](#DocumentHighlightKind.Text).
        /// </summary>
        public DocumentHighlightKind kind { get; set; }

        /// <summary>
        /// Create a DocumentHighlight object.
        /// @param range The range the highlight applies to.
        /// </summary>
        public DocumentHighlight(Range range, DocumentHighlightKind kind)
        {
            this.range = range;
            this.kind = kind;
        }
    }
}
