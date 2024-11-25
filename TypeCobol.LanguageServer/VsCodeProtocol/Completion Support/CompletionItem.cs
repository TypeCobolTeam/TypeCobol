/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A completion item represents a text snippet that is
    /// proposed to complete text that is being typed.
    /// </summary>
    public class CompletionItem
    {
        /// <summary>
        /// The label of this completion item. By default
        /// also the text that is inserted when selecting
        /// this completion.
        /// </summary>
        public string label { get; set; }

        /// <summary>
        /// The kind of this completion item. Based of the kind
        /// an icon is chosen by the editor.
        /// </summary>
        public CompletionItemKind kind { get; set; }

        /// <summary>
        /// A human-readable string with additional information
        /// about this item, like type or symbol information.
        /// </summary>
        public string detail { get; set; }

        /// <summary>
        /// A human-readable string that represents a doc-comment.
        /// </summary>
        public string documentation { get; set; }

        /// <summary>
        /// A string that shoud be used when comparing this item
        /// with other items. When `falsy` the [label](#CompletionItem.label)
        /// is used.
        /// </summary>
        public string sortText { get; set; }

        /// <summary>
        /// A string that should be used when filtering a set of
        /// completion items. When `falsy` the [label](#CompletionItem.label)
        /// is used.
        /// </summary>
        public string filterText { get; set; }

        /// <summary>
        /// A string that should be inserted a document when selecting
        /// this completion. When `falsy` the [label](#CompletionItem.label)
        /// is used.
        /// </summary>
        public string insertText { get; set; }

        /// <summary>
        /// An [edit](#TextEdit) which is applied to a document when selecting
        /// this completion. When an edit is provided the value of
        /// [insertText](#CompletionItem.insertText) is ignored.
        /// </summary>
        public TextEdit textEdit { get; set; }

        /// <summary>
        /// An data entry field that is preserved on a completion item between
        /// a [CompletionRequest](#CompletionRequest) and a [CompletionResolveRequest]
        /// (#CompletionResolveRequest)
        /// </summary>
        public object data { get; set; }
    }
}
