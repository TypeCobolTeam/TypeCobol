/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using System.Collections.Generic;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A workspace change helps constructing changes to a workspace.
    /// </summary>
    class WorkspaceChange
    {
        private WorkspaceEdit workspaceEdit;
        private IDictionary<string, TextEditChange> textEditChanges; // [uri: string]: TextEditChange

        public WorkspaceChange()
        {
            this.workspaceEdit = new WorkspaceEdit();
            this.textEditChanges = new Dictionary<string, TextEditChange>();
        }

        /// <summary>
        /// Returns the underlying [WorkspaceEdit](#WorkspaceEdit) literal
        /// use to be returned from a workspace edit operation like rename.
        /// </summary>
        public WorkspaceEdit edit
        {
            get
            {
                return this.workspaceEdit;
            }
        }

        /// <summary>
        /// Returns the [TextEditChange](#TextEditChange) to manage text edits
        /// for resources.
        /// </summary>
        public TextEditChange getTextEditChange(string uri)
        {
            TextEditChange result;
            if (!this.textEditChanges.TryGetValue(uri, out result))
            {
                var edits = new List<TextEdit>();

                this.workspaceEdit.changes[uri] = edits;

                result = new TextEditChangeImpl(edits);
                this.textEditChanges[uri] = result;
            }
            return result;
        }

        private class TextEditChangeImpl : TextEditChange
        {
            private IList<TextEdit> edits;

            public TextEditChangeImpl(IList<TextEdit> edits)
            {
                this.edits = edits;
            }

            public void insert(Position position, string newText)
            {
                this.edits.Add(TextEdit.insert(position, newText));
            }

            public void replace(Range range, string newText)
            {
                this.edits.Add(TextEdit.replace(range, newText));
            }

            public void delete(Range range)
            {
                this.edits.Add(TextEdit.del(range));
            }

            public IList<TextEdit> all()
            {
                return this.edits;
            }

            public void clear()
            {
                this.edits.Clear();
            }
        }
    }
}
