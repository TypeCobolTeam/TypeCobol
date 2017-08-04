using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// Document changes are tracked at the line level.
    /// This class models a change which can span several lines of a document.
    /// </summary>
    public class DocumentChangedEvent<T> : EventArgs
    {
        public DocumentChangedEvent(DocumentVersion<T> before, DocumentVersion<T> after)
        {
            DocumentVersionBefore = before;
            DocumentVersionAfter = after;
        }

        /// <summary>
        /// Version of the document before this change event
        /// </summary>
        public DocumentVersion<T> DocumentVersionBefore { get; private set; }

        /// <summary>
        /// Version of the document after this change event
        /// </summary>
        public DocumentVersion<T> DocumentVersionAfter { get; private set; }

        /// <summary>
        /// List of document changes signaled by this event
        /// </summary>
        public IEnumerable<DocumentChange<T>> DocumentChanges
        {
            get
            {
                return DocumentVersionBefore.GetChangesTo(DocumentVersionAfter);
            }
        }
    }
}
