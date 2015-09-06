using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// Document changes are tracked at the line level.
    /// This class models a change which can span several lines of a document.
    /// </summary>
    public class DocumentChangedEvent
    {
        public DocumentChangedEvent(DocumentVersion before, DocumentVersion after)
        {
            DocumentVersionBefore = before;
            DocumentVersionAfter = after;
        }

        /// <summary>
        /// Version of the document before this change event
        /// </summary>
        public DocumentVersion DocumentVersionBefore { get; private set; }

        /// <summary>
        /// Version of the document after this change event
        /// </summary>
        public DocumentVersion DocumentVersionAfter { get; private set; }

        /// <summary>
        /// List of document changes signaled by this event
        /// </summary>
        public IEnumerable<DocumentChange> DocumentChanges
        {
            get
            {
                return DocumentVersionBefore.GetChangesTo(DocumentVersionAfter);
            }
        }
    }
}
