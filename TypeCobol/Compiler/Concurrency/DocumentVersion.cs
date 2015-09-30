// Inspired by ICSharpCode.AvalonEdit.Document.ITextSourceVersion
// https://github.com/icsharpcode/AvalonEdit/tree/master/ICSharpCode.AvalonEdit/Document
// Copyright (c) 2014 AlphaSierraPapa for the SharpDevelop Team

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// Represents a version identifier for a document snapshot.
    /// Implements of a linked list of document versions tracking incremental changes.
    /// </summary> 
    /// <remarks>
    /// Versions can be used to efficiently detect whether a document has changed and needs reparsing;
    /// or to implement incremental parsers.
    /// </remarks>
    /// </summary>
    [DebuggerDisplay("Version #{id}")]
    public sealed class DocumentVersion
    {
        // Reference back to the document version provider.
        // Used to determine if two checkpoints belong to the same document.
        readonly object provider;

        // ID used for CompareAge()
        readonly int id;

        // Changes from this version to the next version
        internal IEnumerable<DocumentChange> changes;

        // Linked list of successive document versions
        internal DocumentVersion next;

        /// <summary>
        /// Initial version
        /// </summary>
        internal DocumentVersion(object provider)
        {
            this.provider = provider;
        }

        /// <summary>
        /// New version of an existing document
        /// </summary>
        internal DocumentVersion(DocumentVersion prev)
        {
            this.provider = prev.provider;
            this.id = unchecked(prev.id + 1);
        }

        /// <summary>
        /// Gets whether this checkpoint belongs to the same document as the other checkpoint.
        /// </summary>
        /// <remarks>
        /// Returns false when given <c>null</c>.
        /// </remarks>
        public bool BelongsToSameDocumentAs(DocumentVersion other)
        {
            DocumentVersion o = other as DocumentVersion;
            return o != null && provider == o.provider;
        }

        /// <summary>
        /// Compares the age of this checkpoint to the other checkpoint.
        /// </summary>
        /// <remarks>This method is thread-safe.</remarks>
        /// <exception cref="ArgumentException">Raised if 'other' belongs to a different document than this version.</exception>
        /// <returns>-1 if this version is older than <paramref name="other"/>.
        /// 0 if <c>this</c> version instance represents the same version as <paramref name="other"/>.
        /// 1 if this version is newer than <paramref name="other"/>.</returns>
        public int CompareAge(DocumentVersion other)
        {
            if (other == null)
                throw new ArgumentNullException("other");
            DocumentVersion o = other as DocumentVersion;
            if (o == null || provider != o.provider)
                throw new ArgumentException("Versions do not belong to the same document");
            // We will allow overflows, but assume that the maximum distance between checkpoints is 2^31-1.
            // This is guaranteed on x86 because so many checkpoints don't fit into memory.
            return Math.Sign(unchecked(this.id - o.id));
        }
       
        /// <summary>
        /// Gets the changes from this checkpoint to the other checkpoint.
        /// If 'other' is older than this checkpoint, an exception is thrown.
        /// </summary>
        /// <remarks>This method is thread-safe.</remarks>
        /// <exception cref="ArgumentException">Raised if 'other' belongs to a different document than this checkpoint, or 'other' is older than this checkpoint.</exception>
        public IEnumerable<DocumentChange> GetChangesTo(DocumentVersion other)
        {
            int result = CompareAge(other);
            if (result < 0)
                return GetForwardChanges(other);
            else if (result > 0)
                return other.GetForwardChanges(this).Reverse().Select(change => change.Invert());
            else
                return NO_CHANGES;
        }

        private static DocumentChange[] NO_CHANGES = new DocumentChange[] { };
        
        /*
        /// <summary>
        /// 
        /// </summary>
        /// <param name="other"></param>
        /// <param name="oldOffset"></param>
        /// <param name="movement"></param>
        public int FindLineIndexInOtherVersion(ITextSourceVersion other, int oldOffset)
        {
            int offset = oldOffset;
            foreach (var e in GetChangesTo(other))
            {
                offset = e.GetNewOffset(offset, movement);
            }
            return offset;
        }
        */

        private IEnumerable<DocumentChange> GetForwardChanges(DocumentVersion other)
        {
            // Return changes from this(inclusive) to other(exclusive).
            IList<DocumentChange> result = new List<DocumentChange>();
            for (DocumentVersion node = this; node != other; node = node.next)
            {
                foreach (DocumentChange change in node.changes)
                {
                    result.Add(change);
                }
            }
            return result;
        }
    }
}
