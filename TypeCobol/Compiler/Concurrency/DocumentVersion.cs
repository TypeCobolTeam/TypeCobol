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
    public sealed class DocumentVersion<T>
    {
        // Reference back to the document version provider.
        // Used to determine if two checkpoints belong to the same document.
        readonly object provider;

        // ID used for CompareAge()
        readonly int id;

        // Changes from this version to the next version
        internal IEnumerable<DocumentChange<T>> changes;

        // Linked list of successive document versions
        internal DocumentVersion<T> next;

        /// <summary>
        /// True if this version is initial, False if it is a new version of an existing document
        /// </summary>
        public bool IsInitial { get; }

        /// <summary>
        /// Initial version
        /// </summary>
        internal DocumentVersion(object provider)
        {
            this.provider = provider;
            IsInitial = true;
        }

        /// <summary>
        /// New version of an existing document
        /// </summary>
        internal DocumentVersion(DocumentVersion<T> prev)
        {
            this.provider = prev.provider;
            this.id = unchecked(prev.id + 1);
            IsInitial = false;
        }

        /// <summary>
        /// Compares the age of this checkpoint to the other checkpoint.
        /// </summary>
        /// <remarks>This method is thread-safe.</remarks>
        /// <exception cref="ArgumentException">Raised if 'other' belongs to a different document than this version.</exception>
        /// <returns>-1 if this version is older than <paramref name="other"/>.
        /// 0 if <c>this</c> version instance represents the same version as <paramref name="other"/>.
        /// 1 if this version is newer than <paramref name="other"/>.</returns>
        public int CompareAge(DocumentVersion<T> other)
        {
            if (other == null)
                throw new ArgumentNullException("other");
            DocumentVersion<T> o = other as DocumentVersion<T>;
            if (o == null || provider != o.provider)
                throw new ArgumentException("Versions do not belong to the same document");
            // We will allow overflows, but assume that the maximum distance between checkpoints is 2^31-1.
            // This is guaranteed on x86 because so many checkpoints don't fit into memory.
            return Math.Sign(unchecked(this.id - o.id));
        }
        
        private static DocumentChange<T>[] NO_CHANGES = new DocumentChange<T>[] { };

        /// <summary>
        /// Gets the changes from this checkpoint to the other checkpoint.
        /// If 'other' is older than this checkpoint, an exception is thrown.
        /// </summary>
        /// <remarks>This method is thread-safe.</remarks>
        /// <exception cref="ArgumentException">Raised if 'other' belongs to a different document than this checkpoint, or 'other' is older than this checkpoint.</exception>
        public IEnumerable<DocumentChange<T>> GetChangesTo(DocumentVersion<T> other)
        {
            int result = CompareAge(other);
            if (result < 0)
                return GetForwardChanges(other);
            else if (result > 0)
                return other.GetForwardChanges(this).Reverse().Select(change => change.Invert());
            else
                return NO_CHANGES;
        }
        
        private IEnumerable<DocumentChange<T>> GetForwardChanges(DocumentVersion<T> other)
        {
            // Return changes from this(inclusive) to other(exclusive).
            for (DocumentVersion<T> node = this; node != other; node = node.next)
            {
                foreach (DocumentChange<T> change in node.changes)
                {
                    yield return change;
                }
            }
        }

        /// <summary>
        /// Get changes between older version v1 and more recent version v2 :
        /// - with redundant changes applied on the same line merged in one single change
        /// - with line indexes translated to be valid in the v2 document
        /// - with changes sorted in the order of the line indexes in the v2 document
        /// </summary>
        public IList<DocumentChange<T>> GetReducedAndOrderedChangesInNewerVersion(DocumentVersion<T> other)
        {
            // Ensure the version received as parameter is more recent than teh current version
            int result = CompareAge(other);
            if (result >= 0)
            {
                throw new InvalidOperationException("other version must be more recent than this version");
            }

            // Merge the redundant changes and translate the line indexes to the last version
            List<DocumentChange<T>> reducedDocumentChanges = new List<DocumentChange<T>>();
            foreach (DocumentChange<T> documentChange in GetChangesTo(other))
            {
                switch (documentChange.Type)
                {
                    case DocumentChangeType.DocumentCleared:
                        // Ignore all previous document changes : they are meaningless now that the document was completely cleared
                        reducedDocumentChanges.Clear();
                        // Add the clear event
                        reducedDocumentChanges.Add(documentChange);
                        break;
                    case DocumentChangeType.LineInserted:
                        // Recompute the line indexes of all the changes prevously applied
                        foreach (DocumentChange<T> documentChangeToAdjust in reducedDocumentChanges)
                        {
                            if (documentChangeToAdjust.LineIndex >= documentChange.LineIndex)
                            {
                                documentChangeToAdjust.LineIndex = documentChangeToAdjust.LineIndex + 1;
                            }
                        }
                        // Add the insert change
                        reducedDocumentChanges.Add(documentChange);
                        break;
                    case DocumentChangeType.LineUpdated:
                        // Check to see if this change can be merged with a previous one
                        bool changeAlreadyApplied = false;
                        foreach (DocumentChange<T> documentChangeToAdjust in reducedDocumentChanges)
                        {
                            if (documentChangeToAdjust.LineIndex == documentChange.LineIndex)
                            {
                                changeAlreadyApplied = true;
                                break;
                            }
                        }
                        if (!changeAlreadyApplied)
                        {
                            // Add the update change
                            reducedDocumentChanges.Add(documentChange);
                        }
                        break;
                    case DocumentChangeType.LineRemoved:
                        // Recompute the line indexes of all the changes prevously applied
                        DocumentChange<T> documentChangeToRemove = null;
                        foreach (DocumentChange<T> documentChangeToAdjust in reducedDocumentChanges)
                        {
                            if (documentChangeToAdjust.LineIndex > documentChange.LineIndex)
                            {
                                documentChangeToAdjust.LineIndex = documentChangeToAdjust.LineIndex - 1;
                            }
                            else if (documentChangeToAdjust.LineIndex == documentChange.LineIndex)
                            {
                                documentChangeToRemove = documentChangeToAdjust;
                            }
                        }
                        // Ignore all previous changes applied to a line now removed
                        if (documentChangeToRemove != null)
                        {
                            reducedDocumentChanges.Remove(documentChangeToRemove);
                        }
                        break;
                }
            }

            // Sort all changes by line index
            reducedDocumentChanges.Sort((documentChange1, documentChange2) => documentChange1.LineIndex - documentChange2.LineIndex);
            return reducedDocumentChanges;
        }
    }
}
