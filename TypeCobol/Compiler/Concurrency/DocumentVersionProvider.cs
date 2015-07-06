// Inspired by ICSharpCode.AvalonEdit.Document.TextSourceVersionProvider
// https://github.com/icsharpcode/AvalonEdit/tree/master/ICSharpCode.AvalonEdit/Document
// Copyright (c) 2014 AlphaSierraPapa for the SharpDevelop Team

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reactive.Subjects;

namespace TypeCobol.Compiler.Concurrency
{   
    /// <summary>
    /// Base class used to implement immutable snapshots and versioning on top of a mutable document.
    /// The document changes are triggered by InputChangeType events.
    /// The changes applied to the document are signaled by DocumentChangeType events.
    /// </summary>
    public abstract class DocumentVersionProvider<InputChangeType,DocumentChangeType> : IObserver<InputChangeType>
    {
        /// <summary>
        /// Creates a new DocumentVersionProvider instance.
        /// </summary>
        public DocumentVersionProvider()
        {
            // First version
            currentVersion = new Version<DocumentChangeType>(this);

            // !! Constructors of derived classes MUST initialize an empty document snapshot !!
        }

        // --- Update the document content in response to input changes ---

        /// <summary>
        /// Replaces the current version with a new version.
        /// </summary>
        /// <param name="change">Change from current version to new version</param>
        public void OnNext(InputChangeType inputChange)
        {
            try
            {
                if (inputChange == null)
                    throw new ArgumentNullException("inputChange");

                DocumentChangeType documentChange;
                lock (this)
                {
                    // Compute the changes produced in the document by the changes signaled in the input
                    documentChange = ApplyInputChange(inputChange);

                    // Track these changes and increment the document version
                    currentVersion.change = documentChange;
                    currentVersion.next = new Version<DocumentChangeType>(currentVersion);
                    currentVersion = currentVersion.next;

                    // Capture a new document snapshot, consistent with the new version id
                    currentSnapshot = CreateNewDocumentSnapshot(currentVersion);
                }

                // Inform all the listeners of the changes applied in the document
                documentChangedEventsSource.OnNext(documentChange);
            }
            catch(Exception e)
            {
                // Remember that an exception occured
                LastExceptionOnInputChange = new Tuple<InputChangeType, Exception>(inputChange, e);

                // In case something goes wrong, notify all listeners
                documentChangedEventsSource.OnError(e);
            }
        }

        /// <summary>
        /// Not null if an unexpected exception occured while handling a previous input change signaled to the document
        /// </summary>
        public Tuple<InputChangeType,Exception> LastExceptionOnInputChange { get; private set; }

        /// <summary>
        /// Applies an input change on the document, and compute the changes produced on the document
        /// </summary>
        protected abstract DocumentChangeType ApplyInputChange(InputChangeType inputChange);

        /// <summary>
        /// Errors signaled by the source of the input changes must be handled in the derived class
        /// </summary>
        public abstract void OnError(Exception error);

        /// <summary>
        /// Completion signaled by the source of the input changes : do nothing, a versioned document always stays mutable
        /// </summary>
        public void OnCompleted()
        { }

        // --- Track document changes in versions ---

        // Current document version : accessible only via a document snapshot
        private Version<DocumentChangeType> currentVersion;

        /// <summary>
        /// Implementation of a linked list of document versions
        /// </summary>
        [DebuggerDisplay("Version #{id}")]
        sealed class Version<DocumentChangeType> : IDocumentVersion<DocumentChangeType>
        {
            // Reference back to the provider.
            // Used to determine if two checkpoints belong to the same document.
            readonly object provider;
            // ID used for CompareAge()
            readonly int id;

            // the change from this version to the next version
            internal DocumentChangeType change;
            internal Version<DocumentChangeType> next;

            internal Version(object provider)
            {
                this.provider = provider;
            }

            internal Version(Version<DocumentChangeType> prev)
            {
                this.provider = prev.provider;
                this.id = unchecked(prev.id + 1);
            }

            public bool BelongsToSameDocumentAs(IDocumentVersion<DocumentChangeType> other)
            {
                Version<DocumentChangeType> o = other as Version<DocumentChangeType>;
                return o != null && provider == o.provider;
            }

            public int CompareAge(IDocumentVersion<DocumentChangeType> other)
            {
                if (other == null)
                    throw new ArgumentNullException("other");
                Version<DocumentChangeType> o = other as Version<DocumentChangeType>;
                if (o == null || provider != o.provider)
                    throw new ArgumentException("Versions do not belong to the same document");
                // We will allow overflows, but assume that the maximum distance between checkpoints is 2^31-1.
                // This is guaranteed on x86 because so many checkpoints don't fit into memory.
                return Math.Sign(unchecked(this.id - o.id));
            }

            public IEnumerable<DocumentChangeType> GetChangesTo(IDocumentVersion<DocumentChangeType> other)
            {
                int result = CompareAge(other);
                Version<DocumentChangeType> o = (Version<DocumentChangeType>)other;
                if (result < 0)
                    return GetForwardChanges(o);
                else if (result > 0)
                    throw new ArgumentException("'other' version is older than this checkpoint");
                else
                    return null;
            }

            IEnumerable<DocumentChangeType> GetForwardChanges(Version<DocumentChangeType> other)
            {
                // Return changes from this(inclusive) to other(exclusive).
                for (Version<DocumentChangeType> node = this; node != other; node = node.next)
                {
                    yield return node.change;
                }
            }
        }

        // --- Provide an immutable document snapshot for each version ---

        // Current document snapshot
        private IDocumentSnapshot<DocumentChangeType> currentSnapshot;

        /// <summary>
        /// Gets an immutable document snapshot, which can then be used in any thread.
        /// </summary>
        public IDocumentSnapshot<DocumentChangeType> GetSnapshot()
        {
            return currentSnapshot;
        }

        /// <summary>
        /// Create a new document snapshot from the current internal state of the document, with a new version id 
        /// </summary>
        protected abstract IDocumentSnapshot<DocumentChangeType> CreateNewDocumentSnapshot(IDocumentVersion<DocumentChangeType> newVersion);

        // --- Publish the document changes to all interested listeners ---

        // Broadcast document changes to all listeners
        private ISubject<DocumentChangeType> documentChangedEventsSource = new Subject<DocumentChangeType>();

        /// <summary>
        /// Subscribe to this events source to be notified of all changes int this document
        /// </summary>
        public IObservable<DocumentChangeType> DocumentChangedEventsSource
        {
            get { return documentChangedEventsSource; }
        }
    }
}
