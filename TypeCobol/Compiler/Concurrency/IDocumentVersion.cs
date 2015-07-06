// Inspired by ICSharpCode.AvalonEdit.Document.ITextSourceVersion
// https://github.com/icsharpcode/AvalonEdit/tree/master/ICSharpCode.AvalonEdit/Document
// Copyright (c) 2014 AlphaSierraPapa for the SharpDevelop Team

using System;
using System.Collections.Generic;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// Represents a version identifier for a document.
    /// </summary> 
    /// <remarks>
    /// Versions can be used to efficiently detect whether a document has changed and needs reparsing;
    /// or to implement incremental parsers.
    /// </remarks>
    public interface IDocumentVersion<DocumentChangeType>
    {
        /// <summary>
        /// Gets whether this checkpoint belongs to the same document as the other checkpoint.
        /// </summary>
        /// <remarks>
        /// Returns false when given <c>null</c>.
        /// </remarks>
        bool BelongsToSameDocumentAs(IDocumentVersion<DocumentChangeType> other);

        /// <summary>
        /// Compares the age of this checkpoint to the other checkpoint.
        /// </summary>
        /// <remarks>This method is thread-safe.</remarks>
        /// <exception cref="ArgumentException">Raised if 'other' belongs to a different document than this version.</exception>
        /// <returns>-1 if this version is older than <paramref name="other"/>.
        /// 0 if <c>this</c> version instance represents the same version as <paramref name="other"/>.
        /// 1 if this version is newer than <paramref name="other"/>.</returns>
        int CompareAge(IDocumentVersion<DocumentChangeType> other);

        /// <summary>
        /// Gets the changes from this checkpoint to the other checkpoint.
        /// If 'other' is older than this checkpoint, an exception is thrown.
        /// </summary>
        /// <remarks>This method is thread-safe.</remarks>
        /// <exception cref="ArgumentException">Raised if 'other' belongs to a different document than this checkpoint, or 'other' is older than this checkpoint.</exception>
        IEnumerable<DocumentChangeType> GetChangesTo(IDocumentVersion<DocumentChangeType> other);
    }
}
