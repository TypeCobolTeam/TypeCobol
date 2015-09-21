using System;

namespace TypeCobol.Compiler.Concurrency
{
    /// <summary>
    /// A document line can be reused simultaneously in different versions of the document.
    /// Thus you can NOT get a line number from an isolated reference to a document line.
    /// The line number is only defined in a specific snapshot of the document :
    /// - pass the IDocumentLine object to the IndexOf method on the list of lines in a document snapshot 
    ///   (WARNING : expensive O(n) operation !)
    /// or in the original text source (for example a text editor accessed in the specific thread where it lives) :
    /// - pass the property TextSourceLineReference to a dedicated method of the text source 
    ///   (much less expensive O(log n) operation)
    /// </summary>
    public interface IDocumentLine
    {
        /// <summary>
        /// Opaque reference to an object from the original text source which will enable 
        /// an efficient retrieval of the line number for this line in the live document 
        /// </summary>
        object TextSourceLineReference { get; }
    }
}
