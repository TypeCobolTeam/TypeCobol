using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Text
{
    /// <summary>
    /// Interface enabling the integration of the Cobol compiler with any kind of text editor.
    /// A document is both :
    /// - an array of characters, which can be accessed by offset from the beginning of the document
    /// - a list of text lines, which can be accessed by their index in the list
    /// A document sends notifications each time one of its lines is changed. 
    /// </summary>
    public interface ITextDocument
    {
        /// <summary>
        /// Document source name and text format
        /// </summary>
        TextSourceInfo Source { get; }

        // -- Document chars --

        /// <summary>
        /// Reloads the text document with new chars
        /// </summary>
        void LoadChars(IEnumerable<char> textSource);

        /// <summary>
        /// Iterator over the document chars
        /// (without line delimiters)
        /// </summary>
        IEnumerable<char> Chars { get; }

        /// <summary>
        /// Access the document like a char array
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        char CharAt(int offset);

        /// <summary>
        /// Part of the text between start offset and end offset (included), 
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        string TextSegment(int startOffset, int endOffset);

        /// <summary>
        /// Total number of chars in the document, excluding line delimiters
        /// </summary>
        int Length { get; }

        // -- Document lines --
                
        /// <summary>
        /// Iterator over the document lines
        /// </summary>
        IEnumerable<ITextLine> Lines { get; }

        /// <summary>
        /// Gets the document line with the specified index
        /// (first line at index 0)
        /// </summary>
        ITextLine GetLineByIndex(int lineIndex);

        /// <summary>
        /// Gets the line containing the character at a specific offset in the document
        /// (first char at index 0 at the beginning of document)
        /// </summary>
        ITextLine GetLineByOffset(int offset, out int indexOfCharInLine);

        /// <summary>
        /// Total number of lines in the document
        /// </summary>
        int LineCount { get; }

        // -- Document changes --

        /// <summary>
        /// A TextChangedEvent is sent to all observers each time a line 
        /// is inserted, updated, or removed in the document
        /// </summary>
        IObservable<TextChangedEvent> TextChangedEventsSource { get; }

        /// <summary>
        /// Call this method only after all observers have been chained to form the compilation pipeline
        /// </summary>
        void StartSendingChangeEvents();       
    }
}
