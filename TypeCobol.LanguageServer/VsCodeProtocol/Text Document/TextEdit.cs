/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A text edit applicable to a text document.
    /// </summary>
    public class TextEdit
    {
        /// <summary>
        /// The range of the text document to be manipulated. To insert
        /// text into a document create a range where start === end.
        /// </summary>
        public Range range { get; set; }

        /// <summary>
        /// The string to be inserted. For delete operations use an
        /// empty string.
        /// </summary>
        public string newText { get; set; }

        /// <summary>
        /// Creates a replace text edit.
        /// @param range The range of text to be replaced.
        /// @param newText The new text.
        /// </summary>
        public static TextEdit Replace(Position start, Position end, string newText)
        {
            var range = new Range() { start = start, end = end };
            return new TextEdit() { range = range, newText = newText };
        }

        /// <summary>
        /// Creates a insert text edit.
        /// @param psotion The position to insert the text at.
        /// @param newText The text to be inserted.
        /// </summary>
        public static TextEdit Insert(Position position, string newText)
        {
            Range range = new Range() { start = position, end = position };
            return new TextEdit() { range = range, newText = newText };
        }

        /// <summary>
        /// Creates a delete text edit.
        /// @param range The range of text to be deleted.
        /// </summary>
        public static TextEdit Del(Range range)
        {
            return new TextEdit() { range = range, newText = string.Empty };
        }
    }
}
