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
        /// </summary>
        /// <param name="start">The start position of text to be replaced.</param>
        /// <param name="end">The end position of text to be replaced.</param>
        /// <param name="newText">The replacement text.</param>
        /// <returns>Non-null instance of TextEdit.</returns>
        public static TextEdit Replace(Position start, Position end, string newText) => Replace(new Range() { start = start, end = end }, newText);

        /// <summary>
        /// Creates a replace text edit.
        /// </summary>
        /// <param name="range">The range of text to be replaced.</param>
        /// <param name="newText">The replacement text.</param>
        /// <returns>Non-null instance of TextEdit.</returns>
        public static TextEdit Replace(Range range, string newText) => new TextEdit() { range = range, newText = newText };

        /// <summary>
        /// Creates an insert text edit.
        /// </summary>
        /// <param name="position">The position to insert the text at.</param>
        /// <param name="newText">The text to be inserted.</param>
        /// <returns>Non-null instance of TextEdit.</returns>
        public static TextEdit Insert(Position position, string newText)
        {
            Range range = new Range() { start = position, end = position };
            return new TextEdit() { range = range, newText = newText };
        }

        /// <summary>
        /// Creates a delete text edit.
        /// </summary>
        /// <param name="range">The range of text to be deleted.</param>
        /// <returns>Non-null instance of TextEdit.</returns>
        public static TextEdit Delete(Range range)
        {
            return new TextEdit() { range = range, newText = string.Empty };
        }
    }
}
