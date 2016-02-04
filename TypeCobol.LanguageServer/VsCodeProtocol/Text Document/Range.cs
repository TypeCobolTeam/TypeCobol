/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A range in a text document expressed as (zero-based) start and end positions.
    /// </summary>
    public class Range
    {
        /// <summary>
        /// The range's start position
        /// </summary>
        public Position start { get; set; }

        /// <summary>
        /// The range's end position
        /// </summary>
        public Position end { get; set; }

        public Range() { }

        /// <summary>
        /// Create a new Range liternal.
        /// @param start The range's start position.
        /// @param end The range's end position.
        /// </summary>
        public Range(Position start, Position end)
        {
            this.start = start;
            this.end = end;
        }

        /// <summary>
        /// Create a new Range literal.
        /// @param startLine The start line number.
        /// @param startCharacter The start character.
        /// @param endLine The end line number.
        /// @param endCharacter The end character.
        /// </summary>
        public Range(int startLine, int startCharacter, int endLine, int endCharacter)
        {
            this.start = new Position(startLine, startCharacter);
            this.end = new Position(endLine, endCharacter);
        }
    }
}
