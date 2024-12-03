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
        public static Range FromPositions(int lineStart, int characterStart, int lineEnd, int characterEnd)
        {
            var start = new Position() { line = lineStart, character = characterStart };
            var end = new Position() { line = lineEnd, character = characterEnd };
            return new Range() { start = start, end = end };
        }

        /// <summary>
        /// The range's start position
        /// </summary>
        public Position start { get; set; }

        /// <summary>
        /// The range's end position
        /// </summary>
        public Position end { get; set; }
    }
}
