/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Position in a text document expressed as zero-based line and character offset.
    /// </summary>
    public class Position
    {
        /// <summary>
        /// Line position in a document (zero-based).
        /// </summary>
        public int line { get; set; }

        /// <summary>
        /// Character offset on a line in a document (zero-based).
        /// </summary>
        public int character { get; set; }
    }
}
