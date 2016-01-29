/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The result of a hover request.
    /// </summary>
    public class Hover
    {
        /// <summary>
        /// The hover's content
        /// </summary>
        public MarkedString[] contents { get; set; } // MarkedString | MarkedString[]

        /// <summary>
        /// An optional range
        /// </summary>
        public Range range { get; set; }
    }
}
