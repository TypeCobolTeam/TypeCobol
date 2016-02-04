/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Value-object that contains additional information when
    /// requesting references.
    /// </summary>
    public class ReferenceContext
    {
        /// <summary>
        /// Include the declaration of the current symbol.
        /// </summary>
        public bool includeDeclaration { get; set; }
    }
}
