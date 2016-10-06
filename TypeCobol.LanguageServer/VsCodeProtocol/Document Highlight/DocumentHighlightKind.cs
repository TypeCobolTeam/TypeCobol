/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A document highlight kind.
    /// </summary>
    public enum DocumentHighlightKind
    {
        /// <summary>
        /// A textual occurrance.
        /// </summary>
        Text = 1,

        /// <summary>
        /// Read-access of a symbol, like reading a variable.
        /// </summary>
        Read = 2,

        /// <summary>
        /// Write-access of a symbol, like writing to a variable.
        /// </summary>
        Write = 3
    }
}
