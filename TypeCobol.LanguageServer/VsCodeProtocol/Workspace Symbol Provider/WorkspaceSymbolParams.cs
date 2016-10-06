/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The parameters of a [WorkspaceSymbolRequest](#WorkspaceSymbolRequest).
    /// </summary>
    public class WorkspaceSymbolParams
    {
        /// <summary>
        /// A non-empty query string
        /// </summary>
        public string query { get; set; }
    }
}
