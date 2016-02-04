/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The watched files change notification's parameters.
    /// </summary>
    public class DidChangeWatchedFilesParams
    {
        /// <summary>
        /// The actual file events.
        /// </summary>
        public FileEvent[] changes { get; set; }
    }
}
