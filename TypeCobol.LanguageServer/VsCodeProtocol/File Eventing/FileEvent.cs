/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// An event describing a file change.
    /// </summary>
    public class FileEvent
    {
        /// <summary>
        /// The file's uri.
        /// </summary>
        public string uri { get; set; }

        /// <summary>
        /// The change type.
        /// </summary>
        public FileChangeType type { get; set; }
    }
}
