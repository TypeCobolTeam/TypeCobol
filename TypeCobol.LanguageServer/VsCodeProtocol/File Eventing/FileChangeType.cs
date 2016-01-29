/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The file event type
    /// </summary>
    public enum FileChangeType
    {
        /// <summary>
        // The file got created.
        /// </summary>
        Created = 1,
        /// <summary>
        // The file got changed.
        /// </summary>
        Changed = 2,
        /// <summary>
        // The file got deleted.
        /// </summary>
        Deleted = 3
    }
}
