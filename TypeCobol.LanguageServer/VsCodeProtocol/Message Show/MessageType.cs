/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The message type
    /// </summary>
    enum MessageType
    {
        /// <summary>
        /// An error message.
        /// </summary>
        Error = 1,
        /// <summary>
        /// A warning message.
        /// </summary>
        Warning = 2,
        /// <summary>
        /// An information message.
        /// </summary>
        Info = 3,
        /// <summary>
        /// A log message.
        /// </summary>
        Log = 4
    }
}
