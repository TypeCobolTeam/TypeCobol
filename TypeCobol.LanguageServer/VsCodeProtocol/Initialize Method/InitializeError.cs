/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The error returned if the initialize request fails.
    /// </summary>
    public class InitializeError
    {
        /// <summary>
        /// Indicates whether the client should retry to send the
        /// initilize request after showing the message provided
        /// in the {@link ResponseError}
        /// </summary>
        public bool retry { get; set; }
    }
}
