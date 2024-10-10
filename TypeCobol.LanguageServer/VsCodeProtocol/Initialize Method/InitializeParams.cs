/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The initialize parameters
    /// </summary>
    public class InitializeParams
    {
        /// <summary>
        /// The process Id of the parent process that started
        /// the server.
        /// </summary>
        public int processId { get; set; }

        /// <summary>
        /// The rootPath of the workspace. Is null
        /// if no folder is open.
        /// </summary>
        public string rootPath { get; set; }

        /// <summary>
        /// The capabilities provided by the client (editor)
        /// </summary>
        public ClientCapabilities capabilities { get; set; }

        /// <summary>
        /// The initial trace setting. If omitted trace is disabled ('off').
        /// </summary>
        public string trace { get; set; }
    }
}
