/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The parameters of a change configuration notification.
    /// </summary>
    public class DidChangeConfigurationParams
    {
        /// <summary>
        /// The actual changed settings
        /// </summary>
        public object settings { get; set; }
    }
}
