/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The publish diagnostic notification's parameters.
    /// </summary>
    public class PublishDiagnosticsParams
    {
        /// <summary>
        /// The URI for which diagnostic information is reported.
        /// </summary>
        public string uri { get; set; }

        /// <summary>
        /// An array of diagnostic information items.
        /// </summary>
        public Diagnostic[] diagnostics { get; set; }
    }
}
