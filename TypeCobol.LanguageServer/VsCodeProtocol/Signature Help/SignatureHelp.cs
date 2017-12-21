/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Signature help represents the signature of something
    /// callable.There can be multiple signature but only one
    /// active and only one active parameter.
    /// </summary>
    public class SignatureHelp
    {
        /// <summary>
        /// One or more signatures.
        /// </summary>
        public SignatureInformation[]  signatures { get; set; }

        /// <summary>
        /// The active signature.
        /// </summary>
        public int? activeSignature { get; set; }

        /// <summary>
        /// The active parameter of the active signature.
        /// </summary>
        public int? activeParameter { get; set; }
    }
}
