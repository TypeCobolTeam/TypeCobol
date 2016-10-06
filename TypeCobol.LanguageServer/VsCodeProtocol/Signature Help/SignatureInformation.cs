/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Represents the signature of something callable. A signature
    /// can have a label, like a function-name, a doc-comment, and
    /// a set of parameters.
    /// </summary>
    public class SignatureInformation
    {
        /// <summary>
        /// The label of this signature. Will be shown in
        /// the UI.
        /// </summary>
        public string label { get; set; }

        /// <summary>
        /// The human-readable doc-comment of this signature. Will be shown
        /// in the UI but can be omitted.
        /// </summary>
        public string documentation { get; set; }

        /// <summary>
        /// The parameters of this signature.
        /// </summary>
        public ParameterInformation[] parameters { get; set; }

        public SignatureInformation(string label, string documentation, params ParameterInformation[] parameters)
        {
            this.label = label;
            this.documentation = documentation;
            this.parameters = parameters;
	    }
    }
}
