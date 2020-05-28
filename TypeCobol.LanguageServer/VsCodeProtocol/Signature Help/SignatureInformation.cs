/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

using System;
using System.Collections.Generic;
using System.Linq;

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Represents the signature of something callable. A signature
    /// can have a label, like a function-name, a doc-comment, and
    /// a set of parameters.
    /// </summary>
    public class SignatureInformation : IEquatable<SignatureInformation>
    {
        /// <summary>
        /// The label of this signature. Will be shown in
        /// the UI.
        /// </summary>
        public string label { get; }

        /// <summary>
        /// The human-readable doc-comment of this signature. Will be shown
        /// in the UI but can be omitted.
        /// </summary>
        public string documentation { get; }

        /// <summary>
        /// The parameters of this signature.
        /// </summary>
        public ParameterInformation[] parameters { get; }

        public SignatureInformation(string label, string documentation, params ParameterInformation[] parameters)
        {
            this.label = label;
            this.documentation = documentation;
            this.parameters = parameters;
	    }

        public override bool Equals(object obj)
        {
            return Equals(obj as SignatureInformation);
        }

        public bool Equals(SignatureInformation signatureInformation)
        {
            if (Object.ReferenceEquals(this, signatureInformation)) return true;
            if (Object.ReferenceEquals(null, signatureInformation)) return false;

            return signatureInformation.label == this.label
                   && signatureInformation.documentation == this.documentation
                   && CompareLists(this.parameters.ToList(), signatureInformation.parameters.ToList());
        }

        public override int GetHashCode()
        {
            unchecked
            {
                var hashCode = 13;
                hashCode = (hashCode * 397) ^ label.GetHashCode();
                var docHashCode = documentation?.GetHashCode() ?? 0;
                hashCode = (hashCode * 397) ^ docHashCode;
                if (parameters != null)
                {
                    foreach (var param in parameters)
                    {
                        hashCode = (hashCode * 397) ^ param.label.GetHashCode();
                        hashCode = (hashCode * 397) ^ (param.documentation?.GetHashCode() ?? 0);
                    }
                }

                return hashCode;
            }
        }

        private static bool CompareLists(List<ParameterInformation> list1, List<ParameterInformation> list2)
        {

            if (list1.Count != list2.Count)
                return false;

            for (int i = 0; i < list1.Count; i++)
            {
                if (list1[i].label != list2[i].label || list1[i].documentation != list2[i].documentation)
                    return false;
            }

            return true;
        }
    }
}
