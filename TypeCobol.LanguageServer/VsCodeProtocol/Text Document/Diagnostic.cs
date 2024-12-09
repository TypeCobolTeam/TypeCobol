/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The diagnostic's severity.
    /// </summary>
    public enum DiagnosticSeverity
    {
        /// <summary>
         /// Reports an error.
         /// </summary>
        Error = 1,
        /// <summary>
         /// Reports a warning.
         /// </summary>
        Warning = 2,
        /// <summary>
         /// Reports an information.
         /// </summary>
        Information = 3,
        /// <summary>
         /// Reports a hint.
         /// </summary>
        Hint = 4
    }

    /// <summary>
    /// Represents a diagnostic, such as a compiler error or warning. Diagnostic objects
    /// are only valid in the scope of a resource.
    /// </summary>
    public class Diagnostic
    {
        /// <summary>
        /// The range at which the message applies
        /// </summary>
        public Range range { get; set; }

        /// <summary>
        /// The diagnostic's severity. Can be omitted. If omitted it is up to the
        /// client to interpret diagnostics as error, warning, info or hint.
        /// </summary>
        public DiagnosticSeverity severity { get; set; } // int?

        /// <summary>
        /// The diagnostic's code. Can be omitted.
        /// </summary>
        public string code { get; set; } //  int | string

        /// <summary>
        /// A human-readable string describing the source of this
        /// diagnostic, e.g. 'typescript' or 'super lint'.
        /// </summary>
        public string source { get; set; }

        /// <summary>
        /// The diagnostic's message.
        /// </summary>
        public string message { get; set; }
    }
}
