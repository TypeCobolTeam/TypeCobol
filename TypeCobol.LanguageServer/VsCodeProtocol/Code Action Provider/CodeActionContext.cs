/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Contains additional diagnostic information about the context in which
    /// a[code action](#CodeActionProvider.provideCodeActions) is run.
    /// </summary>
    public class CodeActionContext
    {
        /// <summary>
        /// An array of diagnostics.
        /// </summary>
        public Diagnostic[] diagnostics { get; set; }

        /// <summary>
        /// Creates a new CodeActionContext literal.
        /// </summary>
        public CodeActionContext(Diagnostic[] diagnostics)
        {
            this.diagnostics = diagnostics;
        }
    }
}