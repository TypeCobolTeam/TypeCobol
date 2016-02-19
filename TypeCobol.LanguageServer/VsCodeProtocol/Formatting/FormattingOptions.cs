/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Value-object describing what options formatting should use.
    /// </summary>
    public class FormattingOptions
    {
        /// <summary>
        /// Size of a tab in spaces.
        /// </summary>
        public int tabSize { get; set; }

        /// <summary>
        /// Prefer spaces over tabs.
        /// </summary>
        public bool insertSpaces { get; set; }

        /// <summary>
        /// Signature for further properties.
        /// </summary>
        public string key { get; set; } // boolean | int | string

        /// <summary>
        /// Creates a new FormattingOptions literal.
        /// </summary>
        public FormattingOptions(int tabSize, bool insertSpaces)
        {
            this.tabSize = tabSize;
            this.insertSpaces = insertSpaces;
        }
    }
}
