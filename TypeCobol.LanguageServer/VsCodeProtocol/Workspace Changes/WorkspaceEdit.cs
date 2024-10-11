/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A workspace edit represents changes to many resources managed
    /// in the workspace.
    /// </summary>
    public class WorkspaceEdit
    {
        /// <summary>
        /// Holds changes to existing resources.
        /// </summary>
        public IDictionary<string, IList<TextEdit>> changes { get; set; } // [uri: string]: TextEdit[]
    }
}
