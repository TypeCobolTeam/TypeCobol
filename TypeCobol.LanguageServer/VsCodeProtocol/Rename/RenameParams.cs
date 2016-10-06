/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    public class RenameParams
    {
        /// <summary>
        /// The document to format.
        /// </summary>
        public TextDocumentIdentifier textDocument { get; set; }

        /// <summary>
        /// The position at which this request was send.
        /// </summary>
        public Position position { get; set; }

        /// <summary>
        /// The new name of the symbol. If the given name is not valid the
        /// request must return a [ResponseError](#ResponseError) with an
        /// appropriate message set.
        /// </summary>
        public string newName { get; set; }
    }
}
