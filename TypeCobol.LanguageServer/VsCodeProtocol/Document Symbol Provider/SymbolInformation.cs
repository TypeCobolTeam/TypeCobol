/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Represents information about programming constructs like variables, classes,
    /// interfaces etc.
    /// </summary>
    public class SymbolInformation
    {
        /// <summary>
        /// The name of this symbol.
        /// </summary>
        public string name { get; set; }

        /// <summary>
        /// The kind of this symbol.
        /// </summary>
        public SymbolKind kind { get; set; }

        /// <summary>
        /// The location of this symbol.
        /// </summary>
        public Location location { get; set; }

        /// <summary>
        /// The name of the symbol containing this symbol.
        /// </summary>
        public string containerName { get; set; }

        /// <summary>
        /// Creates a new symbol information literal.
        ///
        /// @param name The name of the symbol.
        /// @param kind The kind of the symbol.
        /// @param range The range of the location of the symbol.
        /// @param uri The resource of the location of symbol, defaults to the current document.
        /// @param containerName The name of the symbol containg the symbol.
        /// </summary>
        public SymbolInformation(string name, SymbolKind kind, Range range, string uri, string containerName)
        {
            this.name = name;
            this.kind = kind;
            this.location = new Location(uri, range);
            this.containerName = containerName;
        }
    }
}
