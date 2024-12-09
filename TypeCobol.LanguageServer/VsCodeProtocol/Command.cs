/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Represents a reference to a command. Provides a title which
    /// will be used to represent a command in the UI and, optionally,
    /// an array of arguments which will be passed to the command handler
    /// function when invoked.
    /// </summary>
    public class Command
    {
        /// <summary>
        /// Title of the command, like `save`.
        /// </summary>
        public string title { get; set; }
        /// <summary>
        /// The identifier of the actual command handler.
        /// </summary>
        public string command { get; set; }
        /// <summary>
        /// Arguments that the command handler should be
        /// invoked with.
        /// </summary>
        public object[] arguments { get; set; }
    }
}
