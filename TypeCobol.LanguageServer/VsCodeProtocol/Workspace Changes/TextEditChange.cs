/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A change to capture text edits for existing resources.
    /// </summary>
    interface TextEditChange
    {
        /// <summary>
        /// Gets all text edits for this change.
        ///
        /// @return An array of text edits.
        /// </summary>
        System.Collections.Generic.IList<TextEdit> all();

        /// <summary>
        /// Clears the edits for this change.
        /// </summary>
        void clear();

        /// <summary>
        /// Insert the given text at the given position.
        ///
        /// @param position A position.
        /// @param newText A string.
        /// </summary>
        void insert(Position position, string newText);

        /// <summary>
        /// Replace the given range with given text for the given resource.
        ///
        /// @param range A range.
        /// @param newText A string.
        /// </summary>
        void replace(Range range, string newTex);

        /// <summary>
        /// Delete the text at the given range.
        ///
        /// @param range A range.
        /// </summary>
        void delete(Range range);
    }
}
