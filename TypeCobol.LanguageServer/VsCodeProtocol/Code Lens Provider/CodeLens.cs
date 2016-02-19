/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// A code lens represents a [command](#Command) that should be shown along with
    /// source text, like the number of references, a way to run tests, etc.
    /// 
    /// A code lens is _unresolved_ when no command is associated to it.For performance
    /// reasons the creation of a code lens and resolving should be done to two stages.
    /// </summary>
    public class CodeLens
    {
        /// <summary>
        /// The range in which this code lens is valid. Should only span a single line.
        /// </summary>
        public Range range { get; set; }

        /// <summary>
        /// The command this code lens represents.
        /// </summary>
        public Command command { get; set; }

        /// <summary>
        /// An data entry field that is preserved on a code lens item between
        /// a [CodeLensRequest](#CodeLensRequest) and a [CodeLensResolveRequest]
        /// (#CodeLensResolveRequest)
        /// </summary>
        public object data { get; set; }

        /// <summary>
        ///Creates a new CodeLens literal.
        /// </summary>
        public CodeLens(Range range, object data)
        {
            this.range = range;
            this.data = data;
        }
    }
}
