/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// The definition of a symbol represented as one or many [locations](#Location).
    /// For most programming languages there is only one location at which a symbol is
    /// defined.
    /// </summary>
    public class Definition : Location
    {
        //export type Definition = Location | Location[];

        public Definition(string uri, Range range)
        {
            this.uri = uri;
            this.range = range;
        }
    }
}
