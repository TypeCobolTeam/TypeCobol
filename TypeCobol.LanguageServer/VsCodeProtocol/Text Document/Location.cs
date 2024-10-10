/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

namespace TypeCobol.LanguageServer.VsCodeProtocol
{
    /// <summary>
    /// Represents a location inside a resource, such as a line
    /// inside a text file.
    /// </summary>
    public class Location
    {
        public string uri { get; set; }

        public Range range { get; set; }
    }
}
