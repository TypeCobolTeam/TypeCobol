using System;
using System.IO;

namespace TypeCobol.Compiler.File
{
    /// <summary>
    /// Common interface for Cobol text libraries (dictionary of files) : local filesystem directory, remote zOs PDS, source control repository
    /// </summary>
    public interface ICobolLibrary : IDisposable
    {
        /// <summary>
        /// Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)
        /// </summary>
        string Name { get; }

        /// <summary>
        /// Returns true if the library contains a file for the specified text name (COPY textName OF libraryName)
        /// </summary>
        bool ContainsFile(string textName);

        /// <summary>
        /// Returns an object representing a local or remote Cobol file
        /// </summary>
        bool TryGetFile(string textName, out CobolFile cobolFile);

        /// <summary>
        /// Returns an new empty file created in the library 
        /// </summary>
        CobolFile CreateNewFile(string textName, string fullPath);

        /// <summary>
        /// Removes an existing file from the library
        /// </summary>
        void RemoveFile(string textName, string fullPath);
    }
}
