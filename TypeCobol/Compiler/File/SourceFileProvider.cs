using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using JetBrains.Annotations;

namespace TypeCobol.Compiler.File
{
    /// <summary>
    /// This class enables the compiler to find Cobol source files referenced by name in the Cobol syntax
    /// </summary>
    public class SourceFileProvider
    {
        /// <summary>
        /// If text-name is not qualified, a library-name of SYSLIB is assumed.
        /// </summary>
        public string DEFAULT_LIBRARY_NAME = "SYSLIB";

        /// <summary>
        /// Set of Cobol libraries where text files will be searched by name during the compilation process
        /// </summary>
        public IList<ICobolLibrary> CobolLibraries { get; private set; }

        /// <summary>
        /// Use the Add methods to extend the set of Cobol source libraries
        /// </summary>
        public SourceFileProvider()
        {
            CobolLibraries = new List<ICobolLibrary>();
        }
        
        /// <summary>
        /// Add all Cobol files contained in a local directory, filtered by a set of extensions, to the default text library
        /// </summary>
        /// <param name="rootPath">Local directory containing all the Cobol files of this library</param>
        /// <param name="includeSubdirectories">Does this library also includes the files contained in all the subdirectories found below the root path ?</param>
        /// <param name="fileExtensions">File extensions which should be optionnaly appended to the text name to find corresponding Cobol files (for example : { ".cbl",".cpy" })</param>
        /// <param name="documentFormat">For of the document (RDZ, FreeFormat)</param>
        public LocalDirectoryLibrary AddLocalDirectoryLibrary(string rootPath, bool includeSubdirectories, string[] fileExtensions, [NotNull] DocumentFormat documentFormat)
        {
            return AddLocalDirectoryLibrary(DEFAULT_LIBRARY_NAME, rootPath, includeSubdirectories, fileExtensions, documentFormat.Encoding, documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength);
        }
        
        /// <summary>
        /// Add all Cobol files contained in a local directory, filtered by a set of extensions, to the default text library
        /// </summary>
        /// <param name="rootPath">Local directory containing all the Cobol files of this library</param>
        /// <param name="includeSubdirectories">Does this library also includes the files contained in all the subdirectories found below the root path ?</param>
        /// <param name="fileExtensions">File extensions which should be optionnaly appended to the text name to find corresponding Cobol files (for example : { ".cbl",".cpy" })</param>
        /// <param name="encoding">Character encoding used to read and write all the files contained in this library (use IBMCodePages.GetDotNetEncodingFromIBMCCSID if necessary)</param>
        /// <param name="endOfLineDelimiter">Type of line delimiter which is used in the files of this library</param>
        /// <param name="fixedLineLength">If the files of this library all have a fixed line length : number of chars found on each line (for example : 80)</param>        
        public LocalDirectoryLibrary AddLocalDirectoryLibrary(string rootPath, bool includeSubdirectories, string[] fileExtensions, Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength)
        {
            return AddLocalDirectoryLibrary(DEFAULT_LIBRARY_NAME, rootPath, includeSubdirectories, fileExtensions, encoding, endOfLineDelimiter, fixedLineLength);
        }

        /// <summary>
        /// Add all Cobol files contained in a local directory, filtered by a set of extensions, to a specific text library named : libraryName
        /// </summary>
        /// <param name="libraryName">Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)</param>
        public LocalDirectoryLibrary AddLocalDirectoryLibrary(string libraryName, string rootPath, bool includeSubdirectories, string[] fileExtensions, Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength)
        {
            LocalDirectoryLibrary localLibrary = new LocalDirectoryLibrary(libraryName, rootPath, includeSubdirectories, fileExtensions, encoding, endOfLineDelimiter, fixedLineLength);
            AddCobolLibrary(localLibrary);
            return localLibrary;
        }

        /// <summary>
        /// Generic method to add any type of Cobol library to the source set
        /// </summary>
        public void AddCobolLibrary(ICobolLibrary cobolLibrary)
        {
            CobolLibraries.Add(cobolLibrary);
        }

        /// <summary>
        /// Returns true if the default library contains a file for the specified textName (COPY textName)
        /// </summary>
        public bool ContainsFile(string textName)
        {
            return ContainsFile(DEFAULT_LIBRARY_NAME, textName);
        }

        /// <summary>
        /// Returns true if a library named librayName contains a file for the specified textName (COPY textName OF libraryName)
        /// </summary>
        public bool ContainsFile(string libraryName, string textName)
        {
            foreach(ICobolLibrary cobolLibrary in CobolLibraries)
            {
                if (cobolLibrary.Name == libraryName && cobolLibrary.ContainsFile(textName))
                {
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Returns an object representing a local or remote Cobol file 
        /// if the default library contains a file for the specified textName 
        /// (COPY textName)
        /// </summary>
        public bool TryGetFile(string textName, out CobolFile cobolFile)
        {
            return TryGetFile(DEFAULT_LIBRARY_NAME, textName, out cobolFile);
        }

        /// <summary>
        /// Returns an object representing a local or remote Cobol file 
        /// if a library named librayName contains a file for the specified textName 
        /// (COPY textName OF libraryName)
        /// </summary>
        public bool TryGetFile(string libraryName, string textName, out CobolFile cobolFile)
        {
            if (libraryName == null) libraryName = DEFAULT_LIBRARY_NAME;
            foreach (ICobolLibrary cobolLibrary in CobolLibraries)
            {
                if (cobolLibrary.Name == libraryName && cobolLibrary.TryGetFile(textName, out cobolFile))
                {
                    return true;
                }
            }
            cobolFile = null;
            return false;
        }
    }
}
