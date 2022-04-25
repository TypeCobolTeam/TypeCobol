using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.File
{
    /// <summary>
    /// Implementation of a Cobol text library as a local directory containing Cobol files
    /// </summary>
    public class LocalDirectoryLibrary : ICobolLibrary
    {

        // Local directory properties

        public DirectoryInfo RootDirectory { get; }
        private string _rootPath;
        private bool includeSubdirectories;
        private string[] fileExtensions;

        // Library file format properties

        private Encoding encoding;
        private EndOfLineDelimiter endOfLineDelimiter;
        private int fixedLineLength;

        /// <summary>
        /// Initializes a Cobol text library from files contained in a root directory
        /// </summary>
        /// <param name="libraryName">Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)</param>
        /// <param name="rootPath">Local directory containing all the Cobol files of this library</param>
        /// <param name="includeSubdirectories">Does this library also includes the files contained in all the subdirectories found below the root path ?</param>
        /// <param name="fileExtensions">File extensions which should be optionnaly appended to the text name to find corresponding Cobol files (for example : { ".cbl",".cpy" })</param>
        /// <param name="encoding">Character encoding used to read and write all the files contained in this library (use IBMCodePages.GetDotNetEncodingFromIBMCCSID if necessary)</param>
        /// <param name="endOfLineDelimiter">Type of line delimiter which is used in the files of this library</param>
        /// <param name="fixedLineLength">If the files of this library all have a fixed line length : number of chars found on each line (for example : 80)</param>
        /// <exception cref="ArgumentException"></exception>
        public LocalDirectoryLibrary(string libraryName, string rootPath, bool includeSubdirectories, string[] fileExtensions, Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength)
        {
            Name = libraryName;

            this._rootPath = rootPath;
            RootDirectory = new DirectoryInfo(rootPath);
            if (!RootDirectory.Exists)
            {
                throw new ArgumentException($"Local copy library {rootPath} does not exist on disk.");
            }
            this.includeSubdirectories = includeSubdirectories;
            if (fileExtensions != null)
            {
                foreach (var fileExtension in fileExtensions)
                {
                    if (fileExtension[0] != '.')
                    {
                        throw new ArgumentException(string.Format("File extension must start with a '.' : {0}", fileExtension));
                    }
                }
            }
            this.fileExtensions = fileExtensions;

            this.encoding = encoding;
            this.endOfLineDelimiter = endOfLineDelimiter;
            this.fixedLineLength = fixedLineLength;
        }

        /// <summary>
        /// Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// Returns true if the library contains a file for the specified text name (COPY textName OF libraryName)
        /// </summary>
        public bool ContainsFile(string textName)
        {
            return FindFirstMatchingFilePath(textName) != null;
        }

        /// <summary>
        /// Returns an object representing a local or remote Cobol file
        /// </summary>
        public bool TryGetFile(string textName, out CobolFile cobolFile)
        {
            string absoluteFilePath = FindFirstMatchingFilePath(textName);
            if (absoluteFilePath != null)
            {
                cobolFile = new LocalCobolFile(textName, absoluteFilePath, encoding, endOfLineDelimiter, fixedLineLength);
                return true;
            }
            else
            {
                cobolFile = null;
                return false;
            }
        }

        /// <summary>
        /// Returns the full path of the first file matching textName with one of the given extensions, below the root directory.
        /// Or returns null if no matching file is found.
        /// </summary>
        /// <param name="textName">Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)</param>
        private string FindFirstMatchingFilePath(string textName)
        {
            //If file name already contains a "." => apply search first without any extension and then with all extensions filtering through given values
            if (textName.Contains("."))
            {
                return FindFirstMatchingFilePathWithoutExtensionsFirst(textName);
            }

            //Otherwise first search using given extensions first and then without
            return FindFirstMatchingFilePathWithExtensionsFirst(textName);
        }

        /// <summary>
        /// Returns the full path of the first file matching textName with one of the given extensions, below the root directory.
        /// First searches without any extension added to file name and then, if extensions are given in fileExtensions, search and filter all possible extensions added to file name through them 
        /// If no result found return null
        /// </summary>
        /// <param name="textName">Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)</param>
        private string FindFirstMatchingFilePathWithoutExtensionsFirst(string textName)
        {
            //First search without any extension
            FileInfo candidateFile = SearchForFileWithoutExtensions(textName);

            if (candidateFile != null)
            {
                return candidateFile.FullName;
            }

            //Search using given extensions
            if (fileExtensions != null)
            {
                return SearchForFileWithExtensions(textName)?.FullName;
            }

            //No result found then return null
            return null;
        }

        /// <summary>
        /// Returns the full path of the first file matching textName with one of the given extensions, below the root directory.
        /// First search all files matching all possible extensions filtered through given extensions in fileExtensions, if no result is found search without any extension
        /// If no result found return null
        /// </summary>
        /// <param name="textName">Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)</param>
        private string FindFirstMatchingFilePathWithExtensionsFirst(string textName)
        {
            //Search using given extensions
            if (fileExtensions != null)
            {
                FileInfo candidateFile = SearchForFileWithExtensions(textName);

                //Return this result only if not null, otherwise proceed to next search instruction
                if (candidateFile != null)
                {
                    return candidateFile.FullName;
                }
            }

            //If previous search wasn't conclusive, search without any extension, and return null if no result retrieved
            return SearchForFileWithoutExtensions(textName)?.FullName;
        }

        /// <summary>
        /// Return result from search in Root directory without any extension added
        /// </summary>
        /// <param name="textName">Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)</param>
        public FileInfo SearchForFileWithoutExtensions(string textName)
        {
            return RootDirectory.EnumerateFiles(textName,
                    includeSubdirectories ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly)
                .FirstOrDefault();
        }

        /// <summary>
        /// Return result from search in Root directory filtering all possible extensions added to file name through given extensions in fileExtensions
        /// </summary>
        /// <param name="textName">Name of the Cobol library, as specified in a Cobol program (COPY textName OF libraryName)</param>
        public FileInfo SearchForFileWithExtensions(string textName)
        {
            return RootDirectory.EnumerateFiles($"{textName}.*", includeSubdirectories ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly)
                .FirstOrDefault(f =>
                    fileExtensions.Any(suffix =>
                        f.Extension.Equals(suffix, StringComparison.OrdinalIgnoreCase)));
        }

        /// <summary>
        /// Returns an new empty file created in the library 
        /// </summary>
        public CobolFile CreateNewFile(string textName, string fullPath)
        {
            FileInfo newLocalFile = new FileInfo(fullPath);
            if (newLocalFile.Exists)
            {
                throw new InvalidOperationException(String.Format("The local library {0} already contains a Cobol file named {1} at path {2} : impossible to create a new one with the same name and path", Name, textName, fullPath));
            }
            else
            {
                newLocalFile.Create();
                return new LocalCobolFile(textName, fullPath, encoding, endOfLineDelimiter, fixedLineLength);
            }
        }

        /// <summary>
        /// Removes an existing file from the library
        /// </summary>
        public void RemoveFile(string textName, string fullPath)
        {
            FileInfo localFile = new FileInfo(fullPath);
            if (!localFile.Exists)
            {
                throw new InvalidOperationException(String.Format("The local library {0} does not contain a Cobol file named {1} at path {2} : impossible to delete", Name, textName, fullPath));
            }
            else
            {
                localFile.Delete();
            }
        }

        // -- Implementation of file changes monitoring based on FileSystemWatcher --

        FileSystemWatcher fileSystemWatcher;
        IDictionary<string, LocalCobolFile> filesToMonitor;

        internal void StartMonitoringCobolFile(LocalCobolFile localCobolFile)
        {
            // Register the full path of a file to monitor
            if (filesToMonitor == null)
            {
                filesToMonitor = new Dictionary<string, LocalCobolFile>();
            }
            filesToMonitor.Add(localCobolFile.FullPath, localCobolFile);

            // Lazy initialization of FileSystemWatcher
            if (fileSystemWatcher == null)
            {
                fileSystemWatcher = new FileSystemWatcher(RootDirectory.FullName);
                fileSystemWatcher.IncludeSubdirectories = includeSubdirectories;
                fileSystemWatcher.Changed += fileSystemWatcher_Changed;
                fileSystemWatcher.Renamed += fileSystemWatcher_Renamed;
                fileSystemWatcher.Deleted += fileSystemWatcher_Deleted;
                fileSystemWatcher.EnableRaisingEvents = true;
            }
        }

        internal void StopMonitoringCobolFile(LocalCobolFile localCobolFile)
        {
            // Unregister a file to monitor
            if (filesToMonitor != null && filesToMonitor.ContainsKey(localCobolFile.FullPath))
            {
                filesToMonitor.Remove(localCobolFile.FullPath);
            }
        }

        void fileSystemWatcher_Deleted(object sender, FileSystemEventArgs e)
        {
            LocalCobolFile localCobolFile;
            if (filesToMonitor.TryGetValue(e.FullPath, out localCobolFile))
            {
                CobolFileChangedEvent deletedEvent = new CobolFileChangedEvent(CobolFileChangeType.FileDeleted, DateTime.Now, null);
                localCobolFile.RaiseCobolFileChanged(deletedEvent);
            }
        }

        void fileSystemWatcher_Renamed(object sender, RenamedEventArgs e)
        {
            LocalCobolFile localCobolFile;
            if (filesToMonitor.TryGetValue(e.FullPath, out localCobolFile))
            {
                CobolFileChangedEvent renamedEvent = new CobolFileChangedEvent(CobolFileChangeType.FileRenamed, DateTime.Now, e.FullPath);
                localCobolFile.RaiseCobolFileChanged(renamedEvent);
            }
        }

        void fileSystemWatcher_Changed(object sender, FileSystemEventArgs e)
        {
            LocalCobolFile localCobolFile;
            if (filesToMonitor.TryGetValue(e.FullPath, out localCobolFile))
            {
                CobolFileChangedEvent changedEvent = new CobolFileChangedEvent(CobolFileChangeType.FileChanged, DateTime.Now, null);
                localCobolFile.RaiseCobolFileChanged(changedEvent);
            }
        }

        /// <summary>
        /// Free all ressources acquired by this library
        /// </summary>
        public void Dispose()
        {
            fileSystemWatcher.Dispose();
            fileSystemWatcher = null;
        }
    }
}
