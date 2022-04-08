using System;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Analysis;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Preprocessor;
using TypeCobol.Compiler.Scanner;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Compiler
{
    /// <summary>
    /// Collection of linked Cobol files grouped to be compiled together
    /// </summary>
    public class CompilationProject : IDocumentImporter
    {
        // -- Project creation and persistence --

        /// <summary>
        /// Create a new Cobol compilation project in a local directory
        /// </summary>
        public CompilationProject(string projectName, string rootDirectory, string[] fileExtensions, DocumentFormat documentFormat, TypeCobolOptions compilationOptions, IAnalyzerProvider analyzerProvider)
        {
            Name = projectName;
            RootDirectory = rootDirectory;
            SourceFileProvider = new SourceFileProvider();

            Format = documentFormat;
            CompilationOptions = compilationOptions;
            AnalyzerProvider = analyzerProvider;

            rootDirectoryLibrary = SourceFileProvider.AddLocalDirectoryLibrary(rootDirectory, false, fileExtensions, Format.Encoding, Format.EndOfLineDelimiter, Format.FixedLineLength);
            CobolFiles = new Dictionary<string, CobolFile>();
            CobolTextReferences = new Dictionary<string, CobolFile>();
            CobolProgramCalls = new Dictionary<string, CobolFile>();

            _copyCache = new CompilationDocumentCache();
        }

        /// <summary>
        /// Save project configuration to an XML file in the project root directory
        /// </summary>
        public void Save()
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// Load project configuration from an XML file in the project root directory
        /// </summary>
        public static CompilationProject Load(string projectName, string rootDirectory)
        {
            throw new NotImplementedException();
        }

        // -- Project properties --

        /// <summary>
        /// Project Name (read-only)
        /// </summary>
        public string Name { get; private set; }

        /// <summary>
        /// Root directory for all project files (read-only)
        /// </summary>
        public string RootDirectory { get; private set; }

        /// <summary>
        /// Set of text libraries where all the source files referenced by this project can be found.
        /// Initialized with the project root directory and its subdirectories.
        /// Call SourceFileProvider.AddLocalDirectoryLibrary() or SourceFileProvider.AddCobolLibrary()
        /// to enable other text libraries.
        /// </summary>
        public SourceFileProvider SourceFileProvider { get; private set; }

        // Used for file creation and file import in the root directory
        ICobolLibrary rootDirectoryLibrary;

        // Default properties for all files of the project
        public DocumentFormat Format { get; }
        /// <summary>
        /// Changing the value of this property work only if we discard current FileCompilers and recreate them. 
        /// So it is a bit misleading.
        /// </summary>
        public TypeCobolOptions CompilationOptions { get;}
        /// <summary>
        /// Changing the value of this property work only if we discard current FileCompilers and recreate them. 
        /// So it is a bit misleading.
        /// </summary>
        public IAnalyzerProvider AnalyzerProvider { get;}

        // -- Files manipulation --

        /// <summary>
        /// Files added explicitly to the project by the developer (read-only)
        /// </summary>
        public IDictionary<string, CobolFile> CobolFiles { get; private set; }

        /// <summary>
        /// Create a new file and add it to the project
        /// </summary>
        public CobolFile CreateNewFile(string cobolFileName)
        {
            CobolFile cobolFile = rootDirectoryLibrary.CreateNewFile(cobolFileName, RootDirectory + "/" + cobolFileName);
            CobolFiles.Add(cobolFileName, cobolFile);
            return cobolFile;
        }

        /// <summary>
        /// Import an existing file in the project
        /// </summary>
        public CobolFile ImportExistingFile(string cobolFileName)
        {
            CobolFile cobolFile = null;
            if (rootDirectoryLibrary.TryGetFile(cobolFileName, out cobolFile))
            {
                CobolFiles.Add(cobolFileName, cobolFile);
            }
            return cobolFile;
        }

        /// <summary>
        /// Remove a file from the project
        /// </summary>
        public void RemoveFile(CobolFile cobolFile)
        {
            if (rootDirectoryLibrary.ContainsFile(cobolFile.Name))
            {
                rootDirectoryLibrary.RemoveFile(cobolFile.Name, cobolFile.FullPath);
            }
            CobolFiles.Remove(cobolFile.Name);
        }

        /// <summary>
        /// Add a Cobol text reference found while compiling one file of the project
        /// </summary>
        public CobolFile AddCobolTextReference(string cobolTextName)
        {
            CobolFile cobolFile = null;
            if (SourceFileProvider.TryGetFile(cobolTextName, out cobolFile))
            {
                CobolTextReferences.Add(cobolTextName, cobolFile);
            }
            return cobolFile;
        }

        /// <summary>
        /// Text file referenced by at least one file in the project and automatically imported by the compiler (read-only)
        /// </summary>
        public IDictionary<string, CobolFile> CobolTextReferences { get; private set; }

        /// <summary>
        /// Add a Cobol program call found while compiling one file of the project
        /// </summary>
        public CobolFile AddCobolProgramCalls(string cobolProgramName)
        {
            CobolFile cobolFile = null;
            if (SourceFileProvider.TryGetFile(cobolProgramName, out cobolFile))
            {
                CobolTextReferences.Add(cobolProgramName, cobolFile);
            }
            return cobolFile;
        }

        /// <summary>
        /// Program external to the project and called by at least one file in the project and automatically imported by the compiler (read-only)
        /// </summary>
        public IDictionary<string, CobolFile> CobolProgramCalls { get; private set; }

        // -- Implementation of IProcessedTokensDocumentProvider interface --

        // Cache for all the compilation documents imported by COPY directives in this project
        private readonly CompilationDocumentCache _copyCache;

        /// <summary>
        /// Clear the cache of loaded COPY
        /// </summary>
        public void ClearImportedCompilationDocumentsCache()
        {
            _copyCache.Clear();
        }

        /// <summary>
        /// Returns a CompilationDocument already in cache or loads, scans and processes a new CompilationDocument
        /// </summary>
        public virtual CompilationDocument Import(string libraryName, string textName,
            MultilineScanState scanState, List<RemarksDirective.TextNameVariation> copyTextNameVariations, out PerfStatsForImportedDocument perfStats)
        {
            var stats = new PerfStatsForImportedDocument { WasRetrievedFromCache = true };
            var result = _copyCache.GetOrAddDocument(libraryName, textName, scanState, CompileCopy);
            perfStats = stats; //Local function CompileCopy can't capture out var perfStats so we have to use local var instead
            return result;

            CompilationDocument CompileCopy()
            {
#if !EUROINFO_RULES
                if (copyTextNameVariations != null)
                    copyTextNameVariations.Add(new RemarksDirective.TextNameVariation(textName));
#endif
                bool wasAlreadyInsideCopy = scanState.InsideCopy;
                scanState.InsideCopy = true;
                FileCompiler fileCompiler = new FileCompiler(libraryName, textName, Format.ColumnsLayout, true, SourceFileProvider, this, CompilationOptions, null, scanState, this, copyTextNameVariations);
                fileCompiler.CompileOnce();
                scanState.InsideCopy = wasAlreadyInsideCopy;

                stats.WasRetrievedFromCache = false;
                stats.SourceFileSearchTime = fileCompiler.SourceFileSearchTime;
                stats.SourceFileLoadTime = fileCompiler.SourceFileLoadTime;

                return fileCompiler.CompilationResultsForCopy;
            }
        }
    }
}
