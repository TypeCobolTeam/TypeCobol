using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.LanguageServer.Context;
using TypeCobol.Tools.APIHelpers;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// A class that represents a Workspace project. 
    /// A Workspace Project is the association of a CompilationProject instance and a set of Document contexts.
    /// </summary>
    class WorkspaceProject
    {
        /// <summary>
        /// The Key associated to the instance of WorkspaceProject.
        /// Keys are not case sensitive.
        /// </summary>
        internal string ProjectKey { get; }

        /// <summary>
        /// Associated CompilationProject instance.
        /// </summary>
        internal CompilationProject Project { get; private set; }

        /// <summary>
        /// A Dictionary to associate an Uri of a Document to its Context.
        /// it represents all documents belonging to the associated project.
        /// </summary>
        private readonly ConcurrentDictionary<Uri, DocumentContext> _openedDocuments;

        /// <summary>
        /// Underlying parent workspace.
        /// </summary>
        private readonly Workspace _workspace;

        /// <summary>
        /// Constructor of a WorkspaceProject instance.
        /// </summary>
        /// <param name="projectKey">The Project's key</param>
        /// <param name="project">The associated CompilationProject instance</param>
        /// <param name="workspace">The owner workspace</param>
        internal WorkspaceProject(string projectKey, CompilationProject project, Workspace workspace)
        {
            this.ProjectKey = projectKey;
            this.Project = project;
            this._openedDocuments = new ConcurrentDictionary<Uri, DocumentContext>();
            this._workspace = workspace;
        }

        /// <summary>
        /// Add a new DocumentContext in this Workspace Project.
        /// </summary>
        /// <param name="documentCtx">The document Context to be added</param>
        /// <returns>true if the document has been added, false if a document with a same Uri already exists in the Project.</returns>
        internal bool AddDocument(DocumentContext documentCtx)
        {
            System.Diagnostics.Debug.Assert(documentCtx.Uri != null);
            Uri uri = documentCtx.Uri;
            return this._openedDocuments.TryAdd(uri, documentCtx);
        }

        /// <summary>
        /// Remove from this WorkspaceProject instance the given document.
        /// </summary>
        /// <param name="documentCtx">The document to be removed </param>
        /// <returns>true if the document has been removed, false if document was not in this instance.</returns>
        internal bool RemoveDocument(DocumentContext documentCtx)
        {
            System.Diagnostics.Debug.Assert(documentCtx.Uri != null);
            Uri uri = documentCtx.Uri;
            if (this._openedDocuments.TryRemove(uri, out var curDocCtx))
            {
                // Hum.. Assert: we removed a document which was not the one in this WorkspaceProject instance.
                System.Diagnostics.Debug.Assert(curDocCtx == documentCtx);
                return true;
            }
            return false;
        }

        /// <summary>
        /// Get the set of copy folders
        /// </summary>
        /// <returns>The set of copy folders</returns>
        private HashSet<string> GetCopyFolders()
        {
            // The Default Root directory of the workspace is always added to the set of copy folders.
            // So we must track the first reference to the RootDirectory that is to ignore.
            // Doing that will avoid to always have a new set of copy folders, from the client.
            bool bDefaultRootDirectoryRemoved = false;
            var result = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase);
            foreach (var cobolLibrary in Project.SourceFileProvider.CobolLibraries)
            {
                if (cobolLibrary is LocalDirectoryLibrary localDirectoryLibrary)
                {
                    if (bDefaultRootDirectoryRemoved)
                    {
                        result.Add(localDirectoryLibrary.RootDirectory.FullName);
                    }
                    else if (localDirectoryLibrary.RootDirectory.FullName.Equals(Project.RootDirectory, StringComparison.InvariantCultureIgnoreCase))
                    {   // Default RootDirectory seen and not added.                     
                        bDefaultRootDirectoryRemoved = true;
                    }
                    else
                    {
                        result.Add(localDirectoryLibrary.RootDirectory.FullName);
                    }
                }
            }

            return result;
        }

        /// <summary>
        /// Compare two Copy folders set
        /// </summary>
        /// <param name="oldCopyFolders">The Old set</param>
        /// <param name="newCopyFolders">The nes set</param>
        /// <returns></returns>
        private static bool AreCopyFoldersDifferent(HashSet<string> oldCopyFolders, List<string> newCopyFolders)
        {
            if (oldCopyFolders.Count != newCopyFolders.Count)
                return true;// The size of the list are different

            foreach (var newCopyFolder in newCopyFolders)
            {
                string fullPath = Path.GetFullPath(newCopyFolder);
                if (!oldCopyFolders.Contains(fullPath))
                    return true;
            }

            return false;
        }

        /// <summary>
        /// Configure this project
        /// </summary>
        /// <param name="format">The new Document format if any, null otherwise</param>
        /// <param name="options">The new options if any, null otherwise</param>
        /// <param name="analyzerProviderWrapper">The new Analyzer provider if any, null otherwise</param>
        /// <param name="copyFolders">The new list of copy folders associated to this project if any, null otherwise</param>
        public void Configure(DocumentFormat format, TypeCobolOptions options, AnalyzerProviderWrapper analyzerProviderWrapper, List<string> copyFolders)
        {
            bool updateFormat = false;
            var newFormat = Project.Format;
            if (format != null)
            {
                updateFormat = true;
                newFormat = format;
            }

            bool updateOptions = false;
            var newOptions = Project.CompilationOptions;
            if (options != null)
            {
                updateOptions = true;
                newOptions = options;
            }

            bool updateAnalyzerProvider = false;
            var newAnalyzerProvider = Project.AnalyzerProvider;
            if (analyzerProviderWrapper != null)
            {
                updateAnalyzerProvider = true;
                newAnalyzerProvider = analyzerProviderWrapper;
            }

            var oldCopyFolders = GetCopyFolders();
            bool updateCopyFolders = false;
            var newCopyFolders = oldCopyFolders.ToList();
            if (copyFolders != null && AreCopyFoldersDifferent(oldCopyFolders, copyFolders))
            {
                updateCopyFolders = true;
                newCopyFolders = copyFolders;
            }

            if (updateFormat || updateOptions || updateAnalyzerProvider || updateCopyFolders)
            {
                var oldCompilationProject = Project;
                var newCompilationProject = new CompilationProject(oldCompilationProject.Name, oldCompilationProject.RootDirectory, Helpers.DEFAULT_EXTENSIONS, newFormat, newOptions, newAnalyzerProvider);
                foreach (var newCopyFolder in newCopyFolders)
                {
                    newCompilationProject.SourceFileProvider.AddLocalDirectoryLibrary(newCopyFolder, false, Helpers.DEFAULT_COPY_EXTENSIONS, newFormat);
                }

                Project = newCompilationProject;
                RefreshOpenedDocuments(); //Note: we recreated the CompilationProject instance, so we always have a new empty copy cache.
            }
        }

        /// <summary>
        /// Indicates whether this workspace project has opened documents or not.
        /// </summary>
        internal bool IsEmpty => _openedDocuments.IsEmpty;

        /// <summary>
        /// Refresh all documents of this WorkspaceProject instance
        /// </summary>
        public void RefreshOpenedDocuments()
        {
            if (!IsEmpty)
            {
                foreach (var openedDocument in _openedDocuments.Values)
                {
                    _workspace.RefreshOpenedDocument(openedDocument);
                }
            }
        }
    }
}
