using System;
using System.Collections;
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
using TypeCobol.Tools.Options_Config;

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
        /// Underlying workspace project
        /// </summary>
        private readonly Workspace _workspace;

        /// <summary>
        /// Enumeration on opened documents.
        /// </summary>
        internal IEnumerable<DocumentContext> OpenedDocuments => _openedDocuments.Values;

        /// <summary>
        /// Constructor of a WorkspaceProject instance.
        /// </summary>
        /// <param name="projectKey">The Project's key</param>
        /// <param name="project">The associated CompilationProject instance</param>
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
        /// Determines if the given list of copy folders is different from those of the current SourceFileProvider instance.
        /// </summary>
        /// <param name="copyFolders">The list of copy folders</param>
        /// <returns>True if copyFolders represents a new set of copy folders, false otherwise.</returns>
        private bool SourceFileProviderNeedsToBeUpdated(List<string> copyFolders)
        {
            if (copyFolders.Count != Project.SourceFileProvider.CobolLibraries.Count)
                return true;// The size of the list are different
            HashSet<string> srcFileProviderFolders = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase);
            foreach (var cblLib in Project.SourceFileProvider.CobolLibraries)
            {
                if (cblLib is LocalDirectoryLibrary localDirLib)
                {
                    srcFileProviderFolders.Add(localDirLib.RootDirectory.FullName);
                }
            }
            foreach (var cpyFolder in copyFolders)
            {
                string fullPath = Path.GetFullPath(cpyFolder);
                if (!srcFileProviderFolders.Contains(fullPath))
                    return true;
            }
            return false;
        }

        /// <summary>
        /// Update the source file provider associated to this project with the given list of Copy Folders.
        /// </summary>
        /// <param name="copyFolders">The new list of Copy Folders.</param>
        /// <param name="documentFormat">The DocumentFormat to be used</param>
        /// <returns>true if the SourcefileProvider has changed, false otherwise.</returns>
        private bool UpdateSourceFileProvider(List<string> copyFolders, DocumentFormat documentFormat)
        {
            if (!SourceFileProviderNeedsToBeUpdated(copyFolders))
            {
                return false;
            }
            // Now set new copy Directories
            Project.SourceFileProvider.RemoveAllLibraries();
            foreach (var copyFolder in copyFolders)
            {
                Project.SourceFileProvider.AddLocalDirectoryLibrary(copyFolder, false,
                    Helpers.DEFAULT_COPY_EXTENSIONS, documentFormat.Encoding,
                    documentFormat.EndOfLineDelimiter, documentFormat.FixedLineLength);
            }
            return true;
        }

        /// <summary>
        /// Configure this project
        /// </summary>
        /// <param name="format"></param>
        /// <param name="options"></param>
        /// <param name="analyzerProviderWrapper"></param>
        /// <param name="copyFolders">The list of copy folders associated to this project</param>
        public void Configure(DocumentFormat format, TypeCobolOptions options, AnalyzerProviderWrapper analyzerProviderWrapper, List<string> copyFolders)
        {
            bool bUpdated = false;
            if (format != null && options != null && analyzerProviderWrapper != null)
            {
                if (copyFolders == null)
                {   // Use current copy folder list from the current project.
                    copyFolders = new List<string>();
                    foreach (var cblLib in Project.SourceFileProvider.CobolLibraries)
                    {
                        if (cblLib is LocalDirectoryLibrary localDirLib)
                        {
                            copyFolders.Add(localDirLib.RootDirectory.FullName);
                        }
                    }
                }

                Project = new CompilationProject(Project.Name, Project.RootDirectory, Helpers.DEFAULT_EXTENSIONS,
                    format, options, analyzerProviderWrapper);
                // Change the target CompilationProject instance for each document.
                foreach (var docCtx in _openedDocuments.Values)
                {
                    docCtx.FileCompiler.CompilationProject = Project;
                }
                bUpdated = true;
            } 
            if (copyFolders != null && UpdateSourceFileProvider(copyFolders, format)) 
            { 
                bUpdated = true;
            }
            if (bUpdated && !IsEmpty)
            {
                _workspace.ScheduleRefresh(this);
            }
        }
        
        /// <summary>
        /// Indicates whether this workspace project has opened documents or not.
        /// </summary>
        internal bool IsEmpty => _openedDocuments.IsEmpty;

        /// <summary>
        /// Refresh all documents of this WorkspaceProject instance
        /// </summary>
        internal void RefreshOpenedFiles()
        {
            _workspace.ScheduleRefresh(this);
        }
    }
}
