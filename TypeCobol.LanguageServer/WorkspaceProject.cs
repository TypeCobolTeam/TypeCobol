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
        public string ProjectKey { get; }
        /// <summary>
        /// Associated CompilationProject instance.
        /// </summary>
        public CompilationProject Project { get; private set; }
        /// <summary>
        /// A Dictionary to associate an Uri of a Document to its Context.
        /// it represents all documents belonging to the associated project.
        /// </summary>
        private readonly ConcurrentDictionary<Uri, DocumentContext> _openedDocuments;
        /// <summary>
        /// Constructor of a WorkspaceProject instance.
        /// </summary>
        /// <param name="projectKey">The Project's key</param>
        /// <param name="project">The associated CompilationProject instance</param>
        public WorkspaceProject(string projectKey, CompilationProject project)
        {
            this.ProjectKey = projectKey;
            this.Project = project;
            this._openedDocuments = new ConcurrentDictionary<Uri, DocumentContext>();
        }

        /// <summary>
        /// Check if the given document is in this WorkspaceProject instance.
        /// </summary>
        /// <param name="documentCtx">The DocumentContext instance to be checked</param>
        /// <returns>return true if yes, false if no</returns>
        internal bool Contains(DocumentContext documentCtx)
        {
            return (documentCtx != null)
                ? this._openedDocuments.TryGetValue(documentCtx.Uri, out var docCtx) && docCtx == documentCtx
                : false;
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
            if (this._openedDocuments.TryAdd(uri, documentCtx))
            {
                if (documentCtx.FileCompiler != null)
                {
                    // Associate the document compilation project's to this one.
                    documentCtx.FileCompiler.CompilationProject = this.Project;
                }
                return true;
            }
            return false;//This document already exists
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
        /// <param name="configuration">The TypeCobolConfiguration instance in use</param>
        /// <returns>true if the SourcefileProvider has changed, false otherwise.</returns>
        internal bool UpdateSourceFileProvider(List<string> copyFolders, TypeCobolConfiguration configuration)
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
                    Helpers.DEFAULT_COPY_EXTENSIONS, configuration.Format.Encoding,
                    configuration.Format.EndOfLineDelimiter, configuration.Format.FixedLineLength);
            }
            return true;
        }

        /// <summary>
        /// Try to get DocumentContext in this Workspace Project, if the given Uri instance exists
        /// </summary>
        /// <param name="fileUri">The Uri instance to get the DocumentContext instance</param>
        /// <param name="openedDocumentContext">[out] the DocumentContext instance if the Uri exists</param>
        /// <returns>true if a DocumentContext instance has been found, false otherwise</returns>
        internal bool TryGetOpenedDocumentContext(Uri fileUri, out DocumentContext openedDocumentContext)
        {
            return _openedDocuments.TryGetValue(fileUri, out openedDocumentContext);
        }
        
        /// <summary>
        /// Indicates whether this workspace project has opened documents or not.
        /// </summary>
        internal bool IsEmpty
        {
            get 
            {
                return _openedDocuments.IsEmpty;
            }            
        }

        /// <summary>
        /// Refresh all documents of this WorkspaceProject instance
        /// </summary>
        internal void RefreshOpenedFiles(Workspace workspace)
        {
            this.Project.ClearImportedCompilationDocumentsCache();
            foreach (var contextEntry in _openedDocuments)
            {
                DocumentContext docContext = contextEntry.Value;
                System.Diagnostics.Debug.Assert(docContext.FileCompiler.CompilationProject == Project);
                workspace.RefreshOpenedDocument(docContext, false);
            }
        }

        /// <summary>
        /// Update for this WorkspaceProject instance its compilation options and analyzer provider instance.
        /// </summary>
        /// <param name="compilationOptions">The new Compilation Options</param>
        /// <param name="analyzerProvider">The new Analyzer Provider instance</param>
        internal void UpdateProjectOptionsAndAnalyzerProvider(TypeCobolOptions compilationOptions, IAnalyzerProvider analyzerProvider)
        {
            this.Project.CompilationOptions = compilationOptions;
            this.Project.AnalyzerProvider = analyzerProvider;
        }

        /// <summary>
        /// Replace the CompilationProject instance associated to this WorkspaceProject by creating a new one.
        /// </summary>
        /// <param name="configuration">Configuration to be applied</param>
        /// <param name="compilationOptions">Compilation Options to be applied</param>
        /// <param name="analyzerProvider">AnalyzerProvider instance to be set</param>
        internal void ReplaceCompilationProject(TypeCobolConfiguration configuration, TypeCobolOptions compilationOptions, IAnalyzerProvider analyzerProvider)
        {
            Project = new CompilationProject(Project.Name, Project.RootDirectory, Helpers.DEFAULT_EXTENSIONS,
                configuration.Format, compilationOptions, analyzerProvider);
            // Change the target CompilationProject instance for each document.
            foreach(var docCtx in _openedDocuments.Values)
            {
                docCtx.FileCompiler.CompilationProject = Project;
            }
            if (configuration.CopyFolders != null && configuration.CopyFolders.Count > 0)
            {
                foreach (var copyFolder in configuration.CopyFolders)
                {
                    Project.SourceFileProvider.AddLocalDirectoryLibrary(copyFolder, false,
                        Helpers.DEFAULT_COPY_EXTENSIONS, configuration.Format.Encoding,
                        configuration.Format.EndOfLineDelimiter, configuration.Format.FixedLineLength);
                }
            }
        }
    }
}
