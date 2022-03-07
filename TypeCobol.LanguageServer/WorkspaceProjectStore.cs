using System;
using System.Collections;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.LanguageServer.Context;
using TypeCobol.Tools.APIHelpers;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.LanguageServer
{
    /// <summary>
    /// A class representing a concept to store all WorkspaceProject instances known by the Workspace.
    /// Internally it is a concurrent dictionary from WorkspaceProject Keys to WorkspaceProject instances.
    /// </summary>
    class WorkspaceProjectStore
    {
        /// <summary>
        /// Exception thrown when project keys are duplicated.
        /// </summary>
        internal class DuplicatedProjectException : Exception
        {
            internal DuplicatedProjectException(string key) : base(key)
            {
            }
        }

        /// <summary>
        /// Exception thrown when project key does not correspond to an existing WorkspaceProject.
        /// </summary>
        internal class UnknownProjectException : Exception
        {
            internal UnknownProjectException(string key) : base(key)
            {
            }
        }

        /// <summary>
        /// The dictionary of WorkspaceProject Keys to WorkspaceProject instance.
        /// </summary>
        private readonly ConcurrentDictionary<string, WorkspaceProject> _workspaceProjects;
        private readonly string _rootDirectoryFullName;
        /// <summary>
        /// A Dictionary to associate an Uri of a Document to its Context.
        /// it represents all documents belonging to all workspace projects.
        /// </summary>
        private readonly ConcurrentDictionary<Uri, DocumentContext> _allOpenedDocuments;
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="rootDirectoryFullName">The root directory of the workspace session</param>
        /// <param name="defaultProjectKey">The key of the default workspace project</param>
        /// <param name="useAntlrProgramParsing">Using ANTLR parsing or not</param>
        /// <param name="useEuroInformationLegacyReplacingSyntax">Use E-I options or not</param>
        internal WorkspaceProjectStore(string rootDirectoryFullName, string defaultProjectKey, bool useAntlrProgramParsing, bool useEuroInformationLegacyReplacingSyntax)
        {
            this._rootDirectoryFullName = rootDirectoryFullName;
            this._allOpenedDocuments = new ConcurrentDictionary<Uri, DocumentContext>();
            this._workspaceProjects = new ConcurrentDictionary<string, WorkspaceProject>(StringComparer.OrdinalIgnoreCase);
            CreateDefaultWorkspaceProject(defaultProjectKey, useAntlrProgramParsing, useEuroInformationLegacyReplacingSyntax);
        }

        /// <summary>
        /// The default WorkspaceProject instance.
        /// </summary>
        internal WorkspaceProject DefaultWorkspaceProject { get; private set; }

        /// <summary>
        /// Create the default workspace project
        /// </summary>
        /// <param name="projectKey"></param>
        /// <param name="useAntlrProgramParsing"></param>
        /// <param name="useEuroInformationLegacyReplacingSyntax"></param>
        private void CreateDefaultWorkspaceProject(string projectKey, bool useAntlrProgramParsing, bool useEuroInformationLegacyReplacingSyntax)
        {
            var defaultDocumentFormat = new DocumentFormat(Encoding.GetEncoding("iso-8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat);
            CompilationProject compilationProject = new CompilationProject(
                projectKey, _rootDirectoryFullName, Helpers.DEFAULT_EXTENSIONS, defaultDocumentFormat,
                new TypeCobolOptions(), null); //Initialize a default CompilationProject - has to be recreated after ConfigurationChange Notification
            compilationProject.CompilationOptions.UseAntlrProgramParsing =
                compilationProject.CompilationOptions.UseAntlrProgramParsing || useAntlrProgramParsing;

            compilationProject.CompilationOptions.UseEuroInformationLegacyReplacingSyntax =
                compilationProject.CompilationOptions.UseEuroInformationLegacyReplacingSyntax ||
                useEuroInformationLegacyReplacingSyntax;

            // Create the Default Workspace Project instance.
            DefaultWorkspaceProject = new WorkspaceProject(projectKey, compilationProject);
            // Add it in the store
            this.AddProject(DefaultWorkspaceProject);
        }

        /// <summary>
        /// Indicates whether this workspace project store has opened documents or not.
        /// </summary>
        internal bool IsEmpty => this._workspaceProjects.Values.All(wksPrj => wksPrj.IsEmpty);

        /// <summary>
        /// Find a WorkspaceProject instance by its key.
        /// If the key is null the Default Workspace Project is returned.
        /// </summary>
        /// <param name="projectKey">The Project's key</param>
        /// <returns>The WorkspaceProject instance if one exists, null otherwise</returns>
        internal WorkspaceProject FindProject(string projectKey)
        {
            if (projectKey == null)
                return DefaultWorkspaceProject;
            if (_workspaceProjects.TryGetValue(projectKey, out WorkspaceProject project))
                return project;
            return null;
        }

        /// <summary>
        /// Add a WorkspaceProject instance
        /// </summary>
        /// <param name="project">The project instance to be added</param>
        /// <exception cref="DuplicatedProjectException">If a project with the same key already exists in the store</exception>
        internal void AddProject(WorkspaceProject project)
        {
            System.Diagnostics.Debug.Assert(project != null && project.ProjectKey != null);
            if (!_workspaceProjects.TryAdd(project.ProjectKey, project))
            {
                throw new DuplicatedProjectException(project.ProjectKey);
            }
        }

        /// <summary>
        /// Create a WorkspaceProject instance, and add it to the store.
        /// </summary>
        /// <param name="projectKey">The Project's key</param>
        /// <param name="configuration">The configuration to be applied</param>
        /// <returns>The WorkspaceProject instance created</returns>
        /// <exception cref="DuplicatedProjectException">Sent if the given Project's key is already associated to a project.</exception>
        internal WorkspaceProject CreateWorkspaceProject(string projectKey, TypeCobolConfiguration configuration)
        {
            System.Diagnostics.Debug.Assert(projectKey != null);
            if (_workspaceProjects.ContainsKey(projectKey))
            {
                throw new DuplicatedProjectException(projectKey);
            }

            CompilationProject compilationProject = new CompilationProject(projectKey, this._rootDirectoryFullName, Helpers.DEFAULT_EXTENSIONS,
                configuration.Format, this.DefaultWorkspaceProject.Project.CompilationOptions, 
                this.DefaultWorkspaceProject.Project.AnalyzerProvider);
            WorkspaceProject workspaceProject = new WorkspaceProject(projectKey, compilationProject);
            AddProject(workspaceProject);
            return workspaceProject;
        }

        /// <summary>
        /// Try to get the DocumentContext instance associated to an opened document from its Uri
        /// </summary>
        /// <param name="docUri">The Document's uri to get the associated DocumentContext instance.</param>
        /// <param name="openedDocumentContext">[out] The DocumentContext instance of the opened document if any, null otherwise</param>
        /// <returns>True if the uri was associated to an opened document, false otherwise</returns>
        internal bool TryGetOpenedocument(Uri docUri, out DocumentContext openedDocumentContext)
        {
            return this._allOpenedDocuments.TryGetValue(docUri, out openedDocumentContext);
        }

        /// <summary>
        /// Try to get the DocumentContext and the corresponding WorkspaceProject instance associated to the given Uri
        /// </summary>
        /// <param name="fileUri">The File Uri</param>
        /// <param name="openedDocumentContext"></param>
        /// <param name="workspaceProject"></param>
        /// <returns>true if the DocumentContext and the corresponding WorkspaceProject instance have been found, false otherwise</returns>
        internal bool TryGetOpenedWorkspaceDocumentProject(Uri fileUri, out DocumentContext openedDocumentContext, out WorkspaceProject workspaceProject)
        {
            openedDocumentContext = null;
            workspaceProject = null;
            foreach (var wksProject in _workspaceProjects.Values)
            {
                if (wksProject.TryGetOpenedDocumentContext(fileUri, out openedDocumentContext))
                {
                    workspaceProject = wksProject;
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Try to get the DocumentContext and the corresponding WorkspaceProject instance by name
        /// </summary>
        /// <param name="name">The document's name</param>
        /// <param name="openedDocumentContext"></param>
        /// <returns>true if the DocumentContext have been found, false otherwise</returns>
        internal bool TryGetOpenedDocumentByName(string name, out DocumentContext openedDocumentContext)
        {
            openedDocumentContext = null;
            foreach (var entry in this._allOpenedDocuments)
            {
                if (entry.Key.LocalPath.Contains(name))
                {
                    openedDocumentContext = entry.Value;
                    return true;
                }
            }
            return false;
        }

        /// <summary>
        /// Update for each WorkspaceProject instance its compilation options and AnalyzerProvider instance
        /// </summary>
        /// <param name="compilationOptions">New Compilation Options</param>
        /// <param name="analyzerProvider">new Analyzer Provider instance</param>
        internal void UpdateProjectOptionsAndAnalyzerProvider(TypeCobolOptions compilationOptions, IAnalyzerProvider analyzerProvider)
        {
            foreach (var wksProject in _workspaceProjects.Values)
            {
                wksProject.UpdateProjectOptionsAndAnalyzerProvider(compilationOptions, analyzerProvider);
            }
        }

        /// <summary>
        /// Refresh all documents in all WorkspaceProject instances
        /// </summary>
        /// <param name="workspace">The main Workspace instance</param>
        internal void RefreshOpenedFiles(Workspace workspace)
        {
            foreach (var wksProject in _workspaceProjects.Values)
            {
                wksProject.RefreshOpenedFiles(workspace);
            }
        }

        /// <summary>
        /// Move a Document from one WorkspaceProject to another WorkSpaceProject.
        /// </summary>
        /// <param name="docContext">The Document Context instance to move</param>
        /// <param name="fromWorkspaceProject">The Source WorkspaceProject instance</param>
        /// <param name="toWorkspaceProject">The Target WorkspaceProject instance.</param>
        private void MoveWorkspaceProjectDocument(DocumentContext docContext, WorkspaceProject fromWorkspaceProject, WorkspaceProject toWorkspaceProject)
        {
            System.Diagnostics.Debug.Assert(docContext != null && fromWorkspaceProject != null && toWorkspaceProject != null);
            System.Diagnostics.Debug.Assert(fromWorkspaceProject.Contains(docContext));
            System.Diagnostics.Debug.Assert(!toWorkspaceProject.Contains(docContext));

            fromWorkspaceProject.RemoveDocument(docContext);
            toWorkspaceProject.AddDocument(docContext);            
        }

        /// <summary>
        /// Add the given Document context to the Workspace Project corresponding to the given project's key.
        /// The project's key must correspond to an existing WorkspaceProjet, if the projectKey value is null then 
        /// the Default Workspace Project will be used.
        /// </summary>
        /// <param name="docContext">The DocumentContext to be added</param>
        /// <param name="projectKey">The target Project's key</param>
        /// <exception cref="UnknownProjectException">If the project key does correspond to an existing project</exception>
        /// <returns>true if the document is added, false if it already exists in the target project</returns>
        internal bool AddDocument(DocumentContext docContext, string projectKey)
        {
            WorkspaceProject workspaceProject = FindProject(projectKey);
            if(workspaceProject == null)
                throw new UnknownProjectException(projectKey);
            if (workspaceProject.AddDocument(docContext))
            {   // Add the document to all opened document set.
                return this._allOpenedDocuments.TryAdd(docContext.Uri, docContext);
            }
            return false;
        }

        /// <summary>
        /// Rmove a document from it's uri
        /// </summary>
        /// <param name="docUri">The document's uri</param>
        /// <returns>The removed document if any, null if no document was removed</returns>
        internal DocumentContext RemoveDocumentFromUri(Uri docUri)
        {
            System.Diagnostics.Debug.Assert(docUri != null);
            // First Check that the document was added in the workspace project store.
            bool wasInTheGlobalDocumentSet = this._allOpenedDocuments.TryRemove(docUri, out DocumentContext docContext);
            // Now check if it was added in a document.
            if (TryGetOpenedWorkspaceDocumentProject(docUri, out DocumentContext wksPrjDocContext, out WorkspaceProject workspaceProject))
            {
                if (!wasInTheGlobalDocumentSet)
                {
                    System.Diagnostics.Debug.Fail("Document was not added in the workspace project store : " + docUri.ToString());
                }
                else
                {
                    System.Diagnostics.Debug.Assert(wksPrjDocContext == docContext);
                }
                return workspaceProject.RemoveDocument(wksPrjDocContext) ? wksPrjDocContext : null;
            }
            else if (wasInTheGlobalDocumentSet)
            {
                System.Diagnostics.Debug.Fail("Document was not added in the workspace project : " + docContext.Uri.ToString());
            }
            return docContext;
        }

        /// <summary>
        /// Updates a Workspace Project configuration.
        /// It creates the project if the one indicated by the key does not exist.
        /// It moves the document associated to the given uri into the target project.
        /// It updates the source file provider with the copy folder.
        /// It also refreshes all impacted documents if source file provider changes.
        /// </summary>
        /// <param name="docUri">The Uri of the document to be associated to the workspace project if any, null otherwise</param>
        /// <param name="projectKey">The project key to which the document shall be associated</param>
        /// <param name="copyFolders">List of Copy Folders to be associated with the underlying CompilationProject instance.</param>
        /// <param name="workspace">The Workspace instance</param>
        internal void UpdateProjectConfiguration(Uri docUri, string projectKey, List<string> copyFolders, Workspace workspace)
        {
            // Determine the target project.
            WorkspaceProject targetWorkspaceProject = FindProject(projectKey);
            if (targetWorkspaceProject == null)
            {   // The target project does not exists ==> We must create this new workspace project.
                targetWorkspaceProject = CreateWorkspaceProject(projectKey, workspace.Configuration);
            }
            // Check if the docUri is already associated to a WorkspaceProject
            bool docUriIsValid = docUri != null;
            WorkspaceProject docUriWorkspaceProject = null;
            DocumentContext docContext = null;
            if (docUriIsValid && !TryGetOpenedWorkspaceDocumentProject(docUri, out docContext, out docUriWorkspaceProject))
            {   // hum... It is a new file without corresponding DidOpenDocument notification sent
                // Ignore such file.
                docUriIsValid = false;
                System.Diagnostics.Debug.Fail("Unknown document URI:" + docUri.ToString());                
            }
            // Now try to match its new WorkspaceProject project it can be the same
            if (docUriIsValid && docUriWorkspaceProject != targetWorkspaceProject)
            {
                // We must remove this document from its current workspace project and add it into its new workspace project.
                MoveWorkspaceProjectDocument(docContext, docUriWorkspaceProject, targetWorkspaceProject);
            }
            bool refreshAll = targetWorkspaceProject.UpdateSourceFileProvider(copyFolders, workspace.Configuration);
            if (refreshAll)
            {   // All documents of the WorkspaceProject must be refreshed.
                targetWorkspaceProject.RefreshOpenedFiles(workspace);
            }
            else if(docUriIsValid && docUriWorkspaceProject != targetWorkspaceProject)
            {   // Only this Document must be refreshed
                workspace.RefreshOpenedDocument(docContext, true);
            }
        }
    }
}
