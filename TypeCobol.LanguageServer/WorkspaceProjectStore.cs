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
    class WorkspaceProjectStore : IEnumerable<WorkspaceProject>
    {
        /// <summary>
        /// Exception thrown when project key are duplicated.
        /// </summary>
        internal class DuplicatedProjectException : Exception
        {
            internal DuplicatedProjectException(string key) : base(key)
            {
            }
        }

        /// <summary>
        /// The dictionary of WorkspaceProject Keys to WorkspaceProject instance.
        /// </summary>
        private readonly ConcurrentDictionary<string, WorkspaceProject> _workspaceProjects;
        private readonly string _rootDirectoryFullName;
        /// <summary>
        /// 
        /// </summary>
        /// <param name="rootDirectoryFullName">The root directory of the workspace session</param>
        /// <param name="defaultProjectKey">The key of the default workspace project</param>
        /// <param name="useAntlrProgramParsing">Using ANTLR parsing or note</param>
        /// <param name="useEuroInformationLegacyReplacingSyntax">Use E-I options or not</param>
        internal WorkspaceProjectStore(string rootDirectoryFullName, string defaultProjectKey, bool useAntlrProgramParsing, bool useEuroInformationLegacyReplacingSyntax)
        {
            this._rootDirectoryFullName = rootDirectoryFullName;
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
        /// Il the key is null the Default Workspace Project is returned.
        /// </summary>
        /// <param name="projectKey">The Project's key</param>
        /// <returns>The WorkspaceProject instance if one exists, null otherwise</returns>
        internal WorkspaceProject FindProject(string projectKey)
        {
            if (projectKey == null)
                return DefaultWorkspaceProject;
            WorkspaceProject project = null;
            if (_workspaceProjects.TryGetValue(projectKey, out project))
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
        /// <exception cref="DuplicatedProjectException">Sent if the given Project's key is already associted to a project.</exception>
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
        /// Try to get the DocumentContext and the corresponding WokspaceProject instance associated to the given Uri
        /// </summary>
        /// <param name="fileUri">The File Uri</param>
        /// <param name="openedDocumentContext"></param>
        /// <param name="workspaceProject"></param>
        /// <returns>true if the DocumentContext and the corresponding WokspaceProject instance have been found, false otherwise</returns>
        internal bool TryGetOpenedWorkspaceDocumentProjet(Uri fileUri, out DocumentContext openedDocumentContext, out WorkspaceProject workspaceProject)
        {
            openedDocumentContext = null;
            workspaceProject = null;
            foreach (var wksProject in this)
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
        /// Try to get the DocumentContext and the corresponding WokspaceProject instance by name
        /// </summary>
        /// <param name="name">The document's name</param>
        /// <param name="openedDocumentContext"></param>
        /// <returns>true if the DocumentContext and the corresponding WokspaceProject instance have been found, false otherwise</returns>
        internal bool TryGetOpenedDocumentByName(string name, out DocumentContext openedDocumentContext, out WorkspaceProject workspaceProject)
        {
            openedDocumentContext = null;
            workspaceProject = null;
            foreach (var wksProject in this)
            {
                if (wksProject.TryGetOpenedDocumentByName(name, out openedDocumentContext))
                {
                    workspaceProject = wksProject;
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
            foreach (var wksProject in this)
            {
                wksProject.UpdateProjectOptionsAndAnalyzerProvider(compilationOptions, analyzerProvider);
            }
        }

        /// <summary>
        /// Refresh all documents in all WorkspaceProject instances
        /// </summary>
        /// <param name="workspace">The main Workspace instance</param>
        internal void DoRefreshOpenedFiles(Workspace workspace)
        {
            foreach (var wksProject in this)
            {
                wksProject.DoRefreshOpenedFiles(workspace);
            }
        }

        /// <summary>
        /// Move a Document from one WorkspaceProject to another WorkSpaceProject.
        /// </summary>
        /// <param name="docContext">The Document Context instance to move</param>
        /// <param name="fromWorkspaceProject">The Source WorkspaceProject instance</param>
        /// <param name="toWorkspaceProject">The Target WorkspaceProject instance.</param>
        internal void MoveWorkspaceProjectDocument(DocumentContext docContext, WorkspaceProject fromWorkspaceProject, WorkspaceProject toWorkspaceProject)
        {
            System.Diagnostics.Debug.Assert(docContext != null && fromWorkspaceProject != null && toWorkspaceProject != null);
            System.Diagnostics.Debug.Assert(fromWorkspaceProject.Contains(docContext));
            System.Diagnostics.Debug.Assert(!toWorkspaceProject.Contains(docContext));

            fromWorkspaceProject.RemoveDocument(docContext);
            toWorkspaceProject.AddDocument(docContext);            
        }

        /// <summary>
        /// Associate a document to a Workspace Project
        /// </summary>
        /// <param name="docUri">The Uri of the document to be associated to workspace projet</param>
        /// <param name="projectKey">The project key to which the document shall be associated</param>
        /// <param name="copyFolders">List of Copy Folders to be associated with the underlying CompilationProject instance.</param>
        /// <param name="workspace">The Workspace project instance</param>
        internal void AssociateDocumentToProject(Uri docUri, string projectKey, List<string> copyFolders, Workspace workspace)
        {
            // Check if the docUri is already associated to a WorkspaceProject
            DocumentContext docContext = null;
            WorkspaceProject workspaceProject = null;
            if (!TryGetOpenedWorkspaceDocumentProjet(docUri, out docContext, out workspaceProject))
            {   // hum... It is a new file without corresponding DidOpenDocument notification sent
                // Ignore such file.
                return;
            }
            // Now try to match its new WorkspaceProject project it can be the same
            WorkspaceProject newWorkspaceProject = null;
            if (projectKey == null)
            {   // No project key specified ==> Goes in to the Default Workspace Project.
                newWorkspaceProject = DefaultWorkspaceProject;
            }
            else
            {   // Find the target WorkspaceProject
                newWorkspaceProject = FindProject(projectKey);
                if (newWorkspaceProject == null)
                { // The target project does not exists ==> We must create this new workspace project.
                    newWorkspaceProject = CreateWorkspaceProject(projectKey, workspace.Configuration);
                }
            }
            if (workspaceProject != newWorkspaceProject)
            {
                // We must remove this document from its current workspace project and add it into its new workspace project.
                MoveWorkspaceProjectDocument(docContext, workspaceProject, newWorkspaceProject);
            }
            bool refreshAll = newWorkspaceProject.UpdateSourceFileProvider(copyFolders, workspace.Configuration);
            if ((workspaceProject != newWorkspaceProject) && !refreshAll)
            {   // Only this Document must be refreshed
                workspace.RefreshOpenedDocument(docContext, true);
            }
            else if (refreshAll)
            {   // All documents of the WokspaceProject must be refreshed.
                newWorkspaceProject.DoRefreshOpenedFiles(workspace);
            }
        }

        /// <summary>
        /// Get an Enumerator instance on all WorkspaceProject instances.
        /// </summary>
        /// <returns>The enumerator instance</returns>
        public IEnumerator<WorkspaceProject> GetEnumerator()
        {
            return _workspaceProjects.Values.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return this.GetEnumerator();
        }
    }
}
