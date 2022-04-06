using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Text;
using TypeCobol.Tools.APIHelpers;

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
        /// <summary>
        /// Underlying workspace project
        /// </summary>
        private readonly Workspace _workspace;

        /// <summary>
        /// Enumeration on WorkspaceProject instances
        /// </summary>
        public IEnumerable<WorkspaceProject> NamedProjects => _workspaceProjects.Values;
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="workspace">The workspace</param>
        internal WorkspaceProjectStore(Workspace workspace)
        {
            this._workspace = workspace;
            this._workspaceProjects = new ConcurrentDictionary<string, WorkspaceProject>(StringComparer.OrdinalIgnoreCase);
            CreateDefaultWorkspaceProject();
        }

        /// <summary>
        /// The default WorkspaceProject instance.
        /// </summary>
        internal WorkspaceProject DefaultWorkspaceProject { get; private set; }

        /// <summary>
        /// Create the default workspace project
        /// </summary>
        private void CreateDefaultWorkspaceProject()
        {
            var defaultDocumentFormat = new DocumentFormat(Encoding.GetEncoding("iso-8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat);
            var defaultOptions = new TypeCobolOptions()
            {
                UseAntlrProgramParsing = _workspace.UseAntlrProgramParsing,
                UseEuroInformationLegacyReplacingSyntax = _workspace.UseEuroInformationLegacyReplacingSyntax
            };
            CompilationProject compilationProject = new CompilationProject(
                this._workspace.Name, this._workspace.RootDirectory, Helpers.DEFAULT_EXTENSIONS, defaultDocumentFormat,
                defaultOptions, null); //Initialize a default CompilationProject - has to be recreated after ConfigurationChange Notification

            // Create the Default Workspace Project instance.
            DefaultWorkspaceProject = new WorkspaceProject(this._workspace.Name, compilationProject, _workspace);
        }

        /// <summary>
        /// Find a WorkspaceProject instance by its key.
        /// If the key is null the Default Workspace Project is returned.
        /// </summary>
        /// <param name="projectKey">The Project's key</param>
        /// <returns>The WorkspaceProject instance if one exists, null otherwise</returns>
        private WorkspaceProject FindProject(string projectKey)
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
        private void AddProject(WorkspaceProject project)
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
        /// <returns>The WorkspaceProject instance created</returns>
        /// <exception cref="DuplicatedProjectException">Sent if the given Project's key is already associated to a project.</exception>
        private WorkspaceProject CreateWorkspaceProject(string projectKey)
        {
            System.Diagnostics.Debug.Assert(projectKey != null);
            if (_workspaceProjects.ContainsKey(projectKey))
            {
                throw new DuplicatedProjectException(projectKey);
            }

            CompilationProject compilationProject = new CompilationProject(projectKey, this._workspace.RootDirectory, Helpers.DEFAULT_EXTENSIONS,
                _workspace.Configuration.Format, this.DefaultWorkspaceProject.Project.CompilationOptions,
                this.DefaultWorkspaceProject.Project.AnalyzerProvider);
            WorkspaceProject workspaceProject = new WorkspaceProject(projectKey, compilationProject, _workspace);
            AddProject(workspaceProject);
            return workspaceProject;
        }

        /// <summary>
        /// Refresh all documents in all WorkspaceProject instances
        /// </summary>
        internal void RefreshOpenedFiles()
        {
            foreach (var wksProject in _workspaceProjects.Values)
            {
                wksProject.RefreshOpenedFiles();
            }
        }

        /// <summary>
        /// Get or create a project.
        /// </summary>
        /// <param name="projectKey">The key of the project to get or to create, if null then the default project is returned</param>
        /// <returns></returns>
        internal WorkspaceProject GetOrCreateProject(string projectKey)
        {
            if (projectKey == null)
                return DefaultWorkspaceProject;
            // Determine the target project.
            WorkspaceProject targetWorkspaceProject = FindProject(projectKey);
            if (targetWorkspaceProject == null)
            {   // The target project does not exists ==> We must create this new workspace project.
                targetWorkspaceProject = CreateWorkspaceProject(projectKey);
            }
            return targetWorkspaceProject;
        }

        /// <summary>
        /// Remove a project from this workspace.
        /// </summary>
        /// <param name="workspaceProject"></param>
        /// <returns>true if the project has been removed, false otherwise</returns>
        internal bool RemoveProject(WorkspaceProject workspaceProject)
        {
            bool bRemoved = this._workspaceProjects.TryRemove(workspaceProject.ProjectKey, out var storedProject);
            // Assertion: If the project has been removed it must correspond to the one that was stored.
            System.Diagnostics.Debug.Assert(!bRemoved || (storedProject == workspaceProject));
            return bRemoved;
        }
    }
}
