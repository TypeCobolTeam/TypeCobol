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
            internal DuplicatedProjectException(string key) : base($"Duplicate Project: '{key}'")
            {
            }
        }

        /// <summary>
        /// The dictionary of WorkspaceProject Keys to WorkspaceProject instance.
        /// </summary>
        private readonly ConcurrentDictionary<string, WorkspaceProject> _namedProjects;

        /// <summary>
        /// Underlying workspace project
        /// </summary>
        private readonly Workspace _workspace;

        /// <summary>
        /// Enumeration on WorkspaceProject instances
        /// </summary>
        public IEnumerable<WorkspaceProject> NamedProjects => _namedProjects.Values;

        /// <summary>
        /// Enumeration of all projects, that is Default and Named ones.
        /// </summary>
        public IEnumerable<WorkspaceProject> AllProjects
        {
            get
            {
                yield return DefaultWorkspaceProject;
                foreach (var namedProject in _namedProjects.Values)
                {
                    yield return namedProject;
                }
            }
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="workspace">The workspace</param>
        internal WorkspaceProjectStore(Workspace workspace)
        {
            this._workspace = workspace;
            this._namedProjects = new ConcurrentDictionary<string, WorkspaceProject>(StringComparer.OrdinalIgnoreCase);
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
            //We don't have the information about the real RootDirectory.
            //We use the RootDirectory from the Workspace because it has no consequences as long as
            //this folder contains no copy
            CompilationProject compilationProject = new CompilationProject(
                this._workspace.Name, this._workspace.RootDirectory, Helpers.DEFAULT_EXTENSIONS, defaultDocumentFormat,
                defaultOptions, null); //Initialize a default CompilationProject - has to be recreated after ConfigurationChange Notification

            // Create the Default Workspace Project instance.
            DefaultWorkspaceProject = new WorkspaceProject(this._workspace.Name, compilationProject, _workspace);
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
            
            //We don't have the information about the real RootDirectory.
            //We use the RootDirectory from the Workspace because it has no consequences as long as
            //this folder contains no copy
            CompilationProject compilationProject = new CompilationProject(projectKey, this._workspace.RootDirectory, Helpers.DEFAULT_EXTENSIONS,
                _workspace.Configuration.Format, this.DefaultWorkspaceProject.Project.CompilationOptions,
                this.DefaultWorkspaceProject.Project.AnalyzerProvider);
            WorkspaceProject workspaceProject = new WorkspaceProject(projectKey, compilationProject, _workspace);
            if (!_namedProjects.TryAdd(projectKey, workspaceProject))
            {
                throw new DuplicatedProjectException(projectKey);
            }

            return workspaceProject;
        }

        /// <summary>
        /// Get or create a project.
        /// </summary>
        /// <param name="projectKey">The key of the project to get or to create, if null then the default project is returned</param>
        /// <returns>Existing or newly created WorkspaceProject instance</returns>
        internal WorkspaceProject GetOrCreateProject(string projectKey)
        {
            if (projectKey == null)
                return DefaultWorkspaceProject;

            return _namedProjects.GetOrAdd(projectKey, CreateWorkspaceProject);
        }

        /// <summary>
        /// Find and return a WorkspaceProject by its project key.
        /// Return default WorkspaceProject when given key is null.
        /// </summary>
        /// <param name="projectKey">Project key, can be null.</param>
        /// <param name="project">[out] Found WorkspaceProject instance, null if no project matches the given key.</param>
        /// <returns>True if the named project has been found, False otherwise.</returns>
        internal bool TryGetProject(string projectKey, out WorkspaceProject project)
        {
            if (projectKey == null)
            {
                project = DefaultWorkspaceProject;
                return true;
            }

            return _namedProjects.TryGetValue(projectKey, out project);
        }


        /// <summary>
        /// Remove a project from this workspace.
        /// </summary>
        /// <param name="workspaceProject"></param>
        /// <returns>true if the project has been removed, false otherwise</returns>
        internal bool RemoveProject(WorkspaceProject workspaceProject)
        {
            if (workspaceProject != this.DefaultWorkspaceProject)
            {
                bool bRemoved = this._namedProjects.TryRemove(workspaceProject.ProjectKey, out var storedProject);
                // Assertion: If the project has been removed it must correspond to the one that was stored.
                System.Diagnostics.Debug.Assert(!bRemoved || (storedProject == workspaceProject));
                return bRemoved;
            }

            return true;
        }
    }
}
