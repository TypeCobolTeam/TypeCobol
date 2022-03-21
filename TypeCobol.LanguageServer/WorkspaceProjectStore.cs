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
        /// <param name="rootDirectoryFullName">The root directory of the workspace session</param>
        /// <param name="defaultProjectKey">The key of the default workspace project</param>
        /// <param name="useAntlrProgramParsing">Using ANTLR parsing or not</param>
        /// <param name="useEuroInformationLegacyReplacingSyntax">Use E-I options or not</param>
        internal WorkspaceProjectStore(Workspace workspace, string rootDirectoryFullName, string defaultProjectKey, bool useAntlrProgramParsing, bool useEuroInformationLegacyReplacingSyntax)
        {
            this._workspace = workspace;
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
            DefaultWorkspaceProject = new WorkspaceProject(projectKey, compilationProject, _workspace);
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
        /// <param name="configuration">The configuration to be applied</param>
        /// <returns>The WorkspaceProject instance created</returns>
        /// <exception cref="DuplicatedProjectException">Sent if the given Project's key is already associated to a project.</exception>
        private WorkspaceProject CreateWorkspaceProject(string projectKey, TypeCobolConfiguration configuration)
        {
            System.Diagnostics.Debug.Assert(projectKey != null);
            if (_workspaceProjects.ContainsKey(projectKey))
            {
                throw new DuplicatedProjectException(projectKey);
            }

            CompilationProject compilationProject = new CompilationProject(projectKey, this._rootDirectoryFullName, Helpers.DEFAULT_EXTENSIONS,
                configuration.Format, this.DefaultWorkspaceProject.Project.CompilationOptions, 
                this.DefaultWorkspaceProject.Project.AnalyzerProvider);
            WorkspaceProject workspaceProject = new WorkspaceProject(projectKey, compilationProject, _workspace);
            AddProject(workspaceProject);
            return workspaceProject;
        }

        /// <summary>
        /// Refresh all documents in all WorkspaceProject instances
        /// </summary>
        /// <param name="workspace">The main Workspace instance</param>
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
                targetWorkspaceProject = CreateWorkspaceProject(projectKey, _workspace.Configuration);
            }
            return targetWorkspaceProject;
        }

        /// <summary>
        /// Remove a project from this workspace if it is empty
        /// </summary>
        /// <param name="workspaceProject"></param>
        /// <returns>true if the project has been removed, false otherwise</returns>
        internal bool RemoveProject(WorkspaceProject workspaceProject)
        {
            bool bRemoved = false;
            if (workspaceProject.IsEmpty)
            {
                bRemoved = this._workspaceProjects.TryRemove(workspaceProject.ProjectKey, out var storedProject);
                // Assertion: If the project has been removed it must correspond to the one that was stored.
                System.Diagnostics.Debug.Assert(!bRemoved || (storedProject == workspaceProject));
            }
            return bRemoved;
        }
    }
}
