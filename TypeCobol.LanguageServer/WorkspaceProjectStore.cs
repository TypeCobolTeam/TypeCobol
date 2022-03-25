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
    internal class WorkspaceProjectStore
    {
        public WorkspaceProject DefaultWorkspaceProject { get; }

        private readonly Workspace _workspace;
        private readonly ConcurrentDictionary<string, WorkspaceProject> _projects;

        public WorkspaceProjectStore(Workspace workspace)
        {
            _workspace = workspace;
            _projects = new ConcurrentDictionary<string, WorkspaceProject>(StringComparer.OrdinalIgnoreCase);

            //Create default WorkspaceProject
            var defaultFormat = new DocumentFormat(Encoding.GetEncoding("iso-8859-1"), EndOfLineDelimiter.CrLfCharacters, 80, ColumnsLayout.CobolReferenceFormat);
            var defaultOptions = new TypeCobolOptions()
                                 {
                                     UseAntlrProgramParsing = _workspace.UseAntlrProgramParsing,
                                     UseEuroInformationLegacyReplacingSyntax = _workspace.UseEuroInformationLegacyReplacingSyntax
                                 };
            var defaultCompilationProject = new CompilationProject(_workspace.Name, _workspace.RootDirectory, Helpers.DEFAULT_EXTENSIONS, defaultFormat, defaultOptions, null);
            DefaultWorkspaceProject = new WorkspaceProject(_workspace, null, defaultCompilationProject);
        }

        public IEnumerable<WorkspaceProject> NamedProjects => _projects.Values;

        public WorkspaceProject GetOrCreateProject(string projectKey)
        {
            if (projectKey == null)
            {
                return DefaultWorkspaceProject;
            }

            return _projects.GetOrAdd(projectKey, CreateProject);

            WorkspaceProject CreateProject(string name)
            {
                var defaultCompilationProject = DefaultWorkspaceProject.CompilationProject;
                var compilationProject = new CompilationProject(name, defaultCompilationProject.RootDirectory, Helpers.DEFAULT_EXTENSIONS, defaultCompilationProject.Format, defaultCompilationProject.CompilationOptions, defaultCompilationProject.AnalyzerProvider);
                return new WorkspaceProject(_workspace, name, compilationProject);
            }
        }

        public void RemoveProject(WorkspaceProject workspaceProject)
        {
            //Default project can't be removed
            if (workspaceProject == DefaultWorkspaceProject) return;

            if (!_projects.TryRemove(workspaceProject.Name, out _))
            {
                //TODO log ?
            }
        }
    }
}
