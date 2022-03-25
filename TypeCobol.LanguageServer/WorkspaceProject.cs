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
    internal class WorkspaceProject
    {
        public static bool AreCopyFoldersSame(HashSet<string> oldCopyFolders, List<string> newCopyFolders)
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

        private readonly Workspace _workspace;
        private readonly ConcurrentDictionary<Uri, DocumentContext> _documents;

        public string Name { get; }

        /// <summary>
        /// Associated CompilationProject instance.
        /// </summary>
        public CompilationProject CompilationProject { get; private set; }

        public WorkspaceProject(Workspace workspace, string projectName, CompilationProject compilationProject)
        {
            _workspace = workspace;
            _documents = new ConcurrentDictionary<Uri, DocumentContext>();
            Name = projectName;
            CompilationProject = compilationProject;
        }

        public IEnumerable<DocumentContext> Documents => _documents.Values;

        public bool IsEmpty => _documents.IsEmpty;

        public void AddDocument(DocumentContext docContext)
        {
            if (!_documents.TryAdd(docContext.Uri, docContext))
            {
                //TODO log ?
            }
        }

        public void RemoveDocument(DocumentContext docContext)
        {
            if (!_documents.TryRemove(docContext.Uri, out _))
            {
                //TODO log ?
            }
        }

        public HashSet<string> GetCopyFolders()
        {
            var result = new HashSet<string>(StringComparer.InvariantCultureIgnoreCase);
            foreach (var cobolLibrary in CompilationProject.SourceFileProvider.CobolLibraries)
            {
                if (cobolLibrary is LocalDirectoryLibrary localDirectoryLibrary)
                {
                    result.Add(localDirectoryLibrary.RootDirectory.FullName);
                }
            }

            return result;
        }

        public void Configure(DocumentFormat format, TypeCobolOptions options, AnalyzerProviderWrapper analyzerProviderWrapper, List<string> copyFolders)
        {
            bool updateFormat = false;
            var newFormat = CompilationProject.Format;
            if (format != null)
            {
                updateFormat = true;
                newFormat = format;
            }

            bool updateOptions = false;
            var newOptions = CompilationProject.CompilationOptions;
            if (options != null)
            {
                updateOptions = true;
                newOptions = options;
            }

            bool updateAnalyzerProvider = false;
            var newAnalyzerProvider = CompilationProject.AnalyzerProvider;
            if (analyzerProviderWrapper != null)
            {
                updateAnalyzerProvider = true;
                newAnalyzerProvider = analyzerProviderWrapper;
            }

            var oldCopyFolders = GetCopyFolders();
            bool updateCopyFolders = false;
            var newCopyFolders = oldCopyFolders.ToList();
            if (copyFolders != null && AreCopyFoldersSame(oldCopyFolders, copyFolders))
            {
                updateCopyFolders = true;
                newCopyFolders = copyFolders;
            }

            if (updateFormat || updateOptions || updateAnalyzerProvider || updateCopyFolders)
            {
                var oldCompilationProject = CompilationProject;
                var newCompilationProject = new CompilationProject(oldCompilationProject.Name, oldCompilationProject.RootDirectory, Helpers.DEFAULT_EXTENSIONS, newFormat, newOptions, newAnalyzerProvider);
                foreach (var newCopyFolder in newCopyFolders)
                {
                    newCompilationProject.SourceFileProvider.AddLocalDirectoryLibrary(newCopyFolder, false, Helpers.DEFAULT_COPY_EXTENSIONS, newFormat);
                }

                CompilationProject = newCompilationProject;
                _workspace.ScheduleRefresh(this, updateCopyFolders);
            }
        }
    }
}
