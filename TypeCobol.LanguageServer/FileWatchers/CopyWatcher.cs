using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;

namespace TypeCobol.LanguageServer
{
    public class CopyWatcher : IDisposable
    {
        private Workspace _TypeCobolWorkSpace;
        private List<FileSystemWatcher> fileWatchers = new List<FileSystemWatcher>();

        public CopyWatcher(Workspace workspace)
        {
            _TypeCobolWorkSpace = workspace;
        }

        public void SetDirectoryWatcher(string directoryPath)
        {
            //Initialize File Watcher
            var watcher = new FileSystemWatcher
            {
                Path = directoryPath,
                NotifyFilter =
                    NotifyFilters.LastAccess | NotifyFilters.LastWrite | NotifyFilters.FileName |
                    NotifyFilters.DirectoryName
            };
            watcher.Filter = "*.cpy";

            // Add event handlers.
            watcher.Changed += OnChanged;
            //No need to watch COPY creation
            //If a copy is missing it'll be loaded once a parser refresh is called
            //watcher.Created += OnChanged; 
            watcher.Deleted += OnChanged;
            watcher.Renamed += OnChanged;

            //Start Watching files
            watcher.EnableRaisingEvents = true;

            fileWatchers.Add(watcher);
        }

        private void OnChanged(object sender, FileSystemEventArgs e)
        {
            var directory= new FileInfo(e.FullPath).Directory;
            if (File.Exists(directory.FullName + Path.DirectorySeparatorChar + "~.lock"))
                return;

            _TypeCobolWorkSpace.CompilationProject.ClearImportedCompilationDocumentsCache();
        }

        public void Dispose()
        {
            foreach (var fileWatcher in fileWatchers)
            {
                fileWatcher.EnableRaisingEvents = false;
                fileWatcher.Dispose();
            }

            fileWatchers.Clear();
        }
    }
}
