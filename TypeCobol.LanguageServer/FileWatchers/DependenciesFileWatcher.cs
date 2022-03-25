using System;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.LanguageServer
{
    public class DependenciesFileWatcher : IDisposable
    {
        private readonly Workspace _workspace;
        private readonly List<FileSystemWatcher> _fileWatchers;

        public DependenciesFileWatcher(Workspace workspace)
        {
            _workspace = workspace;
            _fileWatchers = new List<FileSystemWatcher>();
        }

        public void SetDirectoryWatcher(string directoryPath)
        {
            var fileNameExt = Path.GetFileName(directoryPath);
            if (fileNameExt != null)
                directoryPath = directoryPath.Replace(fileNameExt, "");//Remove filename with extension at the end
            if (!Directory.Exists(directoryPath))
                return; //If directory does not exist do no try to watch..

            //Initialize File Watcher
            var watcher = new FileSystemWatcher
            {
                Path = directoryPath,
                NotifyFilter =
                    NotifyFilters.LastAccess | NotifyFilters.LastWrite | NotifyFilters.FileName |
                    NotifyFilters.DirectoryName
            };
            watcher.Filter = "*.tcbl";

            // Add event handlers.
            watcher.Changed += OnChanged;
            watcher.Created += OnChanged;
            watcher.Deleted += OnChanged;
            watcher.Renamed += OnChanged;

            //Start Watching files
            watcher.EnableRaisingEvents = true;

            _fileWatchers.Add(watcher);
        }

        private void OnChanged(object sender, FileSystemEventArgs e)
        {
            var directory= new FileInfo(e.FullPath).Directory;
            if (File.Exists(directory.FullName + Path.DirectorySeparatorChar + "~.lock"))
                return;

            _workspace.RefreshCustomSymbols();
            _workspace.ScheduleRefreshForAllOpenedFiles(false);
        }

        public void Dispose()
        {
            foreach (var fileWatcher in _fileWatchers)
            {
                fileWatcher.EnableRaisingEvents = false;
                fileWatcher.Dispose();
            }

            _fileWatchers.Clear();
        }
    }
}
