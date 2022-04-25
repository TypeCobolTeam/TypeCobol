using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;

namespace TypeCobol.LanguageServer
{
    public class DependenciesFileWatcher : IDisposable
    {
        private Workspace _TypeCobolWorkSpace;
        private List<FileSystemWatcher> fileWatchers = new List<FileSystemWatcher>();
        private readonly Action refreshAction;

        public DependenciesFileWatcher(Workspace workspace, Action refreshAction)
        {
            _TypeCobolWorkSpace = workspace;
            this.refreshAction = refreshAction;
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

            fileWatchers.Add(watcher);
        }

        private void OnChanged(object sender, FileSystemEventArgs e)
        {
            var directory= new FileInfo(e.FullPath).Directory;
            if (File.Exists(directory.FullName + Path.DirectorySeparatorChar + "~.lock"))
                return;
        
            // Check if there isn't another refresh action from another fileWatcher in the queue
            if (_TypeCobolWorkSpace.MessagesActionsQueue.All(mw => mw.Action != refreshAction))
            {
                _TypeCobolWorkSpace.MessagesActionsQueue.Enqueue(new MessageActionWrapper(refreshAction));
            }
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
