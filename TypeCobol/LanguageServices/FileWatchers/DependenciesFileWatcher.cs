using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using System.Security.Permissions;
using TypeCobol.LanguageServices.Editor;

namespace TypeCobol.LanguageServices.FileWatchers
{
    public class DependenciesFileWatcher : IDisposable
    {
        private Workspace _TypeCobolWorkSpace;
        private List<FileSystemWatcher> fileWatchers = new List<FileSystemWatcher>();

        public DependenciesFileWatcher(Workspace workspace)
        {
            _TypeCobolWorkSpace = workspace;
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
            //watcher.Filter = "*.tcbl|*.cpy" //Does not work like this, may need to initialize multiple filewatcher for each file extension. 

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
            Action refreshAction = () => { _TypeCobolWorkSpace.RefreshOpenedFiles(); };
            lock (_TypeCobolWorkSpace.ActionQueue)
            {
                if (!_TypeCobolWorkSpace.ActionQueue.Contains(refreshAction))
                {
                    _TypeCobolWorkSpace.ActionQueue.Push(refreshAction);
                }
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
