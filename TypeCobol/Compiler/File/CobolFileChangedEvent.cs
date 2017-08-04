using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.File
{
    /// <summary>
    /// An external change can update the contents of a Cobol file, move (or rename) the file to a different path, or delete the file
    /// </summary>
    public enum CobolFileChangeType
    {
        FileChanged,
        FileRenamed,
        FileDeleted
    }

    /// <summary>
    /// External change detected on a Cobol file
    /// </summary>
    public class CobolFileChangedEvent : EventArgs
    {
        /// <summary>
        /// Some Cobol file types can be monitored for external changes
        /// </summary>
        /// <param name="type">Type of file change detected</param>
        /// <param name="changeTime">Date and time at which the file changed occured</param>
        /// <param name="newFullPath">Only for FileMoved events (null for other events) : new full path for the Cobol file after the change occured</param> 
        public CobolFileChangedEvent(CobolFileChangeType type, DateTime changeTime, string newFullPath)
        {
            Type = type;
            ChangeTime = changeTime;
            NewFullPath = newFullPath;
        }

        /// <summary>
        /// Type of file change detected
        /// </summary>
        public CobolFileChangeType Type { get; private set; }

        /// <summary>
        /// Date and time at which the file changed occured
        /// </summary>
        public DateTime ChangeTime { get; private set; }

        /// <summary>
        /// New full path for the Cobol file after the change occured
        /// </summary>
        public string NewFullPath { get; private set; }
    }
}
