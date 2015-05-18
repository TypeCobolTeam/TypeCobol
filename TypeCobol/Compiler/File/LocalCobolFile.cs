using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reactive.Subjects;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.File
{
    /// <summary>
    /// Implementation of a Cobol file as a text file in the local filesystem
    /// </summary>
    public class LocalCobolFile : CobolFile
    {        
        // Underlying filesystem implementation
        private FileInfo localFileInfo;

        /// <summary>
        /// Initialize a Cobol file from a text file in the local filesystem
        /// </summary>
        /// <param name="name">Name of the Cobol text, as specified in a Cobol program (COPY textName)</param>
        /// <param name="fullPath">Full path of the file in the local filesystem</param>
        /// <param name="encoding">Encoding used to read or write the Cobol file
        /// <param name="endOfLineDelimiter">Tells the compiler how to normalize the end of line characters</param>
        /// <param name="fixedLineLength">Used only in case the text is formatted with fixed length lines</param>
        public LocalCobolFile(string name, string fullPath, Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength) : 
            this(null, name, fullPath, encoding, endOfLineDelimiter, fixedLineLength)
        { }

        // Cobol library optionally used as a source for this file
        private LocalDirectoryLibrary parentCobolLibrary;

        /// <summary>
        /// This version of the constructor is called by LocalDirectoryLibrary only
        /// </summary>
        internal LocalCobolFile(LocalDirectoryLibrary parentCobolLibrary, string name, string fullPath, Encoding encoding, EndOfLineDelimiter endOfLineDelimiter, int fixedLineLength)
        {
            Name = name;
            
            localFileInfo = new FileInfo(fullPath);
            if(!localFileInfo.Exists)
            {
                throw new ArgumentException(String.Format("Full path for local Cobol file is invalid : {0}"), fullPath);
            }
            FullPath = localFileInfo.FullName;

            Encoding = encoding;
            EndOfLineDelimiter = endOfLineDelimiter;
            if (endOfLineDelimiter == EndOfLineDelimiter.FixedLengthLines)
            {
                FixedLineLength = fixedLineLength;
            }

            this.parentCobolLibrary = parentCobolLibrary;
        }
               
        /// <summary>
        /// Gets the creation time of the Cobol file 
        /// </summary>
        public override DateTime CreationTime { get { return localFileInfo.CreationTime; } }

        /// <summary>
        /// Gets the time when the Cobol file was last written to
        /// </summary>
        public override DateTime LastWriteTime { get { return localFileInfo.LastWriteTime; } }

        /// <summary>
        /// Determines if the Cobol file is read only
        /// </summary>
        public override bool IsReadOnly { get { return localFileInfo.IsReadOnly; } }

        /// <summary>
        /// Gets the size, in bytes, of the Cobol file
        /// </summary>
        public override long Length { get { return localFileInfo.Length; } }

        // -- File operations --

        /// <summary>
        /// Overload this method to get a stream to read the binary contents of the file
        /// </summary>
        protected override Stream OpenInputStream()
        {
            return new FileStream(FullPath, FileMode.Open);
        }

        /// <summary>
        /// Overload this method to get a stream to write a new binary content to the file
        /// </summary>
        /// <returns></returns>
        protected override Stream OpenOutputStream()
        {
            return new FileStream(FullPath, FileMode.Create);
        }

        /// <summary>
        /// Used by LocalDirectoryLibrary to notify file changes when monitoring is turned on
        /// </summary>
        internal ISubject<CobolFileChangedEvent> CobolFileChangedSubject = new Subject<CobolFileChangedEvent>();

        /// <summary>
        /// Observers can subscribe to be notified of any external change applied to the Cobol file 
        /// </summary>
        public override IObservable<CobolFileChangedEvent> CobolFileChangedEventsSource
        {
            get { return CobolFileChangedSubject; }
        }

        /// <summary>
        /// Starts monitoring the external changes applied to the Cobol file (service stopped by default)
        /// </summary>
        public override void StartMonitoringExternalChanges()
        {
            if (parentCobolLibrary != null)
            {
                parentCobolLibrary.StartMonitoringCobolFile(this);
            }
        }

        /// <summary>
        /// Stops monitoring the external changes applied to the Cobol file
        /// </summary>
        public override void StopMonitoringExternalChanges()
        {
            if (parentCobolLibrary != null)
            {
                parentCobolLibrary.StopMonitoringCobolFile(this);
            }
        }

        /// <summary>
        /// Release all resources acquired to implement and monitor this Cobol file
        /// </summary>
        public override void Dispose()
        {
            StopMonitoringExternalChanges();
            parentCobolLibrary = null;
            localFileInfo = null;
        }       
    }
}
