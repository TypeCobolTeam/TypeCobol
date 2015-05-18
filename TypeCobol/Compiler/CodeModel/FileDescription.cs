using System;
using System.Collections.Generic;
using TypeCobol.Compiler.CodeElements;

namespace TypeCobol.Compiler.CodeModel
{
    /// <summary>
    /// Provides information about the physical structure and identification of a file, and gives the record-names associated with that file. 
    /// </summary>
    public class FileDescription
    {
        /// <summary>
        /// Provides information about the physical structure and identification of a file
        /// </summary>
        public FileDescriptionEntry FileProperties { get; set; }   

        /// <summary>
        /// A set of data description entries that describe the particular records contained within a particular file. 
        /// More than one record description entry can be specified; each is an alternative description of the same record storage area.
        /// Data areas described in the FILE SECTION are not available for processing unless the file that contains the data area is open.
        /// </summary>
        public IList<DataDescriptionEntry> RecordDescriptions { get; set; }
    }    
}
