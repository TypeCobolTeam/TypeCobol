using System.Collections.Generic;

namespace TypeCobol.Server
{
    /// <summary>
    /// Config class that holds all the argument information like input files, output files, error file etc.
    /// </summary>
    public class Config
    {
        public TypeCobol.Compiler.DocumentFormat Format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;
        public bool Codegen = false;
        public bool AutoRemarks;
        public string HaltOnMissingCopyFilePath;
        public List<string> CopyFolders = new List<string>();
        public List<string> InputFiles = new List<string>();
        public List<string> OutputFiles = new List<string>();
        public string ErrorFile = null;
        public string skeletonPath = "";
        public bool IsErrorXML
        {
            get { return ErrorFile != null && ErrorFile.ToLower().EndsWith(".xml"); }
        }
        public List<string> Copies = new List<string>();

        public string EncFormat = null;
    }
}
