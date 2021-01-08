using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace TypeCobol.Compiler.Diagnostics
{   
    public enum Severity
    {
        Error = 1,
        Warning = 2,
        Info = 3       
    }

    public enum Category
    {
        TextFormat = 1,
        Tokens = 2,
        Directives = 3,
        Syntax = 4,
        Semantics = 5,
        Generation = 6,
        General = 7,
        CodeAnalysis = 8
    }

    public class DiagnosticMessage
    {
        internal DiagnosticMessage(Category category, int code, Severity severity, string messageTemplate, ReferenceDocument document, int pageNumber, string referenceText)
        {
            Category = category;
            Code = code;
            Severity = severity;
            MessageTemplate = messageTemplate;

            Document = document;
            PageNumber = pageNumber;
            ReferenceText = referenceText;
        }

        public Category Category { get; private set; }
        public int Code { get; private set; }
        public Severity Severity { get; private set; }

        internal string MessageTemplate { get; private set; }

        public ReferenceDocument Document { get; private set; }
        public int PageNumber { get; private set; }
        public string ReferenceText { get; private set; }

        // Load messages table DiagnosticMessages.csv in memory at startup

        public static IList<DiagnosticMessage> GetFromCode { get; private set; }

        static DiagnosticMessage()
        {
            GetFromCode = new List<DiagnosticMessage>();
            GetFromCode.Add(null); // to start message codes at index 1

            using (StreamReader tableReader = new StreamReader(GetStreamForProjectFile("Compiler/Diagnostics/Resources/DiagnosticMessages.csv")))
            {
                // Skip column headers
                tableReader.ReadLine();
                
                // One message per line
                string line = null;
                while((line = tableReader.ReadLine()) != null)
                {
                    string[] columns = line.Split(';');
                    // Category	
                    Category category = (Category)Int32.Parse(columns[0]);
                    // Code	
                    int code = Int32.Parse(columns[1]);
                    // Severity	
                    Severity severity = (Severity)Int32.Parse(columns[2]);
                    // MessageTemplate	
                    string messageTemplate = columns[3];
                    // Document	
                    int documentId = Int32.Parse(columns[4]);
                    ReferenceDocument referenceDocument = ReferenceDocument.GetFromCode[documentId];
                    // PageNumber	
                    int pageNumber = Int32.Parse(columns[5]);
                    // ReferenceText
                    string referenceText = columns[6];

                    GetFromCode.Add(new DiagnosticMessage(category, code, severity, messageTemplate, referenceDocument, pageNumber, referenceText));
                }
            }
        }

        /// <summary>
        /// If file "foo.txt" is stored in project subdirectory "bar",
        /// relativeFilePath input parameter should be "bar/foo.txt"
        /// </summary>
        private static Stream GetStreamForProjectFile(string relativeFilePath)
        {
            string resourcePath = String.Format("TypeCobol.{0}", relativeFilePath.Replace('/', '.'));
            Stream resourceStream = typeof(DiagnosticMessage).Assembly.GetManifestResourceStream(resourcePath);
            return resourceStream;
        }
     }

}
