using System;
using System.Xml;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Tools;
using TypeCobol.Tools.Options_Config;

namespace TypeCobol.Server {
    interface ErrorWriter {
        void Write(ReturnCode returnCode);
        void FlushAndClose();
    }


    public abstract class AbstractErrorWriter : ErrorWriter {
        internal Dictionary<string, List<Diagnostic>> Errors { get; set; }
        internal Dictionary<string, string> Inputs { get; set; }
        public IList<string> Outputs { get; set; }

        public AbstractErrorWriter() {
            Errors = new Dictionary<string, List<Diagnostic>>();
            Inputs = new Dictionary<string, string>();
            Outputs = new List<string>();
        }

        protected List<Diagnostic> GetErrors(string key) {
            key = GetDefaultKeyIfNull(key);
            if (!Errors.TryGetValue(key, out var list))
            {
                list = new List<Diagnostic>();
                Errors[key] = list;
                Inputs[key] = GenerateNumber().ToString();
            }
            return list;
        }

        public void AddErrors(string key, IEnumerable<Diagnostic> errors) {
            GetErrors(key).AddRange(errors);
        }

        private string GetDefaultKeyIfNull(string key) {
            return key ?? @"_DEFAULT_";
        }

        public void AddErrors(string key, Diagnostic error) {
            GetErrors(key).Add(error);
        }

        private int count = 1;

        public int Count {
            get { return count; }
        }

        private int GenerateNumber()
        {
            return count++; 
        }

        public abstract void Write(ReturnCode returnCode);
        public abstract void FlushAndClose();
    }

    public class XMLWriter : AbstractErrorWriter {
        private XmlWriter writer;

        public XMLWriter(System.IO.TextWriter writer) {
            var settings = new XmlWriterSettings();
            settings.NewLineOnAttributes = false;
            settings.Indent = true;
            this.writer = XmlWriter.Create(writer, settings);
        }

        public override void Write(ReturnCode returnCode) {
            writer.WriteComment(" ReturnCode = " + returnCode +" ");
            writeHead();
            writeInputs();
            foreach (var key in Errors.Keys) {
                foreach (var error in Errors[key])
                    writeMessage(Inputs[key], error);
            }
            //writeOutputs();
            //writeLinesLookup();
            writeTail();
        }

        public override void FlushAndClose() {
            writer.Flush();
            writer.Close();
        }

        private void writeHead() {
            writer.WriteStartElement("BUILD");
            writer.WriteStartElement("PACKAGE");
        }

        private void writeTail() {
            writer.WriteEndElement(); // PACKAGE
            writer.WriteEndElement(); // BUILD
        }

        private void writeLinesLookup() {
            writer.WriteStartElement("STATEMENTTABLE");
            writer.WriteString("\n(2,1,2,1);\n");
            writer.WriteEndElement(); // STATEMENTTABLE
        }

        private void writeInputs() {
            writer.WriteStartElement("FILEREFERENCETABLE");
            writer.WriteElementString("FILECOUNT", Inputs.Count.ToString());
            foreach (var path in Inputs.Keys) {
                string fileName;
                try {
                    fileName = new FileInfo(path).FullName;
                } catch (Exception) {
                    fileName = path;
                }
                writeFile(Inputs[path], fileName);
            }
            writer.WriteEndElement(); // FILEREFERENCETABLE
        }

        private void writeOutputs() {
            writer.WriteStartElement("OUTFILEREFERENCETABLE");
            writer.WriteElementString("OUTFILECOUNT", Outputs.Count.ToString());
            int c = 1;
            foreach (var path in Outputs)
                writeFile((c++).ToString(), new System.IO.FileInfo(path).FullName, "OUT");
            writer.WriteEndElement(); // OUTFILEREFERENCETABLE
        }

        private void writeFile(string id, string name, string prefix = "") {
            writer.WriteStartElement(prefix + "FILE");
            writer.WriteElementString(prefix + "FILENUMBER", id);
            writer.WriteElementString(prefix + "FILENAME", name);
            writer.WriteEndElement(); // FILE
        }

        private void writeMessage(string id, Diagnostic error) {
            writer.WriteStartElement("MESSAGE");
            writer.WriteElementString("MSGNUMBER",
                "TC-" + error.Info.Code.ToString().PadLeft(5, '0') + AsIBMSuffix((int) error.Info.Severity));
            if (error.LineStart == error.LineEnd)
            {
                writer.WriteElementString("MSGLINE", error.LineStart.ToString());
            }
            else
            {
                writer.WriteElementString("MSGLINE", $"{error.LineStart}:{error.LineEnd}");
            }
            writer.WriteElementString("MSGFILE", id);
            writer.WriteElementString("MSGTEXT", error.Message);
            writer.WriteEndElement(); // MESSAGE
        }

        private static string AsIBMSuffix(int severity) {
            switch (severity) {
                // TODO: these depend on Compiler.Diagnostics.Severity int values
                case 3:
                    return "-I";
                case 2:
                    return "-W";
                default:
                    return "-E";
            }
        }
    }

    public class ConsoleWriter : AbstractErrorWriter {
        private System.IO.TextWriter writer;

        public ConsoleWriter(System.IO.TextWriter writer) {
            this.writer = writer;
        }

        public override void Write(ReturnCode returnCode) {
            writer.WriteLineAsync("ReturnCode=" + returnCode +"_");
            foreach (var key in Errors.Keys) {
                write(key, Errors[key]);
            }
        }

        private void write(string title, IList<Diagnostic> errors) {
            if (errors.Count > 0)
                writer.WriteAsync(errors.Count.ToString());
            else writer.WriteAsync("No");
            writer.WriteAsync(" error");
            if (errors.Count > 1) writer.WriteAsync('s');
            writer.WriteAsync(" in \"");
            writer.WriteAsync(title);
            if (errors.Count > 0) {
                writer.WriteLineAsync("\":");
                foreach (var e in errors) writer.WriteLine(e.ToString());
            } else {
                writer.WriteLineAsync("\".");
            }
        }

        public override void FlushAndClose() {
            writer.Flush();
            writer.Close();
        }
    }

    public class XmlDiagWriter : AbstractErrorWriter
    {
        private readonly XmlWriter _xmlWriter;

        public XmlDiagWriter(TextWriter textWriter)
        {
            var settings = new XmlWriterSettings
                           {
                               Indent = true
                           };
            _xmlWriter = XmlWriter.Create(textWriter, settings);
        }

        public override void Write(ReturnCode returnCode)
        {
            _xmlWriter.WriteStartDocument();
            _xmlWriter.WriteStartElement("build");
            _xmlWriter.WriteAttributeString("return-code", returnCode.ToString());
            WriteFileTable();
            WriteDiagnostics();
            _xmlWriter.WriteEndElement();
            _xmlWriter.WriteEndDocument();
        }

        private void WriteFileTable()
        {
            _xmlWriter.WriteStartElement("filetable");
            foreach (var input in Inputs)
            {
                _xmlWriter.WriteStartElement("file");
                _xmlWriter.WriteAttributeString("index", input.Value);
                _xmlWriter.WriteAttributeString("path", input.Key);
                _xmlWriter.WriteEndElement();
            }
            _xmlWriter.WriteEndElement();
        }

        private void WriteDiagnostics()
        {
            _xmlWriter.WriteStartElement("diagnostics");
            foreach (var error in Errors)
            {
                string fileIndex = Inputs[error.Key];
                foreach (var diagnostic in error.Value)
                {
                    _xmlWriter.WriteStartElement("diagnostic");
                    _xmlWriter.WriteAttributeString("file-index", fileIndex);

                    //range
                    _xmlWriter.WriteStartElement("range");
                    _xmlWriter.WriteAttributeString("line-start", diagnostic.LineStart.ToString());
                    _xmlWriter.WriteAttributeString("column-start", diagnostic.ColumnStart.ToString());
                    _xmlWriter.WriteAttributeString("line-end", diagnostic.LineEnd.ToString());
                    _xmlWriter.WriteAttributeString("column-end", diagnostic.ColumnEnd.ToString());
                    _xmlWriter.WriteEndElement();

                    //message
                    _xmlWriter.WriteElementString("message", diagnostic.Message);

                    //info
                    _xmlWriter.WriteStartElement("info");
                    _xmlWriter.WriteAttributeString("severity", diagnostic.Info.Severity.ToString());
                    _xmlWriter.WriteAttributeString("code", diagnostic.Info.Code.ToString());
                    _xmlWriter.WriteAttributeString("source", diagnostic.Info.ReferenceText);
                    _xmlWriter.WriteEndElement();

                    _xmlWriter.WriteEndElement();
                }
            }
            _xmlWriter.WriteEndElement();
        }

        public override void FlushAndClose()
        {
            _xmlWriter.Flush();
            _xmlWriter.Close();
        }
    }
}
