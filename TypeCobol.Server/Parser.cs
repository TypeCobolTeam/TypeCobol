using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Server
{
    public class Parser
    {
        public IObserver<TypeCobol.Compiler.Parser.CodeElementChangedEvent> Observer { get; private set; }
        private CompilationProject project;
        private TypeCobolOptions options;
        private FileCompiler Compiler = null;

        public Parser(string name)
        {
            this.Observer = new Observer();
        }

        private static DocumentFormat GetFormat(string filename) {
            return DocumentFormat.FreeUTF8Format;//TODO autodetect
        }

        public void Init(string path) {
            string directory = Directory.GetParent(path).FullName;
            string filename = Path.GetFileName(path);
            DirectoryInfo root = new DirectoryInfo(directory);
            DocumentFormat format = GetFormat(path);
            /*TypeCobolOptions */options = new TypeCobolOptions();
            /*CompilationProject */project = new CompilationProject(path, root.FullName, new string[] { "*.cbl", "*.cpy" },
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
            //Compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, options, false);
        }

        public void Parse(ITextDocument document)
        {
            Compiler = new FileCompiler(document, project.SourceFileProvider, project, options, false);
            try { Compiler.CompileOnce(); }
            catch(Exception ex) { Observer.OnError(ex); }
        }

        public IEnumerable<CodeElement> CodeElements
        {
            get
            {
                // TODO test if compilation is done
                if (Compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot == null) return new List<CodeElement>();
                return Compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.CodeElements;
            }
        }

        public IEnumerable<Diagnostic> Errors
        {
            get
            {
                // TODO test if compilation is done
                if (Compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot == null) return new List<Diagnostic>();
                return Compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.ParserDiagnostics;
            }
        }
    }



    internal class Observer : IObserver<CodeElementChangedEvent>
    {
        private IList<System.Exception> errors = new List<System.Exception>();
        public bool HasErrors
        {
            get { return errors.Count > 0; }
        }
        public string DumpErrors()
        {
            var str = new System.Text.StringBuilder();
            foreach (var error in errors) str.AppendLine(error.ToString());
            return str.ToString();
        }
        public void OnCompleted() { }
        public void OnError(System.Exception error) { errors.Add(error); }
        public void OnNext(TypeCobol.Compiler.Parser.CodeElementChangedEvent value) { }
    }
}
