using System;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Editor
{
    internal class Parser
    {
        public IObserver<TypeCobol.Compiler.Parser.CodeElementChangedEvent> Observer { get; private set; }
        private CompilationProject Project;
        private TypeCobolOptions Options;
        private CompilationUnit Unit;

        public Parser(string name)
        {
            this.Observer = new Observer();

            DirectoryInfo localDirectory = new DirectoryInfo(".");
            DocumentFormat format = DocumentFormat.FreeUTF8Format;
            this.Options = new TypeCobolOptions();
            this.Project = new CompilationProject(name,
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, this.Options);
        }

        public void Parse(ITextDocument document)
        {
            // TODO this could go in constructor if document could be set after a CopilationUnit creation
            Unit = new CompilationUnit(document, Project.SourceFileProvider, Project, Options);
            Unit.SetupCodeAnalysisPipeline(null, 0);
            Unit.SyntaxDocument.ParseNodeChangedEventsSource.Subscribe(this.Observer);

            Unit.StartDocumentProcessing();
        }

        public IEnumerable<CodeElement> CodeElements
        {
            get
            {
                return Unit.SyntaxDocument.CodeElements; // TODO test if compilation is done
            }
        }

        public IEnumerable<CodeElement> Errors
        {
            get
            {
                return Unit.SyntaxDocument.CodeElementsInError; // TODO test if compilation is done
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
