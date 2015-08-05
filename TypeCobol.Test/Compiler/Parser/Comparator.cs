using System.Collections.Specialized;
using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using TypeCobol.Compiler;
using TypeCobol.Compiler.AntlrUtils;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Parser
{
    internal class TestUnit
    {
        private CompilationUnit unit = null;
        public FilesComparator comparator;
        public DocumentFormat format;

        public TestUnit(string name, bool debug = false)
        {
            this.format = new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
            this.comparator = new FilesComparator(name, debug);
        }

        public void Parse()
        {
            DirectoryInfo localDirectory = new DirectoryInfo(comparator.sample.full.folder);
            TypeCobolOptions options = new TypeCobolOptions();
            CompilationProject project = new CompilationProject("TEST",
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                this.format.Encoding, this.format.EndOfLineDelimiter, this.format.FixedLineLength, this.format.ColumnsLayout, options);
            string filename = comparator.sample.project.file;
            this.unit = new CompilationUnit(null, filename, project.SourceFileProvider, project, this.format.ColumnsLayout, new TypeCobolOptions());
            this.unit.SetupCodeAnalysisPipeline(null, 0);
            this.unit.StartDocumentProcessing();
        }

        public void Compare()
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(comparator.result.project.path)))
            {
                this.comparator.Compare(this.unit.SyntaxDocument.CodeElements, this.unit.SyntaxDocument.Diagnostics, reader);
            }
        }
    }

    internal class FolderTester
    {
        private IList<string> samples;
        private FilesComparator finder = new FilesComparator("whatever");

        internal FolderTester(string folder = null, string[] ignored = null)
        {
            string root = CreateSamplesRoot(folder);
            string[] paths = Directory.GetFiles(root, "*.cbl", SearchOption.AllDirectories);
            this.samples = Filter(paths, (ignored != null ? ignored : new string[0]), folder);
        }

        private string CreateSamplesRoot(string folder)
        {
            string root = finder.sample.full.folder;
            if (folder != null) root += Path.DirectorySeparatorChar + folder;
            return root;
        }

        private IList<string> Filter(string[] paths, string[] ignored, string folder)
        {
            var names = new List<string>();
            for (int c = 0; c < paths.Length; c++)
            {
                string name = GetName(paths[c]);
                string shortname = name.Remove(0, folder.Length +1);
                if (!ignored.Contains(shortname)) names.Add(name);
            }
            return names;
        }

        private string GetName(string path)
        {
            string root = finder.sample.full.folder;
            string name = path.Remove(0, root.Length + 1);
            return name.Remove(name.Length - finder.sextension.Length);
        }

        public void Test(bool debug = false)
        {
            foreach (var sample in this.samples)
            {
                var unit = new TestUnit(sample, debug);
                //unit.comparator = new Outputter(name);
                unit.Parse();
                unit.Compare();
            }
        }
    }





    internal interface Comparator
    {
        void Compare(IList<CodeElement> elements, IList<Diagnostic> diagnostics, StreamReader expected);
    }

    internal class FilesComparator : Comparator
    {
        internal string name;
        internal bool debug;
        internal string root = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar;

        internal FilesComparator(string name, bool debug = false)
        {
            this.name = name;
            this.debug = debug;
        }
        internal string sextension = ".cbl";
        internal string rextension = ".txt";
        internal AbstractPath sample
        {
            get { return new AbstractPath(root + "Samples" + Path.DirectorySeparatorChar, name, sextension); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
        internal AbstractPath result
        {
            get { return new AbstractPath(root + "ResultFiles" + Path.DirectorySeparatorChar, name, rextension); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }

        public virtual void Compare(IList<CodeElement> elements, IList<Diagnostic> diagnostics, StreamReader expected)
        {
            string result = ParserUtils.DumpResult(elements, diagnostics);
            if (this.debug) Console.WriteLine("\"" + this.name + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(this.name, result, expected);
        }
    }

    internal class Outputter : FilesComparator
    {
        internal Outputter(string name, bool debug = false)
            : base(name, debug) {}

        public override void Compare(IList<CodeElement> elements, IList<Diagnostic> diagnostics, StreamReader expected)
        {
            foreach (var e in elements)
            {
                if (e.GetType() == typeof(SentenceEnd)) continue;
                string line = expected.ReadLine();
                TestLine(e, line);
            }
        }

        private static void TestLine(CodeElement e, string line)
        {
            Console.WriteLine("TODO TestLine( " + e + " , \"" + line + "\")");
        }
    }

    internal class AbstractPath
    {
        internal string root;
        internal string name;
        internal string extension;
        internal AbstractPath(string root, string name, string extension)
        {
            this.root = root;
            this.name = name;
            this.extension = extension;
        }
        private string path
        {
            get { return root + name + extension; }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
        internal PathElement project
        {
            get { return new PathElement(path); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
        internal PathElement full
        {
            get { return new PathElement(PlatformUtils.GetPathForProjectFile(path)); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
        public override string ToString() { return project.ToString(); }
    }

    internal class PathElement
    {
        internal string path;
        internal PathElement(string path)
        {
            this.path = path;
        }
        internal string folder
        {
            get { return Path.GetDirectoryName(path); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
        internal string file
        {
            get { return Path.GetFileName(path); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
        public override string ToString() { return path; }
    }
}
