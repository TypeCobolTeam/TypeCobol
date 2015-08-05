using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
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
        public System.Type comparator = typeof(FilesComparator);
        public Names resultnames;

        internal FolderTester() : this(null, null, null, true) { }
        internal FolderTester(string folder) : this(folder, null, null, true) { }
        internal FolderTester(string[] ignored) : this(null, ignored, null, true) { }
        internal FolderTester(Names resultnames) : this(null, null, resultnames, true) { }
        internal FolderTester(bool deep) : this(null, null, null, deep) { }
        internal FolderTester(string folder, string[] ignored) : this(folder, ignored, null, true) { }
        internal FolderTester(string folder, Names namecreator) : this(folder, null, namecreator, true) { }
        internal FolderTester(string folder, bool deep) : this(folder, null, null, deep) { }
        internal FolderTester(string[] ignored, Names resultnames) : this(null, ignored, resultnames, true) { }
        internal FolderTester(string[] ignored, bool deep) : this(null, ignored, null, deep) { }
        internal FolderTester(Names resultnames, bool deep) : this(null, null, resultnames, deep) { }
        internal FolderTester(string folder, string[] ignored, Names resultnames) : this(folder, ignored, resultnames, true) { }
        internal FolderTester(string folder, string[] ignored, bool deep) : this(folder, ignored, null, deep) { }
        internal FolderTester(string folder, Names resultnames, bool deep) : this(folder, null, resultnames, deep) { }
        internal FolderTester(string[] ignored, Names resultnames, bool deep) : this(null, ignored, resultnames, deep) { }
        internal FolderTester(string folder, string[] ignored, Names resultnames, bool deep)
        {
            this.resultnames = resultnames;
            if(this.resultnames == null) this.resultnames = new DummyNames();

            string root = CreateSamplesRoot(folder);
            string[] paths = Directory.GetFiles(root, "*.cbl", (deep ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly));
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
                string shortname = name;
                if (folder != null) shortname = name.Remove(0, folder.Length + 1);
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
            System.Reflection.ConstructorInfo c = comparator.GetConstructor(new[] { typeof(string), typeof(Names), typeof(bool) });

            if (this.samples.Count < 1) throw new System.Exception("No sample file!");
            foreach (var sample in this.samples)
            {
                var unit = new TestUnit(sample, debug);
                unit.comparator = (FilesComparator)c.Invoke(new object[] { sample, this.resultnames, debug });
                if (!System.IO.File.Exists(unit.comparator.result.full.path))
                    if (resultnames.ErrorOnMissingResultFile())
                        throw new System.IO.FileNotFoundException("Missing file \"" + unit.comparator.result.project.path + "\"");
                    else continue; // SKIP test because there is no result file
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
        internal Names resultnames;
        internal string name;
        internal bool debug;
        internal string root = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar;

        public FilesComparator(string name) : this(name, null, false) { }
        public FilesComparator(string name, Names resultnames) : this(name, resultnames, false) { }
        public FilesComparator(string name, bool debug) : this(name, null, debug) { }
        public FilesComparator(string name, Names resultnames = null, bool debug = false)
        {
            this.resultnames = resultnames;
            if (this.resultnames == null) this.resultnames = new DummyNames();
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
            get { return new AbstractPath(root + "ResultFiles" + Path.DirectorySeparatorChar, resultnames.CreateName(name), rextension); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }

        public virtual void Compare(IList<CodeElement> elements, IList<Diagnostic> diagnostics, StreamReader expected)
        {
            string result = ParserUtils.DumpResult(elements, diagnostics);
            if (this.debug) System.Console.WriteLine("\"" + this.name + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(this.name, result, expected);
        }
    }

    internal class ArithmeticComparator : FilesComparator
    {
        public ArithmeticComparator(string name) : this(name, null, false) { }
        public ArithmeticComparator(string name, Names resultnames) : this(name, resultnames, false) { }
        public ArithmeticComparator(string name, bool debug) : this(name, null, debug) { }
        public ArithmeticComparator(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames, debug) { }

        public override void Compare(IList<CodeElement> elements, IList<Diagnostic> diagnostics, StreamReader expected)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            if (elements.Count < 1) throw new System.Exception("No CodeElements found!");
            foreach (var e in elements)
            {
                var statement = e as ArithmeticOperationStatement;
                if (statement == null) continue;
                string rpn = expected.ReadLine();
                if (rpn == null) errors.AppendFormat("RPN number {0} not provided.", c);
                string dump = ToString(statement);
                if (dump != rpn) errors.AppendFormat("line {0}: \"{1}\", expected \"{2}\"\n", c, dump, rpn);
                c++;
            }
            if (expected.ReadLine() != null) errors.AppendLine("Number of CodeElements (" + c + ") lesser than expected.");
            if (errors.Length > 0) throw new System.Exception(errors.ToString());
        }

        private string ToString(ArithmeticOperationStatement statement)
        {
            StringBuilder builder = new StringBuilder();
            foreach (var pair in statement.affectations)
            {
                builder.AppendFormat("{0} = {1}, ", pair.Key.Symbol.NameToken.Text, pair.Value.ToString());
            }
            if (statement.affectations.Count > 0) builder.Length -= 2;
            return builder.ToString();
        }
    }

    internal class NYComparator : FilesComparator
    {
        public NYComparator(string name) : this(name, null, false) { }
        public NYComparator(string name, Names resultnames) : this(name, resultnames, false) { }
        public NYComparator(string name, bool debug) : this(name, null, debug) { }
        public NYComparator(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames, debug) { }

        public override void Compare(IList<CodeElement> elements, IList<Diagnostic> diagnostics, StreamReader expected)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            System.Console.WriteLine("ELEMENTS="+elements.Count+" ERRORS="+diagnostics.Count);
            foreach (var e in elements)
            {
                if ((e as SentenceEnd) != null) continue;
                string line = expected.ReadLine();
                if (line != "Y") errors.AppendFormat("line {0}: \"{1}\", expected \"Y\"\n", c, line);
                c++;
            }
            foreach (var d in diagnostics)
            {
                string line = expected.ReadLine();
                if (line != "N") errors.AppendFormat("line {0}: \"{1}\", expected \"N\"\n", c, line);
                c++;
            }
            if (expected.ReadLine() != null) errors.AppendLine("Number of CodeElements (" + c + ") lesser than expected.");
            if (errors.Length > 0) throw new System.Exception(errors.ToString());
        }
    }

    internal class Outputter : FilesComparator
    {
        public Outputter(string name) : this(name, null, false) { }
        public Outputter(string name, Names resultnames) : this(name, resultnames, false) { }
        public Outputter(string name, bool debug) : this(name, null, debug) { }
        public Outputter(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames, debug) { }

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
            System.Console.WriteLine("TODO TestLine( " + e + " , \"" + line + "\")");
        }
    }



    internal interface Names
    {
        string CreateName(string name);
        bool ErrorOnMissingResultFile();
    }

    internal class DummyNames : Names
    {
        public string CreateName(string name) { return name; }
        public bool ErrorOnMissingResultFile() { return true; }
    }

    internal class RPNNames : Names
    {
        public string CreateName(string name) { return name + "RPN"; }
        public bool ErrorOnMissingResultFile() { return false; }
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
