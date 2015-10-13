using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
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
        private FileCompiler compiler = null;
        public FilesComparator comparator;
        public TestObserver observer;

        public TestUnit(string name, bool debug = false)
        {
            this.comparator = new FilesComparator(name, debug);
            this.observer = new TestObserver();
        }

        public void Init()
        {
            DirectoryInfo localDirectory = new DirectoryInfo(comparator.paths.sample.full.folder);
            DocumentFormat format = comparator.getSampleFormat();
            TypeCobolOptions options = new TypeCobolOptions();
            CompilationProject project = new CompilationProject("TEST",
                localDirectory.FullName, new string[] { "*.cbl", "*.cpy" },
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
            string filename = comparator.paths.sample.project.file;
            this.compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, new TypeCobolOptions(), false);
        }

        public void Parse()
        {
            try
            {
                this.compiler.CompileOnce();
            }
            catch(Exception e)
            {
                this.observer.OnError(e);
            }
        }

        public void Compare()
        {
            using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(comparator.paths.result.project.path)))
            {
                CodeElementsDocument parserResult = this.compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot;
                this.comparator.Compare(parserResult.CodeElements, parserResult.ParserDiagnostics, reader);
            }
        }
    }

    internal class TestObserver
    {
        private IList<System.Exception> errors = new List<System.Exception>();
        public bool HasErrors
        {
            get { return errors.Count > 0; }
        }
        public string DumpErrors()
        {
            var str = new StringBuilder();
            foreach (var error in errors) str.AppendLine(error.ToString());
            return str.ToString();
        }
        public void OnError(System.Exception error) { errors.Add(error); }
    }

    internal class FolderTester
    {
        private static readonly IList<Names> Names = new List<Names>
            {
                new EmptyName(),
                new CodeElementName(),
                new RPNName(),
                new NYName(),
            };

        private IList<string> samples;
        private FilesComparator finder = new FilesComparator("whatever");

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
            string root = CreateSamplesRoot(folder);
            string[] paths = Directory.GetFiles(root, "*.cbl", (deep ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly));
            this.samples = Filter(paths, (ignored != null ? ignored : new string[0]), folder);
        }

        private string CreateSamplesRoot(string folder)
        {
            string root = finder.paths.sample.full.folder;
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
            string root = finder.paths.sample.full.folder;
            string name = path.Remove(0, root.Length + 1);
            return name.Remove(name.Length - finder.paths.sextension.Length);
        }

        public void Test(bool debug = false)
        {
            if (this.samples.Count < 1) throw new System.Exception("No sample file!");
            var errors = new StringBuilder();
            foreach (var sample in this.samples)
            {
                IList<FilesComparator> comparators = GetComparators(sample, debug);
                if (comparators.Count < 1)
                {
                    System.Console.Write("\nERROR: Missing result file \"" + sample + "\"");
                    errors.AppendLine("Missing result file \"" + sample + "\"");
                    continue;
                }
                foreach (var comparator in comparators)
                {
                    System.Console.Write("\nCheck result file \"" + comparator.paths.result.full.path + "\" with " + comparator);
                    var unit = new TestUnit(sample, debug);
                    unit.comparator = comparator;
                    unit.Init();
                    try
                    {
                        unit.Parse();
                        if (unit.observer.HasErrors)
                        {
                            System.Console.Write(" --- EXCEPTION\n" + unit.observer.DumpErrors());
                            errors.AppendLine(unit.observer.DumpErrors());
                        }
                        unit.Compare();
                    }
                    catch (System.Exception ex)
                    {
                        System.Console.Write(" --- MISMATCH\n" + ex.Message);
                        errors.Append("E");
                    }
                }
            }
            if (errors.Length > 0) throw new System.Exception(errors.ToString());
        }

        private IList<FilesComparator> GetComparators(string sample, bool debug)
        {
            IList<FilesComparator> comparators = new List<FilesComparator>();
            foreach (var names in Names)
            {
                var paths = new Paths(sample, names);
                if (System.IO.File.Exists(paths.result.full.path))
                {
                    System.Type type = names.GetComparatorType();
                    System.Reflection.ConstructorInfo constructor = type.GetConstructor(new[] { typeof(string), typeof(Names), typeof(bool) });
                    comparators.Add((FilesComparator)constructor.Invoke(new object[] { sample, names, debug }));
                }
            }
            return comparators;
        }
    }





    internal interface Comparator
    {
        void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected);
    }

    internal class FilesComparator : Comparator
    {
        internal Paths paths;
        internal bool debug;

        public FilesComparator(string name) : this(name, null, false) { }
        public FilesComparator(string name, Names resultnames) : this(name, resultnames, false) { }
        public FilesComparator(string name, bool debug) : this(name, null, debug) { }
        public FilesComparator(string name, Names resultnames = null, bool debug = false)
        {
            this.paths = new Paths(name, resultnames != null? resultnames : new EmptyName());
            this.debug = debug;
        }

        public virtual void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
        {
            string result = ParserUtils.DumpResult(elements, diagnostics);
            if (this.debug) System.Console.WriteLine("\"" + this.paths.name + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(this.paths.name, result, expected);
        }

        internal DocumentFormat getSampleFormat()
        {
            if (paths.sample.name.Contains(".rdz"))
                return DocumentFormat.RDZReferenceFormat;
            return new DocumentFormat(Encoding.UTF8, EndOfLineDelimiter.CrLfCharacters, 0, ColumnsLayout.FreeTextFormat);
        }
    }

    internal class ArithmeticComparator : FilesComparator
    {
        public ArithmeticComparator(string name) : this(name, null, false) { }
        public ArithmeticComparator(string name, Names resultnames) : this(name, resultnames, false) { }
        public ArithmeticComparator(string name, bool debug) : this(name, null, debug) { }
        public ArithmeticComparator(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames, debug) { }

        public override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            bool elementsFound = false;
            foreach (var e in elements)
            {
                elementsFound = true;
                var statement = e as ArithmeticOperationStatement;
                if (statement == null) continue;
                string rpn = expected.ReadLine();
                if (rpn == null) errors.AppendFormat("RPN number {0} not provided.", c);
                string dump = ToString(statement);
                if (dump != rpn) errors.AppendFormat("line {0}: \"{1}\", expected \"{2}\"\n", c, dump, rpn);
                c++;
            }
            if(!elementsFound) throw new System.Exception("No CodeElements found!");
            if (expected.ReadLine() != null) errors.AppendLine("Number of CodeElements (" + c + ") lesser than expected.");
            if (errors.Length > 0)
            {
                errors.Insert(0, this.paths.name + ":\n");
                throw new System.Exception(errors.ToString());
            }
        }

        private string ToString(ArithmeticOperationStatement statement)
        {
            StringBuilder builder = new StringBuilder();
            foreach (var pair in statement.Affectations)
            {
                builder.AppendFormat("{0} = {1}, ", pair.Key.ToString(), pair.Value.ToString());
            }
            if (statement.Affectations.Count > 0) builder.Length -= 2;
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

        public override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            foreach (var e in elements)
            {
                if ((e as SentenceEnd) != null) continue;
                string line = expected.ReadLine();
                if (line != "Y") errors.AppendFormat("line {0}: \"Y\", expected \"{1}\"\n", c, line);
                c++;
            }
            foreach (var d in diagnostics)
            {
                string line = expected.ReadLine();
                if (line != "N") errors.AppendFormat("line {0}: \"N\", expected \"{1}\"\n", c, line);
                c++;
            }
            if (expected.ReadLine() != null) errors.AppendLine("Number of CodeElements (" + c + ") lesser than expected.");
            if (errors.Length > 0)
            {
                errors.Insert(0, this.paths.name + ":\n");
                throw new System.Exception(errors.ToString());
            }
        }
    }

    internal class Outputter : FilesComparator
    {
        public Outputter(string name) : this(name, null, false) { }
        public Outputter(string name, Names resultnames) : this(name, resultnames, false) { }
        public Outputter(string name, bool debug) : this(name, null, debug) { }
        public Outputter(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames, debug) { }

        public override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
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
        System.Type GetComparatorType();
    }

    internal class EmptyName : Names
    {
        public string CreateName(string name) { return name; }
        public System.Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class CodeElementName : Names
    {
        public string CreateName(string name) { return name + "CodeElements"; }
        public System.Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class RPNName : Names
    {
        public string CreateName(string name) { return name + "RPN"; }
        public System.Type GetComparatorType() { return typeof(ArithmeticComparator); }
    }

    internal class NYName : Names
    {
        public string CreateName(string name) { return name + "NY"; }
        public System.Type GetComparatorType() { return typeof(NYComparator); }
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


    internal class Paths
    {
        private readonly string Root = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar;
        internal string name;
        private Names resultnames;

        public Paths(string name, Names resultnames)
        {
            this.name = name;
            this.resultnames = resultnames;
        }
        internal string sextension = ".cbl";
        internal string rextension = ".txt";
        internal AbstractPath sample
        {
            get { return new AbstractPath(Root + "Samples" + Path.DirectorySeparatorChar, name, sextension); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
        internal AbstractPath result
        {
            get { return new AbstractPath(Root + "ResultFiles" + Path.DirectorySeparatorChar, resultnames.CreateName(name), rextension); }
            set { throw new System.InvalidOperationException("Can't touch this!"); }
        }
    }
}
