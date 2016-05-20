﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;

namespace TypeCobol.Test.Compiler.Parser
{
    internal class TestUnit
    {
        internal FileCompiler compiler = null;
        public FilesComparator comparator;
        public TestObserver observer;

        public TestUnit(string name, bool debug = false)
        {
            this.comparator = new FilesComparator(name, debug);
            this.observer = new TestObserver();
        }

        public void Init(string[] extensions = null)
        {
            DirectoryInfo localDirectory = new DirectoryInfo(comparator.paths.sample.full.folder);
            DocumentFormat format = comparator.getSampleFormat();
            TypeCobolOptions options = new TypeCobolOptions();
            if (extensions == null) extensions = new string[] { "*.cbl", "*.cpy" };
            comparator.paths.sextension = extensions[0].Substring(1);
            CompilationProject project = new CompilationProject("TEST",
                localDirectory.FullName, extensions,
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
            string filename = comparator.paths.sample.project.file;
            this.compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, options, null, false);
        }

		public void Parse() {
			try { this.compiler.CompileOnce(); }
			catch(Exception e) { this.observer.OnError(e); }
		}

		public string ToJSON() {
			return new TestJSONSerializer().ToJSON(this.compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.CodeElements);
		}

		public void Compare() {
			using (StreamReader reader = new StreamReader(PlatformUtils.GetStreamForProjectFile(comparator.paths.result.project.path))) {
				this.comparator.Compare(this.compiler.CompilationResultsForProgram, reader);
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
                new PGMName(),
                new MemoryName(),
            };

        private IList<string> samples;
        private FilesComparator finder = new FilesComparator("whatever");
        private string[] extensions;

        internal FolderTester(string folder, string[] extensions, string[] ignored = null, bool deep = true)
        {
            string root = CreateSamplesRoot(folder);
            this.extensions = extensions;
            string[] paths = Directory.GetFiles(root, extensions[0], (deep ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly));
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

		public void Test(bool debug = false, bool json = false) {
			if (this.samples.Count < 1) throw new System.Exception("No sample file!");
			var errors = new StringBuilder();
			foreach (var sample in this.samples) {
				IList<FilesComparator> comparators = GetComparators(sample, debug);
				if (comparators.Count < 1) {
					System.Console.WriteLine(" /!\\ ERROR: Missing result file \"" + sample + "\"");
					errors.AppendLine("Missing result file \"" + sample + "\"");
					continue;
				}
				foreach (var comparator in comparators) {
					System.Console.WriteLine("Check result file \"" + comparator.paths.result.full.path + "\" with " + comparator);
					var unit = new TestUnit(sample, debug);
					unit.comparator = comparator;
					unit.Init(this.extensions);
					unit.Parse();
					if (unit.observer.HasErrors) {
						System.Console.WriteLine(" /!\\ EXCEPTION\n" + unit.observer.DumpErrors());
						errors.AppendLine(unit.observer.DumpErrors());
					}

					if (json) {
						string filename = comparator.paths.result.full.ToString();
						string name = System.IO.Path.GetFileName(filename);
						string extension = System.IO.Path.GetExtension(filename);
						filename = filename.Substring(0, filename.Length - extension.Length);
						string[] lines = { unit.ToJSON() };
						System.IO.File.WriteAllLines(filename + ".json", lines);
					}

					try { unit.Compare(); }
					catch (System.Exception ex) {
						System.Console.WriteLine(" /!\\ MISMATCH\n" + ex);
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
                paths.sextension = extensions[0];
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
        void Compare(CompilationUnit result, StreamReader expected);
    }

	internal class FilesComparator : Comparator
	{
		internal Paths paths;
		internal bool debug;

		public FilesComparator(string name) : this(name, null, false) { }
		public FilesComparator(string name, Names resultnames) : this(name, resultnames, false) { }
		public FilesComparator(string name, bool debug) : this(name, null, debug) { }
		public FilesComparator(string name, Names resultnames = null, bool debug = false) {
			this.paths = new Paths(name, resultnames != null? resultnames : new EmptyName());
			this.debug = debug;
		}

		public virtual void Compare(CompilationUnit result, StreamReader reader) {
			Compare(result.CodeElementsDocumentSnapshot.CodeElements, result.CodeElementsDocumentSnapshot.ParserDiagnostics, reader);
		}

		internal virtual void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected) {
			string result = ParserUtils.DumpResult(elements, diagnostics);
			if (this.debug) System.Console.WriteLine("\"" + this.paths.name + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(this.paths.name, result, expected);
		}

		internal DocumentFormat getSampleFormat() {
			if (paths.sample.name.Contains(".rdz"))
				return DocumentFormat.RDZReferenceFormat;
			return DocumentFormat.FreeUTF8Format;
		}
	}

    internal class ArithmeticComparator : FilesComparator
    {
        public ArithmeticComparator(string name) : this(name, null, false) { }
        public ArithmeticComparator(string name, Names resultnames) : this(name, resultnames, false) { }
        public ArithmeticComparator(string name, bool debug) : this(name, null, debug) { }
        public ArithmeticComparator(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames, debug) { }

        internal override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
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

        internal override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
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

        internal override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
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

    internal class Multipass : FilesComparator
    {
        public Multipass(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames != null ? resultnames : new IndexNames(), debug) { }
        /*
        public override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected)
        {
            System.Console.WriteLine("pass="+paths.resultnames.CreateName(paths.name));
        }
        */
        internal class IndexNames : Names
        {
            internal int index = 0;
            public string CreateName(string name) { return name+'.'+index; }
            public System.Type GetComparatorType() { return typeof(Multipass); }
        }
    }

    internal class ProgramsComparator : FilesComparator
    {
        public ProgramsComparator(string name, Names resultnames = null, bool debug = false)
            : base(name, resultnames, debug) { }

        public override void Compare(CompilationUnit result, StreamReader reader)
        {
            ProgramClassDocument pcd = result.ProgramClassDocumentSnapshot;
            List<TypeCobol.Compiler.Diagnostics.Diagnostic> diagnostics = new List<TypeCobol.Compiler.Diagnostics.Diagnostic>();
            diagnostics.AddRange(result.CodeElementsDocumentSnapshot.ParserDiagnostics);
            diagnostics.AddRange(pcd.Diagnostics);
            foreach (var element in result.CodeElementsDocumentSnapshot.CodeElements) {
                diagnostics.AddRange(element.Diagnostics);
            }
            Compare(pcd.Program, pcd.Class, diagnostics, reader);
        }

        internal void Compare(Program program, Class cls, IList<Diagnostic> diagnostics, StreamReader expected)
        {
            string result = ParserUtils.DumpResult(program, cls, diagnostics);
            if (this.debug) System.Console.WriteLine("\"" + this.paths.name + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(this.paths.name, result, expected);
        }
    }

	internal class MemoryComparator: FilesComparator
	{
		public MemoryComparator(string name, Names resultnames = null, bool debug = false)
			: base(name, resultnames, debug) { }

		public override void Compare(CompilationUnit result, StreamReader reader) {
			ProgramClassDocument pcd = result.ProgramClassDocumentSnapshot;
			Compare(pcd.Program.SymbolTable, reader);
		}

		internal void Compare(SymbolTable table, StreamReader expected) {
			string result = Dump(table);
			if (this.debug) System.Console.WriteLine("\"" + this.paths.name + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(this.paths.name, result, expected);
		}

		private string Dump(SymbolTable table) {
			var str = new System.Text.StringBuilder();
			str.AppendLine("--------- FIELD LEVEL/NAME ---------- START     END  LENGTH");
			foreach(var line in table.DataEntries) {
				foreach(var data in line.Value) {
					if (data.LevelNumber == 1) Dump(str, data, 0);
				}
			}
			return str.ToString();
		}
		private void Dump(StringBuilder str, DataDescriptionEntry data, int indent, string indexes = "", int baseaddress = 1) {
			int level = data.LevelNumber;
			string name = (data.Name != null?data.Name.ToString():"?");
			if (data.MemoryArea is TableInMemory) {
				var table = data.MemoryArea as TableInMemory;
				foreach(var e in table) {
					str.AppendLine(CreateLine(level, name, e.Offset, e.Length, e.Index, table.Count, indent));
					string strindexes = CreateIndexes(indexes, e.Index);
					foreach(var child in data.Subordinates) Dump(str, child, indent+1, strindexes);
				}
			} else {
				str.AppendLine(CreateLine(level, name, data.MemoryArea.Offset, data.MemoryArea.Length, 0, 1, indent));
				foreach(var child in data.Subordinates) Dump(str, child, indent+1, indexes);
			}
		}
		private string CreateLine(int level, string name, int offset, int length, int index, int max, int indent, string strindexes = "") {
			var res = new System.Text.StringBuilder();
			BeginFirstColumn(res, indent, level, name);
			EndFirstColumn(res, strindexes, index, max);
			EndLine(res, offset, length);
			return res.ToString();
		}
		private void BeginFirstColumn(StringBuilder str, int indent, int level, string name) {
			for(int i=0; i<indent; i++) str.Append("  ");
			if (level > 1) str.Append(String.Format("{0,2} ", level));
			str.Append(name);
		}
		private void EndFirstColumn(StringBuilder str, string strprefix, int index, int max) {
			if (strprefix.Length > 0 || (index >= 0 && max > 1)) {
				str.Append('(').Append(CreateIndexes(strprefix,index)).Append(')');
			}
			while(str.Length < 36) str.Append(' ');
		}
		private void EndLine(StringBuilder str, int offset, int size, int baseaddress = 1) {
			str.Append(String.Format(" {0,6:0} ", baseaddress + offset));//start
			str.Append(String.Format(" {0,6:0} ", offset + size));//end
			str.Append(String.Format(" {0,6:0} ", size));//length
		}
		private string CreateIndexes(string prefix, int index) {
			return prefix + (index >= 0?((prefix.Length > 0?",":"")+(index+1).ToString()):"");
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

    internal class PGMName : Names
    {
        public string CreateName(string name) { return name + "PGM"; }
        public System.Type GetComparatorType() { return typeof(ProgramsComparator); }
    }

    internal class MemoryName : Names
    {
        public string CreateName(string name) { return name + "MEM"; }
        public System.Type GetComparatorType() { return typeof(MemoryComparator); }
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
        internal readonly string Root = "Compiler" + Path.DirectorySeparatorChar + "Parser" + Path.DirectorySeparatorChar;
        internal string name;
        internal Names resultnames;

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
