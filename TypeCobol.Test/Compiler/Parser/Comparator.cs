using System;
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
        internal FileCompiler Compiler;
        public readonly FilesComparator Comparator;
        public readonly TestObserver Observer;

        public TestUnit(FilesComparator comparator1, bool debug = false)
        {
            Comparator = comparator1;
            Observer = new TestObserver();
        }

        public void Init(string[] extensions = null)
        {
            DirectoryInfo localDirectory = new DirectoryInfo(Path.GetDirectoryName( Comparator.paths.SamplePath));
            DocumentFormat format = Comparator.getSampleFormat();
            TypeCobolOptions options = new TypeCobolOptions();
            if (extensions == null) extensions = new[] { "*.cbl", "*.cpy" };
            //comparator.paths.sextension = extensions[0].Substring(1);
            CompilationProject project = new CompilationProject("TEST",
                localDirectory.FullName, extensions,
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
            string filename = Comparator.paths.SampleName;
            Compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, options, null, false);
        }

		public void Parse() {
			try { Compiler.CompileOnce(); }
			catch(Exception e) { Observer.OnError(e); }
		}

		public string ToJSON() {
			return new TestJSONSerializer().ToJSON(Compiler.CompilationResultsForProgram.CodeElementsDocumentSnapshot.CodeElements);
		}

		public void Compare() {
            
            using (StreamReader reader = new StreamReader(new FileStream(Comparator.paths.Result, FileMode.Open))) {
				Comparator.Compare(Compiler.CompilationResultsForProgram, reader);
			}
		}
    }

    internal class TestObserver
    {
        private readonly IList<Exception> errors = new List<Exception>();
        public bool HasErrors { get { return errors.Count > 0; } }

        public string DumpErrors()
        {
            var str = new StringBuilder();
            foreach (var error in errors) str.AppendLine(error.ToString());
            return str.ToString();
        }
        public void OnError(Exception error) { errors.Add(error); }
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
        private string[] extensions;

        private string _sampleRoot;
        private string _resultsRoot;

		internal FolderTester(string sampleRoot, string resultsRoot,string folder, string[] extensions, string[] ignored = null, bool deep = true) {
			_sampleRoot = sampleRoot;
			_resultsRoot = resultsRoot;

			this.extensions = extensions;
			var option = deep? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly;
			string[] samples = new string[0];
			foreach(var ext in this.extensions) {
				string[] paths = Directory.GetFiles(folder, ext, option);
				var tmp = new string[samples.Length+paths.Length];
				samples.CopyTo(tmp, 0);
				paths.CopyTo(tmp, samples.Length);
				samples = tmp;
			}
			this.samples = Filter(samples, (ignored ?? new string[0]));
		}

        private IList<string> Filter(string[] paths, string[] ignored)
        {
            var names = new List<string>();
            foreach (string path in paths)
            {
                string shortname = Path.GetFileNameWithoutExtension(path);
                if (!ignored.Contains(shortname)) names.Add(path);
            }
            return names;
        }

        /// <summary>
        /// Return the number of test runs
        /// </summary>
        /// <returns></returns>
        public int GetTestCount()
        {
            return samples.Count;
        }

		public void Test(bool debug = false, bool json = false) {
			var errors = new StringBuilder();
			foreach (var samplePath in samples) {
				IList<FilesComparator> comparators = GetComparators(_sampleRoot, _resultsRoot, samplePath, debug);
				if (comparators.Count < 1) {
					Console.WriteLine(" /!\\ ERROR: Missing result file \"" + samplePath + "\"");
					errors.AppendLine("Missing result file \"" + samplePath + "\"");
					continue;
				}
				foreach (var comparator in comparators) {
					Console.WriteLine(comparator.paths.Result + " checked with " + comparator.GetType().Name);
					var unit = new TestUnit(comparator, debug);
					unit.Init(extensions);
					unit.Parse();
				    if (unit.Observer.HasErrors)
				    {
				        Console.WriteLine(" /!\\ EXCEPTION\n" + unit.Observer.DumpErrors());
				        errors.AppendLine(unit.Observer.DumpErrors());
				    }
				    else
				    {
				        if (json)
				        {
				            string filename = comparator.paths.Result;
				            //string name = Path.GetFileName(filename);
				            string extension = Path.GetExtension(filename);
				            filename = filename.Substring(0, filename.Length - extension.Length);
				            string[] lines = {unit.ToJSON()};
				            System.IO.File.WriteAllLines(filename + ".json", lines);
				        }

				        try
				        {
				            unit.Compare();
				        }
				        catch (Exception ex)
				        {
				            Console.WriteLine(" /!\\ MISMATCH\n" + ex);
				            errors.Append("E");
				        }
				    }
				}
			}
			if (errors.Length > 0) throw new Exception(errors.ToString());
		}

		private IList<FilesComparator> GetComparators(string sampleRoot, string resultsRoot, string samplePath, bool debug)	{
			IList<FilesComparator> comparators = new List<FilesComparator>();
			foreach (var names in Names) {
				Paths path = new Paths(sampleRoot, resultsRoot, samplePath, names);
				if (!System.IO.File.Exists(path.Result)) continue;

				Type type = names.GetComparatorType();
				System.Reflection.ConstructorInfo constructor = type.GetConstructor(new[] { typeof(Paths), typeof(bool) });
				comparators.Add((FilesComparator)constructor.Invoke(new object[] { path, debug }));
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

	    public FilesComparator(string name, bool debug) /*: this(name, null, debug)*/
	    {
	        
	    }
		public FilesComparator(Paths path, bool debug = false)
		{
		    paths = path;
			this.debug = debug;
		}

		public virtual void Compare(CompilationUnit result, StreamReader reader) {
			Compare(result.CodeElementsDocumentSnapshot.CodeElements, result.CodeElementsDocumentSnapshot.ParserDiagnostics, reader);
		}

		internal virtual void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected) {
			string result = ParserUtils.DumpResult(elements, diagnostics);
			if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected);
		}

		internal DocumentFormat getSampleFormat() {
			if (paths.SamplePath.Contains(".rdz"))
				return DocumentFormat.RDZReferenceFormat;
			return DocumentFormat.FreeUTF8Format;
		}
	}

internal class ArithmeticComparator : FilesComparator {
	public ArithmeticComparator(Paths path, bool debug = false) : base(path, debug) { }
	internal override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> diagnostics, StreamReader expected) {
		var errors = new System.Text.StringBuilder();
		int c = 0, line = 1;
		foreach(var e in elements) {
			c++;
			var operation = e as ArithmeticStatement;
			if (operation == null) continue;
			string rpn = expected.ReadLine();
			if (rpn == null) errors.AppendFormat("RPN number {0} not provided.", line);
			string dump = ToString(operation);
			if (dump != rpn) errors.AppendFormat("line {0}: \"{1}\", expected \"{2}\"\n", line, dump, rpn);
			line++;
		}
		if(c < 1) throw new Exception("No CodeElements found!");
		if (expected.ReadLine() != null) errors.AppendLine("Number of CodeElements ("+(line-1)+") lesser than expected.");
		if (errors.Length > 0) {
			errors.Insert(0, paths.SamplePath + ":\n");
			throw new Exception(errors.ToString());
		}
	}
	private string ToString(ArithmeticStatement statement) {
		if (statement == null) return null;
		var str = new System.Text.StringBuilder();
		foreach(var operations in statement.Affectations)
			foreach(var operation in operations.Value)
				str.Append(operations.Key).Append(" = ").Append(operation).Append(", ");
		if (statement.Affectations.Count > 0) str.Length -= 2;
		return str.ToString();
	}
}

    internal class NYComparator : FilesComparator
    {
        public NYComparator(Paths path, bool debug = false) : base(path, debug)
        {
        }

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
                errors.Insert(0, paths.SamplePath + ":\n");
                throw new Exception(errors.ToString());
            }
        }
    }

    internal class Outputter : FilesComparator
    {

        public Outputter(Paths path, bool debug = false) : base(path, debug) { }

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
            Console.WriteLine("TODO TestLine( " + e + " , \"" + line + "\")");
        }
    }

    internal class Multipass : FilesComparator
    {
        public Multipass(Paths path, bool debug = false) : base(path, debug) { }
        
        internal class IndexNames : Names
        {
            internal int index = 0;
            public string CreateName(string name) { return name+'.'+index; }
            public Type GetComparatorType() { return typeof(Multipass); }
        }
    }

    internal class ProgramsComparator : FilesComparator
    {
        public ProgramsComparator(Paths path, bool debug = false) : base(path, debug)
        {
        }

        public override void Compare(CompilationUnit result, StreamReader reader)
        {
            ProgramClassDocument pcd = result.ProgramClassDocumentSnapshot;
            List<Diagnostic> diagnostics = new List<Diagnostic>();
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
            if (debug) Console.WriteLine("\"" + paths.SamplePath+ "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected);
        }
    }

	internal class MemoryComparator: FilesComparator
	{
	    public MemoryComparator(Paths path, bool debug = false) : base(path, debug)
	    {
	    }

	    public override void Compare(CompilationUnit result, StreamReader reader) {
			ProgramClassDocument pcd = result.ProgramClassDocumentSnapshot;
			Compare(pcd.Program.SymbolTable, reader);
		}

		internal void Compare(SymbolTable table, StreamReader expected) {
			string result = Dump(table);
			if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected);
		}

		private string Dump(SymbolTable table) {
			var str = new StringBuilder();
			str.AppendLine("--------- FIELD LEVEL/NAME ---------- START     END  LENGTH");
			foreach(var line in table.DataEntries) {
				foreach(var data in line.Value) {
//TODO#249 print memory representation
//					if (data is DataDefinition && ((DataDefinition)data).CodeElement().LevelNumber.Value == 1)
//					if (data.LevelNumber.Value == 1) Dump(str, data, 0);
				}
			}
			return str.ToString();
		}
		private void Dump(StringBuilder str, object data, int indent, string indexes = "", int baseaddress = 1) {
/*TODO#249
			long level = data.LevelNumber.Value;
			string name = (data.DataName != null?data.DataName.Name:"?");
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
*/
		}
		private string CreateLine(long level, string name, int offset, int length, int index, int max, int indent, string strindexes = "") {
			var res = new StringBuilder();
			BeginFirstColumn(res, indent, level, name);
			EndFirstColumn(res, strindexes, index, max);
			EndLine(res, offset, length);
			return res.ToString();
		}
		private void BeginFirstColumn(StringBuilder str, int indent, long level, string name) {
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
			return prefix + (index >= 0?((prefix.Length > 0?",":"")+(index+1)):"");
		}
	}



    internal interface Names
    {
        string CreateName(string name);
        Type GetComparatorType();
    }

    internal class EmptyName : Names
    {
        public string CreateName(string name) { return name; }
        public Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class CodeElementName : Names
    {
        public string CreateName(string name) { return name + "CodeElements"; }
        public Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class RPNName : Names
    {
        public string CreateName(string name) { return name + "RPN"; }
        public Type GetComparatorType() { return typeof(ArithmeticComparator); }
    }

    internal class NYName : Names
    {
        public string CreateName(string name) { return name + "NY"; }
        public Type GetComparatorType() { return typeof(NYComparator); }
    }

    internal class PGMName : Names
    {
        public string CreateName(string name) { return name + "PGM"; }
        public Type GetComparatorType() { return typeof(ProgramsComparator); }
    }

    internal class MemoryName : Names
    {
        public string CreateName(string name) { return name + "MEM"; }
        public Type GetComparatorType() { return typeof(MemoryComparator); }
    }

    internal class Paths
    {
        private readonly string _sampleRoot;
        private readonly string _resultRoot;

        /// <summary>
        /// The complete path to the sample file
        /// </summary>
        public string SamplePath { get; private set; }

        public Names Resultnames { get; private set; }

        public Paths(string sampleRoot, string resultRoot, string samplePath, Names resultnames)
        {
            _sampleRoot = sampleRoot;
            _resultRoot = resultRoot;
            SamplePath = samplePath;
            Resultnames = resultnames;
        }

        private const string Rextension = ".txt";


        /// <summary>
        /// Returns the sample filename with its extension
        /// </summary>
        internal string SampleName { get { return Path.GetFileName(SamplePath); } }

        /// <summary>
        /// Return the complete path to the result file
        /// </summary>
        internal string Result
        {
            get
            {
                string ResultFilePath = Path.GetDirectoryName(SamplePath).Substring(_sampleRoot.Length);
                string ResultFileName = Path.GetFileNameWithoutExtension(SamplePath);
                return _resultRoot + Path.DirectorySeparatorChar + ResultFilePath  + Path.DirectorySeparatorChar + Resultnames.CreateName(ResultFileName) + Rextension;
            }
            
        }
    }
}
