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
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
using String = System.String;

namespace TypeCobol.Test.Utils
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

        public void Init(string[] extensions = null, bool autoRemarks = false, bool AntlrProfiler = false)
        {
            DirectoryInfo localDirectory = new DirectoryInfo(Path.GetDirectoryName( Comparator?.paths?.SamplePath));
            DocumentFormat format = Comparator?.GetSampleFormat();
            TypeCobolOptions options = new TypeCobolOptions();
#if EUROINFO_RULES
            options.AutoRemarksEnable = autoRemarks;
#endif
            if (extensions == null) extensions = new[] { ".cbl", ".cpy" };
            //comparator.paths.sextension = extensions[0].Substring(1);
            CompilationProject project = new CompilationProject("TEST",
                localDirectory.FullName, extensions,
                format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
            string filename = Comparator.paths.SampleName;
            Compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, options, null, false, project);

            if(AntlrProfiler)
            {
                Compiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.ActivateDetailedAntlrPofiling = true;
                Compiler.CompilationResultsForProgram.PerfStatsForTemporarySemantic.ActivateDetailedAntlrPofiling = true;
            }
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
				Comparator.Compare(Compiler.CompilationResultsForProgram, reader, Comparator.paths.Result);
            }
		}

        public void Compare(string parsingResult)
        {
            using (StreamReader reader = new StreamReader(new FileStream(Comparator.paths.Result, FileMode.Open)))
            {
                ParserUtils.CheckWithResultReader(Comparator.paths.SamplePath, parsingResult, reader, Comparator.paths.Result);
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
                new MixDiagIntoSourceName(),
                new MemoryName(),
                new NodeName(),
                new TokenName(),
                new AntlrName(),
#if EUROINFO_RULES
                new EIEmptyName(),
                new EICodeElementName(),
                new EIRPNName(),
                new EINYName(),
                new EIPGMName(),
                new EIMixDiagIntoSourceName(),
                new EIMemoryName(),
                new EINodeName(),
                new EITokenName(),
#endif
        };

        private IList<string> samples;
        private string[] compilerExtensions;
        private string[] fileToTestsExtensions;

        private string _sampleRoot;
        private string _resultsRoot;

        private int _nbOfTests;

        internal FolderTester(string sampleRoot, string resultsRoot, string folder, string[] fileToTestsExtensions, string[] compilerExtensions, string[] ignored = null, bool deep = true) {
			_sampleRoot = sampleRoot;
			_resultsRoot = resultsRoot;

			this.compilerExtensions = compilerExtensions;
            this.fileToTestsExtensions = fileToTestsExtensions;
			var option = deep? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly;
			string[] samples = new string[0];
			foreach(var ext in this.fileToTestsExtensions) {
				string[] paths = Directory.GetFiles(folder, "*" + ext, option);
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
            return _nbOfTests;
        }

		public void Test(bool debug = false, bool json = false, bool autoRemarks = false) {
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
					unit.Init(compilerExtensions, autoRemarks);
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
				            if (extension != null) filename = filename.Substring(0, filename.Length - extension.Length);
				            string[] lines = {unit.ToJSON()};
				            System.IO.File.WriteAllLines(filename + ".json", lines);
				        }

                        _nbOfTests++;
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

        private IList<FilesComparator> GetComparators(string sampleRoot, string resultsRoot, string samplePath, bool debug) {
            IList<FilesComparator> comparators = new List<FilesComparator>();
            foreach (var names in Names) {
                Paths path = new Paths(sampleRoot, resultsRoot, samplePath, names);
                if (System.IO.File.Exists(path.Result)) {
                    Type type = names.GetComparatorType();
                    var isEI = names.IsEI();
                    System.Reflection.ConstructorInfo constructor = type.GetConstructor(new[] { typeof(Paths), typeof(bool), typeof(bool) });
                    comparators.Add((FilesComparator)constructor?.Invoke(new object[] { path, debug, isEI }));
                }
            }
#if EUROINFO_RULES
            if(comparators.Any(c => c.IsEI))
            {
                //If any -EI result file exists => Remove all comparators without isEI flag to true. 
                //We only want to check EI results files. 
                foreach (var comparatorToRemove in comparators.Where(c => !(c.IsEI)).ToList())
                {
                    comparators.Remove(comparatorToRemove);
                }
            }
#endif
            return comparators;
        }
    }





    internal interface Comparator
    {
        void Compare(CompilationUnit result, StreamReader expected, string expectedResultPath);
    }

    internal class FilesComparator : Comparator
	{
		internal Paths paths;
		internal bool debug;
        internal bool IsEI { get; private set; }

        public FilesComparator(string name, bool debug) /*: this(name, null, debug)*/
	    {
	        
	    }
		public FilesComparator(Paths path, bool debug = false, bool isEI = false)
		{
		    paths = path;
			this.debug = debug;
            IsEI = isEI;
		}

		public virtual void Compare(CompilationUnit result, StreamReader reader, string expectedResultPath) {
            //Warning by default we only want All codeElementDiagnostics EXCEPT Node Diagnostics
			Compare(result.CodeElementsDocumentSnapshot.CodeElements, result.AllDiagnostics(false), reader, expectedResultPath);
		}

		internal virtual void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> codeElementDiagnostics, StreamReader expected, string expectedResultPath) {
			string result = ParserUtils.DumpResult(elements, codeElementDiagnostics);
			if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected, expectedResultPath);
		}

		internal DocumentFormat GetSampleFormat() {
			if (paths.SamplePath.Contains(".rdz"))
				return DocumentFormat.RDZReferenceFormat;
			return DocumentFormat.FreeUTF8Format;
		}
	}

    internal class ArithmeticComparator : FilesComparator
    {
        public ArithmeticComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }
        internal override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> codeElementDiagnostics, StreamReader expected, string expectedResultPath)
        {
            var errors = new System.Text.StringBuilder();
            int c = 0, line = 1;
            foreach (var e in elements)
            {
                c++;
                var operation = e as ArithmeticStatement;
                if (operation == null) continue;
                string rpn = expected.ReadLine();
                if (rpn == null) errors.AppendFormat("RPN number {0} not provided.", line);
                string dump = ToString(operation);
                if (dump != rpn) errors.AppendFormat("line {0}: \"{1}\", expected \"{2}\"\n", line, dump, rpn);
                line++;
            }
            if (c < 1) throw new Exception("No CodeElements found!");
            if (expected.ReadLine() != null) errors.AppendLine("Number of CodeElements (" + (line - 1) + ") lesser than expected.");
            if (errors.Length > 0)
            {
                errors.Insert(0, paths.SamplePath + ":\n");
                throw new Exception(errors.ToString());
            }
        }
        private string ToString(ArithmeticStatement statement)
        {
            if (statement == null) return null;
            var str = new System.Text.StringBuilder();
            foreach (var operations in statement.Affectations)
                foreach (var operation in operations.Value)
                    str.Append(operations.Key).Append(" = ").Append(operation).Append(", ");
            if (statement.Affectations.Count > 0) str.Length -= 2;
            return str.ToString();
        }
    }

    internal class NYComparator : FilesComparator
    {
        public NYComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        internal override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> codeElementDiagnostics, StreamReader expected, string expectedResultPath)
        {
            int c = 0;
            StringBuilder errors = new StringBuilder();
            foreach (var e in elements)
            {
                if (e is SentenceEnd) continue;
                string line = expected.ReadLine();
                if (line != "Y") errors.AppendFormat("line {0}: \"Y\", expected \"{1}\"\n", c, line);
                c++;
            }
            foreach (var d in codeElementDiagnostics)
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

        public Outputter(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        internal override void Compare(IEnumerable<CodeElement> elements, IEnumerable<Diagnostic> codeElementDiagnostics, StreamReader expected, string expectedResultPath)
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
        public Multipass(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        internal class IndexNames : Names
        {
            internal int index = 0;
            public string CreateName(string name) { return name+'.'+index; }
            public Type GetComparatorType() { return typeof(Multipass); }
            public bool IsEI() { return false; }
        }
    }

    internal class ProgramsComparator : FilesComparator
    {
        public ProgramsComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath)
        {
            IList<Diagnostic> diagnostics = compilationUnit.AllDiagnostics();
            ProgramClassDocument pcd = compilationUnit.ProgramClassDocumentSnapshot;
            
            Compare(pcd.Root.Programs, pcd.Root.Classes, diagnostics, reader, expectedResultPath);
        }

        internal void Compare(IEnumerable<Program> programs, IEnumerable<TypeCobol.Compiler.Nodes.Class> classes, IList<Diagnostic> diagnostics, StreamReader expected, string expectedResultPath)
        {
            string result = ParserUtils.DumpResult(programs, classes, diagnostics);
            if (debug) Console.WriteLine("\"" + paths.SamplePath+ "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected, expectedResultPath);
        }
    }

    /// <summary>
    /// Create a result file which contains:
    /// The original source file with all diagnostics inserted at corresponding lines
    /// </summary>
    internal class ProgramsComparator2 : FilesComparator
    {
        public ProgramsComparator2(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath)
        {
            var sortedDiags = compilationUnit.AllDiagnostics().OrderBy(d => d.Line).GetEnumerator();
            

            //Create result file
            
            //Read original source file
            StreamReader sourceReader = new StreamReader(new FileStream(paths.SamplePath , FileMode.Open));


            StringBuilder resultBuilder = new StringBuilder();
            int linePos = 0;

            Diagnostic nextDiag = sortedDiags.MoveNext() ? sortedDiags.Current : null;
            while (!sourceReader.EndOfStream) {
                string line = sourceReader.ReadLine();
                linePos++;


                while (nextDiag != null && nextDiag.Line <= linePos) {
                    resultBuilder.Append(nextDiag).Append("\n");
                    nextDiag = sortedDiags.MoveNext() ? sortedDiags.Current : null;
                }
                resultBuilder.Append(line).Append("\n");
            }

            //Print all remaining diags
            while (nextDiag != null){
                resultBuilder.Append(nextDiag).Append("\n");
                nextDiag = sortedDiags.MoveNext() ? sortedDiags.Current : null;
            }


            string result = resultBuilder.ToString();
            if (debug) Console.WriteLine("\"" + paths.SamplePath+ "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
        }
    }

    internal class NodeComparator : FilesComparator
    {
        public NodeComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath) {
            ProgramClassDocument pcd = compilationUnit.ProgramClassDocumentSnapshot;
            IList<Diagnostic> diagnostics = compilationUnit.AllDiagnostics();
            
            StringBuilder sb = new StringBuilder();
            foreach (var diagnostic in diagnostics) {
                sb.AppendLine(diagnostic.ToString());
            }
           
            sb.AppendLine("--- Nodes ---");
            sb.Append(pcd.Root);

            string result = sb.ToString();
            if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
        }
    }

    internal class TokenComparator : FilesComparator
    {
        public TokenComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath)
        {
            IList<Diagnostic> diagnostics = compilationUnit.AllDiagnostics();

            StringBuilder sb = new StringBuilder();
            foreach (var diagnostic in diagnostics)
            {
                sb.AppendLine(diagnostic.ToString());
            }

            sb.AppendLine("--- Tokens ---");
            foreach (var tokensLine in compilationUnit.TokensLines) {
                sb.AppendLine("---------------------------------");
                sb.AppendLine("_" + tokensLine.SourceText + "_");
                foreach (var sourceToken in tokensLine.SourceTokens) {
                    sb.AppendLine("    _" + sourceToken.SourceText + "_    " +  sourceToken);
                }
            }

            string result = sb.ToString();
            if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
        }
    }

    internal class MemoryComparator: FilesComparator
	{
	    public MemoryComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit result, StreamReader reader, string expectedResultPath)
        {
			ProgramClassDocument pcd = result.ProgramClassDocumentSnapshot;
            var programs = new List<Program>();
            foreach (var pgm in pcd.Root.Programs)
            {
                programs.Add(pgm);
            }
            
			Compare(programs, reader, expectedResultPath);
		}

		internal void Compare(List<Program> programs, StreamReader expected, string expectedResultPath)
		{
			string result = Dump(programs);
			if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected, expectedResultPath);
		}

		private string Dump(List<Program> programs)
		{
			var str = new StringBuilder();
            List<DataDefinition> dataDefinitions = new List<DataDefinition>();

			str.AppendLine("--------- FIELD LEVEL|NAME ---------- START     END  LENGTH");

            foreach (var program in programs)
            {
                var dataSections = program.Children.First(c => c is DataDivision).Children.Where(c => c is DataSection);

                foreach (Node node in program.Children.First(c => c is DataDivision).Children.Where(c => c is DataSection))
                {
                    dataDefinitions.AddRange(GetDataDefinitions(node));
                }

                foreach (var dataDefinition in dataDefinitions)
                {
                    //TODO: Issue #1192 handle correctly a DataRenames
                    if (dataDefinition is DataRenames == false)
                        str.AppendLine(CreateLine(dataDefinition));
                }
            }
			
			return str.ToString();
		}

        private List<DataDefinition> GetDataDefinitions(Node node)
        {
            List<DataDefinition> dataDefinitions = new List<DataDefinition>();
            foreach (var child in node.Children)
            {
                if (child is DataDefinition)
                {
                    dataDefinitions.Add(child as DataDefinition);
                    if (child.Children.Count > 0)
                    {
                        dataDefinitions.AddRange(GetDataDefinitions(child));
                    }
                }
            }

            return dataDefinitions;
        }

		private string CreateLine(DataDefinition data)
		{
		    var dataEntry = data.CodeElement as DataDefinitionEntry;
		    Node parentData = data.Parent;
			var res = new StringBuilder();
		    int indent = 4;

		    while (parentData is DataSection == false)
		    {
		        parentData = parentData.Parent;
		        res.Append(new string(' ', indent));
		    }

		    if (dataEntry != null)
		    {
		        if (data.IsTableOccurence)
		            res.Append($"{dataEntry.LevelNumber} {data.Name} ({data.MaxOccurencesCount})");
		        else
		            res.Append($"{dataEntry.LevelNumber} {data.Name}");

		    }

		    res.Append(InsertValues(res.Length, data.StartPosition.ToString(), data.PhysicalPosition.ToString(), 
		        data.PhysicalLength.ToString()));

            return res.ToString();
		}

	    private string InsertValues(int lineLength, string startPosition, string physicalPosition, string physicalLength)
	    {
            StringBuilder str = new StringBuilder();
	        const int columnStartPosition = 43;
	        const int offsetBetweenValue = 8;


	        str.Append(new string(' ', columnStartPosition - lineLength - startPosition.Length) + startPosition);
	        str.Append(new string(' ', offsetBetweenValue - physicalPosition.Length) + physicalPosition);
	        str.Append(new string(' ', offsetBetweenValue - physicalLength.Length) + physicalLength);
	        
            return str.ToString();
	    }
	}

    internal class AntlrComparator : FilesComparator
    {
        public AntlrComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit result, StreamReader reader, string expectedResultPath)
        {
            Compare(result.AntlrResult, reader, expectedResultPath);
        }

        internal void Compare(string AntlrResult, StreamReader reader, string expectedResultPath)
        {
            if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + AntlrResult);
            ParserUtils.CheckWithResultReader(paths.SamplePath, AntlrResult, reader, expectedResultPath);
        }

    }

    internal interface Names
    {
        string CreateName(string name);
        Type GetComparatorType();
        bool IsEI();
    }

    internal abstract class AbstractNames : Names {
        public abstract string CreateName(string name);
        public abstract Type GetComparatorType();

        public virtual bool IsEI() {
            return false;
        }
    }

    internal abstract class AbstractEINames : AbstractNames
    {
        public override bool IsEI() {
            return true;
        }
    }

    #region DefaultNames
    internal class EmptyName : Names
    {
        public string CreateName(string name) { return name; }
        public Type GetComparatorType() { return typeof(FilesComparator); }
        public bool IsEI() { return false; }
    }

    internal class CodeElementName : Names
    {
        public string CreateName(string name) { return name + "CodeElements"; }
        public Type GetComparatorType() { return typeof(FilesComparator); }
        public bool IsEI() { return false; }
    }

    internal class RPNName : Names
    {
        public string CreateName(string name) { return name + "RPN"; }
        public Type GetComparatorType() { return typeof(ArithmeticComparator); }
        public bool IsEI() { return false; }
    }

    internal class NYName : Names
    {
        public string CreateName(string name) { return name + "NY"; }
        public Type GetComparatorType() { return typeof(NYComparator); }
        public bool IsEI() { return false; }
    }

    internal class PGMName : Names
    {
        public string CreateName(string name) { return name + "PGM"; }
        public Type GetComparatorType() { return typeof(ProgramsComparator); }
        public bool IsEI() { return false; }
    }
    internal class MixDiagIntoSourceName : AbstractNames
    {
        public override string CreateName(string name) { return name + "Mix"; }
        public override Type GetComparatorType() { return typeof(ProgramsComparator2); }
    }

    internal class NodeName : Names
    {
        public string CreateName(string name) { return name + "-Nodes"; }
        public Type GetComparatorType() { return typeof(NodeComparator); }
        public bool IsEI() { return false; }
    }

    internal class TokenName : Names
    {
        public string CreateName(string name) { return name + "-Tokens"; }
        public Type GetComparatorType() { return typeof(TokenComparator); }
        public bool IsEI() { return false; }
    }

    internal class MemoryName : Names
    {
        public string CreateName(string name) { return name + "MEM"; }
        public Type GetComparatorType() { return typeof(MemoryComparator); }
        public bool IsEI() { return false; }
    }

    internal class AntlrName : Names
    {
        public string CreateName(string name) { return name + "ANTLR"; }
        public Type GetComparatorType() { return typeof(AntlrComparator); }
        public bool IsEI() { return false; }
    }
    #endregion

    #region EINames
#if EUROINFO_RULES
    internal class EIEmptyName : Names
    {
        public string CreateName(string name) { return name + "-EI"; }
        public Type GetComparatorType() { return typeof(FilesComparator); }

        public bool IsEI() { return true; }
    }

    internal class EICodeElementName : Names
    {
        public string CreateName(string name) { return name + "CodeElements-EI"; }
        public Type GetComparatorType() { return typeof(FilesComparator); }
        public bool IsEI() { return true; }
    }

    internal class EIRPNName : Names
    {
        public string CreateName(string name) { return name + "RPN-EI"; }
        public Type GetComparatorType() { return typeof(ArithmeticComparator); }
        public bool IsEI() { return true; }
    }

    internal class EINYName : Names
    {
        public string CreateName(string name) { return name + "NY-EI"; }
        public Type GetComparatorType() { return typeof(NYComparator); }
        public bool IsEI() { return true; }
    }
    internal class EIPGMName : Names
    {
        public string CreateName(string name) { return name + "PGM-EI"; }
        public Type GetComparatorType() { return typeof(ProgramsComparator); }
        public bool IsEI() { return true; }
    }
    internal class EIMixDiagIntoSourceName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "Mix-EI"; }
        public override Type GetComparatorType() { return typeof(ProgramsComparator2); }
    }

    internal class EINodeName : Names
    {
        public string CreateName(string name) { return name + "-Nodes-EI"; }
        public Type GetComparatorType() { return typeof(NodeComparator); }
        public bool IsEI() { return true; }
    }

    internal class EITokenName : Names
    {
        public string CreateName(string name) { return name + "-Tokens-EI"; }
        public Type GetComparatorType() { return typeof(TokenComparator); }
        public bool IsEI() { return true; }
    }

    internal class EIMemoryName : Names
    {
        public string CreateName(string name) { return name + "MEM-EI"; }
        public Type GetComparatorType() { return typeof(MemoryComparator); }
        public bool IsEI() { return true; }
    }
#endif
    #endregion

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
                string ResultFilePath = Path.GetDirectoryName(SamplePath)?.Substring(_sampleRoot.Length);
                string ResultFileName = Path.GetFileNameWithoutExtension(SamplePath);
                return _resultRoot + Path.DirectorySeparatorChar + ResultFilePath  + Path.DirectorySeparatorChar + Resultnames.CreateName(ResultFileName) + Rextension;
            }
            
        }
    }
}
