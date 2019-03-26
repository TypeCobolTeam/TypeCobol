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
using TypeCobol.Compiler.Domain;
using TypeCobol.Compiler.Nodes;
using TypeCobol.Compiler.Parser;
// DocumentFormat
// CodeElementDiagnostics
using TypeCobol.Tools.Options_Config;

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
                new DocumentationName(),
                new DocumentationPropName(),
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

#if DOMAIN_CHECKER
        public static TypeCobolConfiguration DefaultConfig = null;
        public static ProgramSymbolTableBuilder Builder = null;
        public static NodeListenerFactory BuilderNodeListenerFactory = null;
        public static string DefaultIntrinsicPath = null;//@"C:\TypeCobol\Sources\##Latest_Release##\Intrinsic\Intrinsic.txt";

        public void TestInitialize()
        {
            SymbolTableBuilder.Root = null;
            //Create a default configurations for options
            DefaultConfig = new TypeCobolConfiguration();
            if (File.Exists(DefaultIntrinsicPath))
            {
                DefaultConfig.Copies.Add(DefaultIntrinsicPath);
            }
            DefaultConfig.Dependencies.Add(Path.Combine(Directory.GetCurrentDirectory(), "resources", "dependencies"));
            SymbolTableBuilder.Config = DefaultConfig;

            //Force the creation of the Global Symbol Table
            var global = SymbolTableBuilder.Root;

            //Allocate a static Program Symbol Table Builder
            BuilderNodeListenerFactory = () =>
            {
                Builder = new ProgramSymbolTableBuilder();
                ProgramSymbolTableBuilder.LastBuilder = Builder;
                return Builder;
            };
            TypeCobol.Compiler.Parser.NodeDispatcher.RegisterStaticNodeListenerFactory(BuilderNodeListenerFactory);
        }
        public void TestCleanup()
        {
            if (BuilderNodeListenerFactory != null)
            {
                TypeCobol.Compiler.Parser.NodeDispatcher.RemoveStaticNodeListenerFactory(BuilderNodeListenerFactory);
                if (Builder.Programs.Count != 0)
                {
                    foreach (var prog in Builder.Programs)
                        SymbolTableBuilder.Root.RemoveProgram(prog);
                }
                ProgramSymbolTableBuilder.LastBuilder = null;
            }
        }

#endif

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
#if DOMAIN_CHECKER
				    try
				    {
				        TestInitialize();
                        unit.Parse();
                    }
				    finally
				    {
				        TestCleanup();
				    }
#else
    unit.Parse();
#endif
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

#region Comparators
    
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

        internal class IndexNames : AbstractNames
        {
            internal int index = 0;
            public override string CreateName(string name) { return name+'.'+index + Rextension; }
            public override Type GetComparatorType() { return typeof(Multipass); }
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

        public override void Compare(CompilationUnit result, StreamReader reader, string expectedResultPath) {
			ProgramClassDocument pcd = result.ProgramClassDocumentSnapshot;
            var symbolTables = new List<SymbolTable>();
            symbolTables.Add(pcd.Root.SymbolTable);

			Compare(symbolTables, reader, expectedResultPath);
		}

		internal void Compare(List<SymbolTable> tables, StreamReader expected, string expectedResultPath) {
			string result = Dump(tables);
			if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected, expectedResultPath);
		}

		private string Dump(List<SymbolTable> tables) {
			var str = new StringBuilder();
			str.AppendLine("--------- FIELD LEVEL/NAME ---------- START     END  LENGTH");
            foreach (var table in tables)
            {
                foreach (var line in table.DataEntries)
                {
                    foreach (var data in line.Value)
                    {
                        //TODO#249 print memory representation
                        //					if (data is DataDefinition && ((DataDefinition)data).CodeElement().LevelNumber.Value == 1)
                        //					if (data.LevelNumber.Value == 1) Dump(str, data, 0);
                    }
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
			if (level > 1) str.Append(System.String.Format("{0,2} ", level));
			str.Append(name);
		}
		private void EndFirstColumn(StringBuilder str, string strprefix, int index, int max) {
			if (strprefix.Length > 0 || (index >= 0 && max > 1)) {
				str.Append('(').Append(CreateIndexes(strprefix,index)).Append(')');
			}
			while(str.Length < 36) str.Append(' ');
		}
		private void EndLine(StringBuilder str, int offset, int size, int baseaddress = 1) {
			str.Append(System.String.Format(" {0,6:0} ", baseaddress + offset));//start
			str.Append(System.String.Format(" {0,6:0} ", offset + size));//end
			str.Append(System.String.Format(" {0,6:0} ", size));//length
		}
		private string CreateIndexes(string prefix, int index) {
			return prefix + (index >= 0?((prefix.Length > 0?",":"")+(index+1)):"");
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

    internal class DocumentationComparator : FilesComparator
    {
        public DocumentationComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath)
        {
            ProgramClassDocument pcd = compilationUnit.ProgramClassDocumentSnapshot;
            IList<Diagnostic> diagnostics = compilationUnit.AllDiagnostics();

            StringBuilder sb = new StringBuilder();
            foreach (var diagnostic in diagnostics)
            {
                sb.AppendLine(diagnostic.ToString());
            }
            List<IDocumentable> documentedNodes = ParserUtils.GetDocumentedNodes(pcd.Root);

            sb.AppendLine("======================== Documentation ========================");
            foreach (var node in documentedNodes)
            {
                var doc = Documentation.CreateAppropriateDocumentation(node);
                sb.Append(doc.SerializeToJson(true));
                sb.AppendLine();
                sb.AppendLine("---------------------");
                sb.AppendLine();
            }

            string result = sb.ToString();
            if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
        }
    }

    internal class DocumentationPropertiesComparator : FilesComparator
    {
        public DocumentationPropertiesComparator(Paths path, bool debug = false, bool isEI = false) : base(path, debug, isEI) { }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath)
        {
            ProgramClassDocument pcd = compilationUnit.ProgramClassDocumentSnapshot;
            IList<Diagnostic> diagnostics = compilationUnit.AllDiagnostics();

            StringBuilder sb = new StringBuilder();
            foreach (var diagnostic in diagnostics)
            {
                sb.AppendLine(diagnostic.ToString());
            }

            List<IDocumentable> documentedNodes = ParserUtils.GetDocumentedNodes(pcd.Root);

            sb.AppendLine("======================== Nodes properties ========================");
            foreach (var node in documentedNodes)
            {
                var doc = Documentation.CreateAppropriateDocumentation(node);
                WriteDocumentedNodeProperties(doc, sb);
                sb.AppendLine();
                sb.AppendLine("---------------------");
                sb.AppendLine();
            }

            sb.AppendLine("======================== Code Element properties ========================");
            foreach (var node in documentedNodes)
            {
                var doc = Documentation.CreateAppropriateDocumentation(node);
                WriteDocumentedCodeElementProperties(doc, sb);
                sb.AppendLine();
                sb.AppendLine("---------------------");
                sb.AppendLine();
            }


            string result = sb.ToString();
            if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
        }

        private void WriteDocumentedNodeProperties(Documentation doc, StringBuilder sb)
        {
            sb.AppendLine("Name : " + doc.Name);
            sb.AppendLine("Description : " + doc.Description);
            sb.AppendLine("Visibility : " + doc.Visibility);
            sb.AppendLine("Namespace : " + doc.Namespace);
            sb.AppendLine("NodeType : " + (doc.IsTypeDef ? "TypeDef" :
                                               doc.IsFunction ? "Function" :
                                               doc.IsProgram ? "Program" : ""));

            sb.AppendLine("IsDeprecated : " + doc.IsDeprecated);
            sb.AppendLine("Deprecated : " + doc.Deprecated);
            sb.AppendLine("ReplacedBy : " + doc.ReplacedBy);
            sb.AppendLine("Restriction : " + doc.Restriction);
            sb.AppendLine("See : " + doc.See);
            sb.AppendLine("Needs : ");
            foreach (var need in doc.Needs ?? Enumerable.Empty<string>())
                sb.AppendLine("    " + need);
            sb.AppendLine("ToDo : ");
            foreach (var toDo in doc.ToDo ?? Enumerable.Empty<string>())
                sb.AppendLine("    " + toDo);

            // Typedefs Specific:
            DocumentationForType typeDoc = doc as DocumentationForType;
            if (typeDoc != null)
            {
                sb.AppendLine("IsBlankWhenZero : " + typeDoc.IsBlankWhenZero);
                sb.AppendLine("Justified : " + typeDoc.Justified);
                sb.AppendLine("DocDataType : " + typeDoc.DocDataType);
                WriteDocDataType(sb, typeDoc.DocDataType);
                // Childrens is not initialized with an empty list to prevent the documentation export flooding
                if (typeDoc.Childrens != null)
                {
                    foreach (var child in typeDoc.Childrens)
                    {
                        WriteTypeDefChildrens(sb, child);
                    }
                }
            }

            // Programs and Functions Specific:
            DocumentationForFunction funcDoc = doc as DocumentationForFunction;
            DocumentationForProgram pgmDoc = doc as DocumentationForProgram;
            if (funcDoc != null || pgmDoc != null)
            {
                sb.AppendLine("Parameters : ");
                bool isFirstParam = true;
                foreach (var param in (funcDoc?.Parameters ?? pgmDoc?.Parameters) ?? Enumerable.Empty<DocumentationParameter>())
                {
                    if (isFirstParam)
                        isFirstParam = false;
                    else
                        sb.AppendLine("    --------");
                    sb.AppendLine("    " + "Name : " + param.Name);
                    sb.AppendLine("    " + "Info : " + param.Info);
                    sb.AppendLine("    " + "PassingType : " + param.PassingType);
                    WriteDocDataType(sb, param.DocDataType, 1);
                }
            }
        }

        private void WriteDocumentedCodeElementProperties(Documentation doc, StringBuilder sb)
        {

            sb.AppendLine("== " + doc.Name + " ==");
            FormalizedCommentDocumentation formCom = doc.FormCom;

            if (formCom != null)
            {
                sb.AppendLine("Description : " + formCom.Description);
                sb.AppendLine("Deprecated : "  + (formCom.Deprecated == "" ? "*is present*" : formCom.Deprecated));
                sb.AppendLine("ReplacedBy : "  + formCom.ReplacedBy);
                sb.AppendLine("Restriction : " + formCom.Restriction);
                sb.AppendLine("See : "         + formCom.See);
                sb.AppendLine("Needs : ");
                foreach (var need in formCom.Needs ?? Enumerable.Empty<string>())
                    sb.AppendLine("    " + need);
                sb.AppendLine("ToDo : ");
                foreach (var toDo in formCom.ToDo ?? Enumerable.Empty<string>())
                    sb.AppendLine("    " + toDo);
                sb.AppendLine("Parameters : ");
                foreach (var parameter in formCom.Parameters ?? new Dictionary<string, string>())
                {
                    sb.AppendLine("    " + parameter.Key + " : " + parameter.Value);

                }
            }
            else
                sb.AppendLine("No formalized comment found");
        }

        private void WriteDocDataType(StringBuilder sb, DocumentationDataType docDataType, int level = 0)
        {
            sb.AppendLine(new string(' ', level * 4) + "DataType : ");
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Usage : " + docDataType.Usage);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "MaxOccurence : " + docDataType.MaxOccurence);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "DefaultValue : " + docDataType.DefaultValue);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "TypeName : " + docDataType.TypeName);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Picture : " + docDataType.Picture);
        }

        private void WriteTypeDefChildrens(StringBuilder sb, DocumentationTypeChildren child, int level = 0)
        {
            sb.AppendLine(new string(' ', level * 4) + "TypeDefChild : ");
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Name : " + child.Name);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "IsBlankWhenZero : " + child.IsBlankWhenZero);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "Justified : " + child.Justified);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "IsLevel77 : " + child.IsLevel77);
            sb.AppendLine(new string(' ', (level + 1) * 4) + "IsLevel88 : " + child.IsLevel88);

            sb.AppendLine(new string(' ', (level + 1) * 4) + "ConditionValues : ");
            for (int i = 0; i < (child.ConditionValues?.Length ?? 0); i++)
            {
                sb.AppendLine(new string(' ', (level + 2) * 4) + $"Condition{i} : {child.ConditionValues[i]}");
            }

            sb.AppendLine(new string(' ', (level + 1) * 4) + "ConditionValuesRanges : ");
            for (int i = 0; i < (child.ConditionValuesRanges?.Length ?? 0) ; i++)
            {
                sb.AppendLine(new string(' ', (level + 2) * 4) + $"ConditionRanges{i} : {child.ConditionValuesRanges[i].MinValue} -> {child.ConditionValuesRanges[i].MaxValue}");
            }

            WriteDocDataType(sb, child.DocDataType, level + 1);

            if (child.Childrens != null)
            {
                foreach (var subChild in child.Childrens)
                {
                    WriteTypeDefChildrens(sb, subChild, level + 1);
                }
            }
        }
    }

#endregion


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
        public string Rextension => ".txt";
    }

    internal abstract class AbstractEINames : AbstractNames
    {
        public override bool IsEI() {
            return true;
        }
    }

#region DefaultNames
    internal class EmptyName : AbstractNames
    {
        private Names _namesImplementation;
        public override string CreateName(string name) { return name + Rextension; }
        public override Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class CodeElementName : AbstractNames
    {
        public override string CreateName(string name) { return name + "CodeElements" + Rextension; }
        public override Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class RPNName : AbstractNames
    {
        public override string CreateName(string name) { return name + "RPN" + Rextension; }
        public override Type GetComparatorType() { return typeof(ArithmeticComparator); }
    }

    internal class NYName : AbstractNames
    {
        public override string CreateName(string name) { return name + "NY" + Rextension; }
        public override Type GetComparatorType() { return typeof(NYComparator); }
    }

    internal class PGMName : AbstractNames
    {
        public override string CreateName(string name) { return name + "PGM" + Rextension; }
        public override Type GetComparatorType() { return typeof(ProgramsComparator); }
    }
    internal class MixDiagIntoSourceName : AbstractNames
    {
        public override string CreateName(string name) { return name + "Mix" + Rextension; }
        public override Type GetComparatorType() { return typeof(ProgramsComparator2); }
    }

    internal class NodeName : AbstractNames
    {
        public override string CreateName(string name) { return name + "-Nodes" + Rextension; }
        public override Type GetComparatorType() { return typeof(NodeComparator); }
    }

    internal class TokenName : AbstractNames
    {
        public override string CreateName(string name) { return name + "-Tokens" + Rextension; }
        public override Type GetComparatorType() { return typeof(TokenComparator); }
    }

    internal class MemoryName : AbstractNames
    {
        public override string CreateName(string name) { return name + "MEM" + Rextension; }
        public override Type GetComparatorType() { return typeof(MemoryComparator); }
    }

    internal class AntlrName : AbstractNames
    {
        public override string CreateName(string name) { return name + "ANTLR" + Rextension; }
        public override Type GetComparatorType() { return typeof(AntlrComparator); }
    }

    internal class DocumentationName : AbstractNames
    {
        public override string CreateName(string name) { return name + Rextension; }
        public override Type GetComparatorType() { return typeof(DocumentationComparator); }
        public new string Rextension => ".json";
    }

    internal class DocumentationPropName : AbstractNames
    {
        public override string CreateName(string name) { return name + "Doc" + Rextension; }
        public override Type GetComparatorType() { return typeof(DocumentationPropertiesComparator); }
    }
#endregion

#region EINames
#if EUROINFO_RULES
    internal class EIEmptyName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class EICodeElementName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "CodeElements-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(FilesComparator); }
    }

    internal class EIRPNName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "RPN-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(ArithmeticComparator); }
    }

    internal class EINYName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "NY-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(NYComparator); }
    }
    internal class EIPGMName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "PGM-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(ProgramsComparator); }
    }
    internal class EIMixDiagIntoSourceName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "Mix-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(ProgramsComparator2); }
    }

    internal class EINodeName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "-Nodes-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(NodeComparator); }
    }

    internal class EITokenName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "-Tokens-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(TokenComparator); }
    }

    internal class EIMemoryName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "MEM-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(MemoryComparator); }
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
                return _resultRoot + Path.DirectorySeparatorChar + ResultFilePath  + Path.DirectorySeparatorChar + Resultnames.CreateName(ResultFileName);
            }
            
        }
    }
}
