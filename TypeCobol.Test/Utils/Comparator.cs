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
using TypeCobol.Compiler.Sql.CodeElements.Statements;
using TypeCobol.Compiler.Sql.Model;
#if EUROINFO_RULES
using TypeCobol.Compiler.Preprocessor;
#endif

namespace TypeCobol.Test.Utils
{
    internal class TestUnit
    {
        public FileCompiler Compiler { get; }
        public FilesComparator Comparator { get; }
        public TestObserver Observer { get; }

        public TestUnit(FilesComparator comparator1, string[] copyExtensions = null, bool antlrProfiler = false)
        {
            Comparator = comparator1;
            Observer = new TestObserver();

            string filePath = Comparator.paths.SamplePath;
            DirectoryInfo localDirectory = new DirectoryInfo(Path.GetDirectoryName(filePath));
            DocumentFormat format = Comparator?.GetSampleFormat();

            var sampleExtension = Path.GetExtension(filePath);
            HashSet<string> compilerExtensions = new HashSet<string>(StringComparer.OrdinalIgnoreCase)
                                                 {
                                                     sampleExtension
                                                 };
            if (copyExtensions == null || copyExtensions.Length == 0)
            {
                copyExtensions = new[] {".cpy"};
            }
            foreach (var copyExtension in copyExtensions)
            {
                compilerExtensions.Add(copyExtension);
            }

            TypeCobolOptions options = new TypeCobolOptions();
            CompilationProject project = new CompilationProject("TEST",
                localDirectory.FullName, compilerExtensions.ToArray(),
                format, options, null);
            
            string filename = Comparator.paths.SampleName;
            bool isCopy = copyExtensions.Contains(sampleExtension, StringComparer.OrdinalIgnoreCase);
            Compiler = new FileCompiler(null, filename, format.ColumnsLayout, isCopy, project.SourceFileProvider, project, options, null, project);

            if (antlrProfiler)
            {
                Compiler.CompilationResultsForProgram.PerfStatsForCodeElementsParser.ActivateDetailedAntlrPofiling = true;
                Compiler.CompilationResultsForProgram.PerfStatsForTemporarySemantic.ActivateDetailedAntlrPofiling = true;
            }
        }

        public void Parse()
        {
            try
            {
#if EUROINFO_RULES
                Compiler.CompilerOptions.CpyCopyNameMap = Comparator.GetCopyNameMap();
#endif
                Compiler.CompileOnce();
            }
            catch (Exception e)
            {
                Observer.OnError(e);
            }
#if EUROINFO_RULES
            finally
            {
                Compiler.CompilerOptions.CpyCopyNameMap = null;
            }
#endif
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
                new SYMName(),
                new SQLName(),
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
                new EISYMName(),
                new EIMixDiagIntoSourceName(),
                new EIMemoryName(),
                new EINodeName(),
                new EITokenName(),
                
#endif
        };

        private IList<string> samples;
        private string _sampleRoot;
        private string _resultsRoot;
        private string[] _copyExtensions;
        private int _nbOfTests;

        internal FolderTester(string sampleRoot, string resultsRoot, string folder, string[] fileToTestsExtensions, string[] copyExtensions = null, string[] ignored = null, bool deep = true) {
			_sampleRoot = sampleRoot;
			_resultsRoot = resultsRoot;
            _copyExtensions = copyExtensions;
			var option = deep? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly;
			string[] samples = new string[0];
			foreach(var ext in fileToTestsExtensions) {
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

		public void Test(bool debug = false, bool json = false, bool isCobolLanguage = false) {
			var errors = new StringBuilder();
			foreach (var samplePath in samples) {
                // Automatically enable SQL parsing for samples located in a directory containing "SQL" within its path
                string containingDirectory = Path.GetDirectoryName(samplePath);
                bool enableSqlParsing = containingDirectory != null && containingDirectory.IndexOf("SQL", StringComparison.InvariantCultureIgnoreCase) >= 0;
                IList<FilesComparator> comparators = GetComparators(_sampleRoot, _resultsRoot, samplePath, debug);
				if (comparators.Count < 1) {
					Console.WriteLine(" /!\\ ERROR: Missing result file \"" + samplePath + "\"");
					errors.AppendLine("Missing result file \"" + samplePath + "\"");
					continue;
				}
                foreach (var comparator in comparators) {
                    Console.WriteLine(comparator.paths.Result + " checked with " + comparator.GetType().Name);
                    var unit = new TestUnit(comparator, _copyExtensions);
                    unit.Compiler.CompilerOptions.IsCobolLanguage = isCobolLanguage;
                    unit.Compiler.CompilerOptions.EnableSqlParsing = enableSqlParsing;
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
			if (errors.Length > 0)
                throw new Exception(errors.ToString());
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
            //Warning by default we only want All codeElementDiagnostics (Node Diagnostics and Quality Diagnostics are not compared)
			Compare(result.CodeElementsDocumentSnapshot.CodeElements, result.AllDiagnostics(false, false), reader, expectedResultPath);
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

#if EUROINFO_RULES
        /// <summary>
        /// CopyNameMap cache
        /// </summary>
        private static readonly Dictionary<string, CopyNameMapFile> _CopyNameMaps = new Dictionary<string, CopyNameMapFile>(StringComparer.OrdinalIgnoreCase);

        internal CopyNameMapFile GetCopyNameMap()
        {
            //Key is directory full path
            string directory = Path.GetDirectoryName(paths.SamplePath);
            System.Diagnostics.Debug.Assert(directory != null);

            if (!_CopyNameMaps.TryGetValue(directory, out var copyNameMap))
            {
                //No CopyNameMap info in cache, check for the presence of a 'CpyCopies.lst' file.
                string copyNameMapFilePath = Path.Combine(directory, "CpyCopies.lst");
                copyNameMap = File.Exists(copyNameMapFilePath) ? new CopyNameMapFile(copyNameMapFilePath) : null;

                //Add to cache for future tests
                _CopyNameMaps.Add(directory, copyNameMap);
            }

            return copyNameMap;
        }
#endif
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
 
    internal class SqlComparator : FilesComparator
    {
        /// <summary>
        /// Selects the SQL code elements
        /// </summary>
        private class ASTVisitor : AbstractAstVisitor 
        {
            private readonly StringWriter _writer;

            public ASTVisitor(StringBuilder builder)
            {
                _writer = new StringWriter(builder);
            }

            private void DumpObject(string name, object value)
            {
                _writer.Write($"- {name} = ");
                if (value == null)
                {
                    _writer.WriteLine("<NULL>");
                }
                else if (value is SqlObject sqlObject)
                {
                    sqlObject.Dump(_writer, 1);
                }
                else
                {
                    _writer.WriteLine(value.ToString());
                }
            }

            public override bool Visit(SelectStatement selectStatement)
            {
                _writer.WriteLine($"line {selectStatement.Line}: {nameof(SelectStatement)}");
                DumpObject(nameof(selectStatement.FullSelect), selectStatement.FullSelect);
                return true;
            }

            public override bool Visit(RollbackStatement rollbackStatement)
            {
                _writer.WriteLine($"line {rollbackStatement.Line}: {nameof(RollbackStatement)}");
                DumpObject(nameof(rollbackStatement.SavePointClause), rollbackStatement.SavePointClause);
                return true;
            }

            public override bool Visit(CommitStatement commitStatement)
            {
                _writer.WriteLine($"line {commitStatement.Line}: {nameof(CommitStatement)}");
                //No SqlObject in CommitStatement
                return true;
            }

            public override bool Visit(TruncateStatement truncateStatement)
            {
                _writer.WriteLine($"line {truncateStatement.Line}: {nameof(TruncateStatement)}");
                DumpObject(nameof(truncateStatement.TableName), truncateStatement.TableName);
                DumpObject(nameof(truncateStatement.StorageManagement), truncateStatement.StorageManagement);
                DumpObject(nameof(truncateStatement.DeleteTriggersHandling), truncateStatement.DeleteTriggersHandling);
                DumpObject(nameof(truncateStatement.IsImmediate),  truncateStatement.IsImmediate);

                return true;
            }

            public override bool Visit(WhenEverStatement whenEverStatement)
            {
                _writer.WriteLine($"line {whenEverStatement.Line}: {nameof(WhenEverStatement)}");
                DumpObject(nameof(whenEverStatement.ExceptionCondition), whenEverStatement.ExceptionCondition);
                DumpObject(nameof(whenEverStatement.BranchingType), whenEverStatement.BranchingType);
                DumpObject(nameof(whenEverStatement.TargetSectionOrParagraph), whenEverStatement.TargetSectionOrParagraph);
                return true;
            }
            public override bool Visit(SavepointStatement savepointStatement)
            {
                _writer.WriteLine($"line {savepointStatement.Line}: {nameof(SavepointStatement)}");
                DumpObject(nameof(savepointStatement.Name), savepointStatement.Name);
                DumpObject(nameof(savepointStatement.IsUnique), savepointStatement.IsUnique);
                DumpObject(nameof(savepointStatement.RetainLocks), savepointStatement.RetainLocks);
                return true;
            }
            public override bool Visit(LockTableStatement lockTableStatement)
            {
                _writer.WriteLine($"line {lockTableStatement.Line}: {nameof(LockTableStatement)}");
                DumpObject(nameof(lockTableStatement.Table), lockTableStatement.Table);
                DumpObject(nameof(lockTableStatement.PartitionId), lockTableStatement.PartitionId);
                DumpObject(nameof(lockTableStatement.Mode), lockTableStatement.Mode);
                return true;
            }
            public override bool Visit(ReleaseSavepointStatement releaseSavepointStatement)
            {
                _writer.WriteLine($"line {releaseSavepointStatement.Line}: {nameof(ReleaseSavepointStatement)}");
                DumpObject(nameof(releaseSavepointStatement.SavepointName), releaseSavepointStatement.SavepointName);
                return true;
            }
            public override bool Visit(ConnectStatement connectStatement)
            {
                _writer.WriteLine($"line {connectStatement.Line}: {nameof(ConnectStatement)}");
                DumpObject(nameof(connectStatement.Target), connectStatement.Target);
                DumpObject(nameof(connectStatement.Authorization), connectStatement.Authorization);
                DumpObject(nameof(connectStatement.Reset), connectStatement.Reset);
                return true;
            }

            public override bool Visit(DropTableStatement dropTableStatement)
            {
                _writer.WriteLine($"line {dropTableStatement.Line}: {nameof(DropTableStatement)}");
                DumpObject(nameof(dropTableStatement.TableOrAliasName), dropTableStatement.TableOrAliasName);
                return true;
            }
        }

        public SqlComparator(Paths path, bool debug = false, bool isEI = false)
            : base(path, debug, isEI)
        {

        }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath)
        {
            var diagnostics = compilationUnit.AllDiagnostics();
            var programs = compilationUnit.ProgramClassDocumentSnapshot.Root.Programs.ToList();

            var builder = new StringBuilder();

            if (diagnostics.Count > 0)
            {
                builder.AppendLine(ParserUtils.DiagnosticsToString(diagnostics));
            }
            builder.AppendLine("--- Sql Statements ---");
            
            
            compilationUnit.ProgramClassDocumentSnapshot.Root.AcceptASTVisitor(new ASTVisitor(builder) );
            
            
            string result = builder.ToString();
            if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
        }
    }
 
    internal class SymbolComparator : FilesComparator
    {
        public SymbolComparator(Paths path, bool debug = false, bool isEI = false)
            : base(path, debug, isEI)
        {

        }

        public override void Compare(CompilationUnit compilationUnit, StreamReader reader, string expectedResultPath)
        {
            var diagnostics = compilationUnit.AllDiagnostics();
            var programs = compilationUnit.ProgramClassDocumentSnapshot.Root.Programs.ToList();

            var builder = new StringBuilder();

            //Write diagnostics
            if (diagnostics.Count > 0)
            {
                builder.AppendLine(ParserUtils.DiagnosticsToString(diagnostics));
            }

            //Write program symbols
            if (programs.Count > 0)
            {
                foreach (var program in programs)
                {
                    builder.AppendLine("--- Program ---");
                    builder.AppendLine(program.SemanticData?.ToString() ?? "No Semantic Data !");
                }
            }

            //TODO Comparison for classes ?

            string result = builder.ToString();
            if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
            ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
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
            var sortedDiags = compilationUnit.AllDiagnostics().OrderBy(d => d.LineStart).GetEnumerator();

            //Create result file

            //Read original source file
            using (StreamReader sourceReader = new StreamReader(new FileStream(paths.SamplePath, FileMode.Open)))
            {
                StringBuilder resultBuilder = new StringBuilder();
                int linePos = 0;

                Diagnostic nextDiag = sortedDiags.MoveNext() ? sortedDiags.Current : null;
                while (!sourceReader.EndOfStream)
                {
                    string line = sourceReader.ReadLine();
                    linePos++;


                    while (nextDiag != null && nextDiag.LineStart <= linePos)
                    {
                        resultBuilder.Append(nextDiag).Append("\n");
                        nextDiag = sortedDiags.MoveNext() ? sortedDiags.Current : null;
                    }
                    resultBuilder.Append(line).Append("\n");
                }

                //Print all remaining diags
                while (nextDiag != null)
                {
                    resultBuilder.Append(nextDiag).Append("\n");
                    nextDiag = sortedDiags.MoveNext() ? sortedDiags.Current : null;
                }


                string result = resultBuilder.ToString();
                if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
                ParserUtils.CheckWithResultReader(paths.SamplePath, result, reader, expectedResultPath);
            }
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
            
			Compare(result, programs, reader, expectedResultPath);
		}

		internal void Compare(CompilationUnit compUnit, List<Program> programs, StreamReader expected, string expectedResultPath)
		{
			string result = Dump(compUnit, programs);
			if (debug) Console.WriteLine("\"" + paths.SamplePath + "\" result:\n" + result);
			ParserUtils.CheckWithResultReader(paths.SamplePath, result, expected, expectedResultPath);
		}

		private string Dump(CompilationUnit compUnit, List<Program> programs)
		{
			var str = new StringBuilder();
            List<DataDefinition> dataDefinitions = new List<DataDefinition>();
            str.AppendLine("Diagnostics");
            str.AppendLine("------------");
            var diags = compUnit.AllDiagnostics();
            if (diags.Count > 0)
            {
                foreach (var diag in compUnit.AllDiagnostics())
                {
                    str.AppendLine(diag.ToString());
                }
            } else
            {
                str.AppendLine("  None");
            }

            str.AppendLine("   ");
            str.AppendLine("--------- FIELD LEVEL|NAME ---------- START     END  LENGTH");

            foreach (var program in programs)
            {
                foreach (Node node in program.Children.First(c => c is DataDivision).Children.Where(c => c is DataSection))
                {
                    dataDefinitions.AddRange(GetDataDefinitions(node));
                }

                foreach (var dataDefinition in dataDefinitions)
                {
                    //TODO: Issue #1192 handle correctly a DataRenames
                    if (dataDefinition is DataRenames == false && dataDefinition.CodeElement?.LevelNumber?.Value != 88)
                        str.AppendLine(CreateLine(dataDefinition, dataDefinition.SlackBytes == 0));
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

		private string CreateLine(DataDefinition data, bool slackByteIsHandled)
		{
		    Node parentData = data.Parent;
			var res = new StringBuilder();
		    int indent = 4;

		    while (parentData is DataSection == false)
		    {
		        parentData = parentData.Parent;
		        res.Append(new string(' ', indent));
		    }

		    if (!slackByteIsHandled)
		    {
		        res.Append("SlackByte");

		        res.AppendLine(InsertValues(res.Length, (data.StartPosition - data.SlackBytes).ToString(), (data.StartPosition - 1).ToString(),
		            data.SlackBytes.ToString()));

		        res.Append(CreateLine(data, true));
		    }
		    else
		    {
		        var dataEntry = data.CodeElement as DataDefinitionEntry;

		        if (dataEntry != null)
		        {
		            if (data.IsTableOccurence)
		                res.Append($"{dataEntry.LevelNumber} {data.Name} ({data.MaxOccurencesCount})");
		            else
		                res.Append($"{dataEntry.LevelNumber} {data.Name}");

		            res.Append(InsertValues(res.Length, data.StartPosition.ToString(), data.PhysicalPosition.ToString(),
		                data.PhysicalLength.ToString()));
		        }
		    }

            return res.ToString();
		}


        private string InsertValues(int lineLength, string startPosition, string physicalPosition, string physicalLength)
	    {
            StringBuilder str = new StringBuilder();
	        const int columnStartPosition = 60;
	        const int offsetBetweenValue = 8;


	        str.Append(new string(' ', Math.Max(columnStartPosition - lineLength - startPosition.Length,0)) + startPosition);
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

    internal class SYMName : AbstractNames
    {
        public override string CreateName(string name) { return name + "SYM" + Rextension; }
        public override Type GetComparatorType() { return typeof(SymbolComparator); }
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
    internal class SQLName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "SQL" + Rextension; }
        public override Type GetComparatorType() { return typeof(SqlComparator); }
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

    internal class EISYMName : AbstractEINames
    {
        public override string CreateName(string name) { return name + "SYM-EI" + Rextension; }
        public override Type GetComparatorType() { return typeof(SymbolComparator); }
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
