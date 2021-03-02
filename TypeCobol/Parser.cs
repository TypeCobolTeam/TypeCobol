using System;
using System.Collections.Generic;
using System.IO;
using JetBrains.Annotations;
using TypeCobol.Analysis;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.File;
using TypeCobol.Compiler.CodeModel;
using TypeCobol.CustomExceptions;
using TypeCobol.Tools.APIHelpers;
#if EUROINFO_RULES
using TypeCobol.Compiler.Preprocessor;
#endif

namespace TypeCobol
{
	public class Parser
	{
	    public IEnumerable<string> MissingCopys { get; private set; }
        protected Dictionary<string,bool> Inits;
        protected Dictionary<string,FileCompiler> Compilers;
        protected FileCompiler Compiler = null;
		/// <summary>Optional custom symbol table to use for name and type resolution.</summary>
		public SymbolTable CustomSymbols = null;

		public Parser() {
			Inits = new Dictionary<string,bool>();
			Compilers = new Dictionary<string,FileCompiler>();
		}

        public Parser(SymbolTable custmSymbols) :this()
        {
            CustomSymbols = custmSymbols;
        }

		private static DocumentFormat GetFormat(string filename) {
			return DocumentFormat.FreeUTF8Format;//TODO autodetect
		}

		public void Init([NotNull] string path, TypeCobolOptions options, DocumentFormat format = null, IList<string> copies = null, IAnalyzerProvider analyzerProvider = null) {
			FileCompiler compiler;
			if (Compilers.TryGetValue(path, out compiler)) return;
			string filename = Path.GetFileName(path);
			var root = new DirectoryInfo(Directory.GetParent(path).FullName);
			if (format == null) format = GetFormat(path);
            
            CompilationProject project = new CompilationProject(path, root.FullName, Helpers.DEFAULT_EXTENSIONS,
				format, options, analyzerProvider);
			//Add copy folder into sourceFileProvider
			SourceFileProvider sourceFileProvider = project.SourceFileProvider;
			copies = copies ?? new List<string>();
			foreach (var folder in copies) {
				sourceFileProvider.AddLocalDirectoryLibrary(folder, false, Helpers.DEFAULT_COPY_EXTENSIONS, format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength);
			}
			compiler = new FileCompiler(null, filename, project.SourceFileProvider, project, format.ColumnsLayout, options, CustomSymbols, false, project);
            
			Compilers.Add(path, compiler);
			Inits.Add(path, false);
		}


		public void Parse(string path, TextChangedEvent e=null)
		{
            //if the server is restarted during Eclipse lifetime, then we need to init the parser
            //This is useful when debugging. Perhaps it'll be deleted at the end
            if (!Compilers.ContainsKey(path))
			{
				Init(path, new TypeCobolOptions { ExecToStep = ExecutionStep.Generate});
			}
			Compiler = Compilers[path];

			Compiler.CompilationResultsForProgram.TextLinesChanged += OnTextLine;
			Compiler.CompilationResultsForProgram.CodeElementsLinesChanged += OnCodeElementLine;

            if (!Inits[path]) Inits[path] = true;// no need to update with the same content as at compiler creation
            else if (e != null) Compiler.CompilationResultsForProgram.UpdateTextLines(e);

            try { Compiler.CompileOnce(); }
			catch(Exception ex) {
                throw new ParsingException(MessageCode.SyntaxErrorInParser, ex.Message, path, ex, true, true);
			}

		    MissingCopys = Compiler.CompilationResultsForProgram.MissingCopies;

			Compiler.CompilationResultsForProgram.TextLinesChanged -= OnTextLine;
			Compiler.CompilationResultsForProgram.CodeElementsLinesChanged -= OnCodeElementLine;
		}

		private void OnCodeElementLine(object sender, DocumentChangedEvent<ICodeElementsLine> e)
		{
			System.Console.WriteLine("+++ OnCodeElementLine(..):");
			int c = 0;
			if (e.DocumentChanges != null)
			foreach(var change in e.DocumentChanges) {
				System.Console.WriteLine(" - "+change.Type+"@"+change.LineIndex);
				if (change.NewLine == null) System.Console.WriteLine("Line NIL");
				else
				if (change.NewLine.CodeElements == null) System.Console.WriteLine("CodeElements NIL");
				else {
					int i = 0;
					foreach(var ce in change.NewLine.CodeElements) {
						System.Console.WriteLine("   - "+ce);
						i++;
					}
					System.Console.WriteLine("   "+i+" CodeElements");
				}
				c++;
			}
			System.Console.WriteLine("+++ --> "+(c>0?(""+c):(e.DocumentChanges==null?"?":"0"))+" changes");
		}

		private void OnTextLine(object sender, DocumentChangedEvent<ICobolTextLine> e)
		{
			System.Console.WriteLine("--- OnTextLine(..):");
			int c = 0;
			if (e.DocumentChanges != null)
			foreach(var change in e.DocumentChanges) {
				System.Console.Write(" + "+change.Type+"@"+change.LineIndex+": ");
				if (change.NewLine == null) System.Console.WriteLine("?");
				else System.Console.WriteLine("\""+change.NewLine.SourceText+"\"");
				c++;
			}
			System.Console.WriteLine("--- --> "+(c>0?(""+c):(e.DocumentChanges==null?"?":"0"))+" changes");
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

		public ITextDocument Source {
			get { return Compiler.TextDocument; }
		}

		public CompilationUnit Results {
			get { return Compiler.CompilationResultsForProgram; }
		}


#if EUROINFO_RULES
        public static Parser EIParse(string path, DocumentFormat format, bool autoRemarks = false, IList<string> copies = null, IAnalyzerProvider analyzerProvider = null, CopyNameMapFile cpyCopyNamesMap = null)
        {
            var parser = new Parser();
            var typeCobolOption = new TypeCobolOptions() { ExecToStep = ExecutionStep.Generate };
            typeCobolOption.AutoRemarksEnable = autoRemarks;
            typeCobolOption.CpyCopyNamesMap = cpyCopyNamesMap;
            parser.Init(path, typeCobolOption, format, copies, analyzerProvider);
            parser.Parse(path);
            return parser;
        }
#endif
        public static Parser Parse(string path, DocumentFormat format, bool autoRemarks = false, IList<string> copies = null, IAnalyzerProvider analyzerProvider = null) {
            var parser = new Parser();
            var typeCobolOption = new TypeCobolOptions() { ExecToStep = ExecutionStep.Generate };
#if EUROINFO_RULES
		    typeCobolOption.AutoRemarksEnable = autoRemarks;
#endif
            parser.Init(path, typeCobolOption, format, copies, analyzerProvider);

            parser.Parse(path);
			return parser;
		}
	}
}
