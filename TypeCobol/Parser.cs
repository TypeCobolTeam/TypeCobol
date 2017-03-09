﻿using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using JetBrains.Annotations;
using TypeCobol.Compiler;
using TypeCobol.Compiler.CodeElements;
using TypeCobol.Compiler.Directives;
using TypeCobol.Compiler.Parser;
using TypeCobol.Compiler.Text;
using TypeCobol.Compiler.Concurrency;
using TypeCobol.Compiler.Diagnostics;
using TypeCobol.Compiler.File;

namespace TypeCobol
{
	public class Parser
	{
		public IObserver<TypeCobol.Compiler.Parser.CodeElementChangedEvent> Observer { get; private set; }
	    public List<string> MissingCopys { get; set; }
        protected Dictionary<string,bool> Inits;
        protected Dictionary<string,FileCompiler> Compilers;
        protected FileCompiler Compiler = null;
		/// <summary>Optional custom symbol table to use for name and type resolution.</summary>
		public TypeCobol.Compiler.CodeModel.SymbolTable CustomSymbols = null;

		public string[] Extensions = { ".cbl", ".cpy" };
		public string[] CopyExtensions = { ".cpy" };

		public Parser() {
			Observer = new Observer();
			Inits = new Dictionary<string,bool>();
			Compilers = new Dictionary<string,FileCompiler>();
		}

		private static DocumentFormat GetFormat(string filename) {
			return DocumentFormat.FreeUTF8Format;//TODO autodetect
		}

		public void Init([NotNull] string path,  DocumentFormat format = null, IList<string> copies = null, bool autoRemarks = false, string haltOnMissingCopyFilePath = null) {
			FileCompiler compiler;
			if (Compilers.TryGetValue(path, out compiler)) return;
			string filename = Path.GetFileName(path);
			var root = new DirectoryInfo(Directory.GetParent(path).FullName);
			if (format == null) format = GetFormat(path);
#if EUROINFO_LEGACY_REPLACING_SYNTAX
		    TypeCobolOptions options = new TypeCobolOptions {AutoRemarksEnable = autoRemarks, HaltOnMissingCopy = (haltOnMissingCopyFilePath != null) };
#endif
            CompilationProject project = new CompilationProject(path, root.FullName, Extensions,
				format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength, format.ColumnsLayout, options);
			//Add copy folder into sourceFileProvider
			SourceFileProvider sourceFileProvider = project.SourceFileProvider;
			copies = copies ?? new List<string>();
			foreach (var folder in copies) {
				sourceFileProvider.AddLocalDirectoryLibrary(folder, false, CopyExtensions, format.Encoding, format.EndOfLineDelimiter, format.FixedLineLength);
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
				Init(path);
			}
			Compiler = Compilers[path];

			Compiler.CompilationResultsForProgram.TextLinesChanged += OnTextLine;
			Compiler.CompilationResultsForProgram.CodeElementsLinesChanged += OnCodeElementLine;

			if (!Inits[path]) Inits[path] = true;// no need to update with the same content as at compiler creation
			else Compiler.CompilationResultsForProgram.UpdateTextLines(e);
			try { Compiler.CompileOnce(); }
			catch(Exception ex) {
				Observer.OnError(ex);
				System.Console.WriteLine(ex.ToString());
			}

		    MissingCopys = Compiler.CompilationProject.MissingCopys;

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



		public static Parser Parse(string path, DocumentFormat format) {
			var parser = new Parser();
			parser.Init(path, format);
			parser.Parse(path);
			return parser;
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
