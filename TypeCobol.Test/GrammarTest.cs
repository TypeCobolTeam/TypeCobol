using System;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Test.Compiler.Parser;
using System.Diagnostics;
using System.Collections.Generic;
using System.IO;

namespace TypeCobol.Test {

	[TestClass]
	public class GrammarTest {
		[TestMethod]
		[TestCategory("Parsing")]
		[TestProperty("Time","long")]
		[Ignore] // Ignored, as everybody does not have a Samples folder. Remove this if you do have one.
		public void CheckGrammarCorrectness() {

			int STOP_AFTER_AS_MANY_ERRORS = 1000;
			string regex = "*.PGM";
			string samples = @"Samples";
			string root = PlatformUtils.GetPathForProjectFile(samples);
			string[] files = Directory.GetFiles(root, regex, System.IO.SearchOption.AllDirectories);
			string[] include = { };
			string[] exclude = { };
			bool codegen = true;
			var format = TypeCobol.Compiler.DocumentFormat.RDZReferenceFormat;

			System.IO.File.WriteAllText("CheckGrammarResults.txt", "");
			int tested = 0, nbFilesInError = 0, ignores = 0;
			TimeSpan sum = new TimeSpan(0);
			int parseErrors = 0;
			int codegenErrors = 0;
			foreach (var file in files) {

				string filename = Path.GetFileName(file);
				System.IO.File.AppendAllText("CheckGrammarResults.txt", (filename + ':'));
				bool ignore = include.Length > 0 && !include.Contains(filename);
				if (!ignore) ignore = exclude.Contains(filename);
				if (ignore) {
					ignores++;
					System.IO.File.AppendAllText("CheckGrammarResults.txt", " ignored.\n");
					continue;
				}
				string path = Path.Combine(root, filename);
				Stopwatch watch = new Stopwatch();
				watch.Start();
				var document = Parser.Parse(path, format);
				watch.Stop();
				//TestJSONSerializer.DumpAsJSON(unit.CodeElementsDocumentSnapshot.CodeElements, filename);
				TimeSpan elapsed = watch.Elapsed;
				sum += elapsed;
				string formatted = String.Format("{0:00}m{1:00}s{2:000}ms", elapsed.Minutes, elapsed.Seconds, elapsed.Milliseconds);
				System.IO.File.AppendAllText("CheckGrammarResults.txt", (" parsed in " + formatted + "\n"));

				tested++;
				bool okay = true;
				if(hasErrors(document.Results.CodeElementsDocumentSnapshot)) {
					okay = false;
					parseErrors += checkErrors(filename, document.Results.CodeElementsDocumentSnapshot.ParserDiagnostics);
				}
				if(hasErrors(document.Results.ProgramClassDocumentSnapshot)) {
					okay = false;
					parseErrors += checkErrors(filename, document.Results.ProgramClassDocumentSnapshot.Diagnostics);
				}
				if (!okay) {
					nbFilesInError++;
					if (nbFilesInError >= STOP_AFTER_AS_MANY_ERRORS) break;
				}

				if (codegen) {
					var writer = new StringWriter();
					var generator = new TypeCobol.Codegen.Generator(writer, document.Results.TokensLines, null);
					var program = document.Results.ProgramClassDocumentSnapshot.Program;
					var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
					generator.Generate(program.SyntaxTree.Root, program.SymbolTable, columns);
					writer.Close();

					var expected = AsLines(File.ReadAllText(path, format.Encoding));
					var actual = AsLines(writer.ToString());
					var linesKO = new List<int>();
					for(int i=0; i<Math.Min(expected.Count, actual.Count); i++) {
						if (!expected[i].Equals(actual[i])) linesKO.Add(i);
					}
					var errors = new System.Text.StringBuilder();
					string fmt = Lines2FormatString(Math.Max(expected.Count, actual.Count));
					if (linesKO.Count > 0 || expected.Count != actual.Count) errors.AppendLine("--- Lines mismatch ---");
					int start = -1;
					for (int i=0; i<linesKO.Count; i++) {
						int currentline = linesKO[i];
						bool follows = i > 0 && linesKO[i-1] == currentline-1;
						if (!follows) {
							start = currentline;
							before(errors, expected, currentline,3, fmt);
						}
						bool preceeds = i+1 < linesKO.Count && linesKO[i+1] == currentline+1;
						if (!preceeds) {
							diff(errors, expected, actual, start, currentline, fmt);
							after(errors, expected, currentline,3, fmt);
							start = -1;
						}
					}
					for(int i=actual.Count; i<expected.Count; i++)
						errors.AppendLine(String.Format("-{0:"+fmt+"} {1}", i, expected[i]));
					for(int i=expected.Count; i<actual.Count; i++)
						errors.AppendLine(String.Format("+{0:"+fmt+"} {1}", i, actual[i]));
					if (errors.Length > 0) {
						codegenErrors += linesKO.Count + Math.Abs(actual.Count - expected.Count);
						System.IO.File.AppendAllText("CheckGrammarResults.txt", errors.ToString());
						if (okay) nbFilesInError++;
					}
				}
			}
			string total = String.Format("{0:00}m{1:00}s{2:000}ms", sum.Minutes, sum.Seconds, sum.Milliseconds);
			string message = "Files tested=" + tested + "/" + files.Length + ", files in error=" + nbFilesInError + ", ignored=" + ignores + "\n";
			if (parseErrors > 0)   message += "Parsing errors: "+ parseErrors   + '\n';
			if (codegenErrors > 0) message += "Codegen errors: "+ codegenErrors + '\n';
			message += "Total time: " + total;
			System.IO.File.AppendAllText("CheckGrammarResults.txt", message);
			if (nbFilesInError > 0) Assert.Fail('\n'+message);
		}

		private bool hasErrors(TypeCobol.Compiler.Parser.CodeElementsDocument document) {
			return document != null && document.ParserDiagnostics != null && document.ParserDiagnostics.Count() > 0;
		}
		private bool hasErrors(TypeCobol.Compiler.Parser.ProgramClassDocument document) {
			return document != null && document.Diagnostics != null && document.Diagnostics.Count() > 0;
		}
		private int checkErrors(string filename, IEnumerable<TypeCobol.Compiler.Diagnostics.Diagnostic> diagnostics) {
			Console.WriteLine(filename);
			string result = ParserUtils.DiagnosticsToString(diagnostics);
			Console.WriteLine(result);
			System.IO.File.AppendAllText("CheckGrammarResults.txt", (result + "\n"));
			return diagnostics.Count();
		}

		private List<string> AsLines(string text) {
			return text.Replace("\r\n","\n").Replace("\r","\n").Split(new char[]{'\n'}).ToList<string>();
		}
		private string Lines2FormatString(int lines) {
			string res = "0";
			for (int i=1; i<lines.ToString().Length; i++) res += "0";
			return res;
		}
		/// <param name="output"></param>
		/// <param name="input">Input text</param>
		/// <param name="index">Line index</param>
		/// <param name="before">Number of line before line index</param>
		/// <param name="fmt"></param>
		private void before(System.Text.StringBuilder output, List<string> input, int index, int before, string fmt="0") {
			for(int line=index-before; line<index; line++)
				if (line > 0) output.AppendLine(String.Format(" {0:"+fmt+"} {1}", line, input[line]));
		}
		private void diff(System.Text.StringBuilder output, List<string> expected, List<string> actual, int start, int end, string fmt="0") {
			for (int i=start; i<=end; i++)
				output.AppendLine(String.Format("-{0:"+fmt+"} {1}", i, expected[i]));
			for (int i=start; i<=end; i++)
				output.AppendLine(String.Format("+{0:"+fmt+"} {1}", i, actual[i]));
		}
		/// <param name="output"></param>
		/// <param name="input">Input text</param>
		/// <param name="index">Line index</param>
		/// <param name="after">Number of line after line index</param>
		/// <param name="fmt"></param>
		private void after(System.Text.StringBuilder output, List<string> input, int index, int after, string fmt="0") {
			for(int line=index+1; line<=index+after; line++)
				if (line < input.Count) output.AppendLine(String.Format(" {0:"+fmt+"} {1}", line, input[line]));
		}
	}
}
