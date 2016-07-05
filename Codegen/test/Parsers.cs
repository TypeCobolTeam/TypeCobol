using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Config;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler; // DocumentFormat
using TypeCobol.Tools; // CodeElementDiagnostics

namespace TypeCobol.Codegen {

	[TestClass]
	public class Parsers {

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void CreateDefaultParser() {
			System.Type DEFAULT = typeof(XmlParser);
			ConfigParser parser;
			parser = Config.Config.CreateParser("");
			Assert.AreEqual(parser.GetType(), DEFAULT);
			parser = Config.Config.CreateParser(".unexistentextension");
			Assert.AreEqual(parser.GetType(), DEFAULT);
		}

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void CreateXMLParser() {
			var parser = Config.Config.CreateParser(".xml");
			Assert.AreEqual(parser.GetType(), typeof(XmlParser));
		}

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseFailureIfNotFound() {
			try {
				ParseConfig("NOT_FOUND");
				throw new System.Exception("Expected FileNotFoundException");
			} catch(FileNotFoundException) { } // Test OK
		}

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseEmpty() {
			Assert.AreEqual(ParseConfig("Empty.xml").Count,0);
		}

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseTypes() {
			string file = "Types";
			var skeletons = ParseConfig(file+".xml");
			Assert.AreEqual(skeletons.Count,2);
			Assert.AreEqual(skeletons[0].Patterns.Count, 1);
			Assert.AreEqual(skeletons[1].Patterns.Count, 1);

			ParseGenerateCompare(file+".cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseBooleans() {
			string file = Path.Combine("TypeCobol","TypeBOOL");
			var skeletons = ParseConfig(file+".xml");
			Assert.AreEqual(skeletons.Count,2);
			Assert.AreEqual(skeletons[0].Patterns.Count, 1);
			Assert.AreEqual(skeletons[1].Patterns.Count, 1);

			ParseGenerateCompare(file+".cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseFunctions() {
			string file = Path.Combine("TypeCobol","FUNCTION");
			var skeletons = ParseConfig(file+".xml");
			ParseGenerateCompare(file+".cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseFunctionsDeclaration() {
			string file = Path.Combine("TypeCobol","FunDeclare");
			var skeletons = ParseConfig(file+".xml");
			ParseGenerateCompare(file+".cbl", skeletons);
		}





		private static string ROOT = "resources";
		private static string CONFIG = "config";
		private static string INPUT  = "input";
		private static string OUTPUT = "output";

		private List<Skeleton> ParseConfig(string resource) {
			var path = Path.Combine(ROOT, CONFIG, resource);
			var parser = Config.Config.CreateParser(Path.GetExtension(path));
			return parser.Parse(path);
		}

		private void ParseGenerateCompare(string path, List<Skeleton> skeletons) {
			var format = DocumentFormat.RDZReferenceFormat;
			var document = Parser.Parse(Path.Combine(ROOT, INPUT, path), format);
			var columns = document.Results.ProgramClassDocumentSnapshot.TextSourceInfo.ColumnsLayout;
			var writer = new StringWriter();
			// write parsing errors
			WriteErrors(writer, document.Errors[0], "CodeElements", columns);
			WriteErrors(writer, document.Errors[1], "ProgramClass", columns);
			// write generated code
			var codegen = new Generator(writer, document.Results.TokensLines, skeletons);
			var program = document.Results.ProgramClassDocumentSnapshot.Program;

			codegen.Generate(program.SyntaxTree.Root, program.SymbolTable, columns);
			// flush
			writer.Close();

			// compare with expected result
			string expected = File.ReadAllText(Path.Combine(ROOT, OUTPUT, path), format.Encoding);
			Assert.AreEqual(ReplaceLineBreaks(expected), ReplaceLineBreaks(writer.ToString()));
		}

		private string ReplaceLineBreaks(string text) {
			return text.Replace("\r\n","\n").Replace("\r","\n");
		}

		private void WriteErrors(TextWriter writer, ICollection<Diagnostic> errors, string type, Compiler.Text.ColumnsLayout columns) {
			string comment = GetComment(columns);
			if (errors.Count > 0) {
				writer.WriteLine(comment+" "+errors.Count+" "+type+" errors");
				foreach(var error in errors)
					writer.WriteLine(comment+" "+error);
			}
		}
		private string GetComment(Compiler.Text.ColumnsLayout columns) {
			if(columns == Compiler.Text.ColumnsLayout.CobolReferenceFormat)
				return "      *";
			return "*";
		}
	}
}
