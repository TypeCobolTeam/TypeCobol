using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Config;
using TypeCobol.Codegen.Skeletons;

using TypeCobol.Compiler; // DocumentFormat
using TypeCobol.Compiler.Parser; // ProgramClassDocument
using TypeCobol.Server; // Parser
using TypeCobol.Tools; // CodeElementDiagnostics

namespace TypeCobol.Codegen {

	[TestClass]
	public class Parsers {

		[TestMethod]
		public void CreateDefaultParser() {
			System.Type DEFAULT = typeof(XmlParser);
			ConfigParser parser;
			parser = Config.Config.CreateParser("");
			Assert.AreEqual(parser.GetType(), DEFAULT);
			parser = Config.Config.CreateParser(".unexistentextension");
			Assert.AreEqual(parser.GetType(), DEFAULT);
		}

		[TestMethod]
		public void CreateXMLParser() {
			var parser = Config.Config.CreateParser(".xml");
			Assert.AreEqual(parser.GetType(), typeof(XmlParser));
		}

		[TestMethod]
		public void ParseFailureIfNotFound() {
			try {
				ParseConfig("NOT_FOUND");
				throw new System.Exception("Expected FileNotFoundException");
			} catch(FileNotFoundException) { } // Test OK
		}

		[TestMethod]
		public void ParseEmpty() {
			Assert.AreEqual(ParseConfig("Empty.xml").Count,0);
		}

		[TestMethod]
		public void ParseTypes() {
			var skeletons = ParseConfig("Types.xml");
			Assert.AreEqual(skeletons.Count,2);
			Assert.AreEqual(skeletons[0].Patterns.Count, 1);
			Assert.AreEqual(skeletons[1].Patterns.Count, 1);
			var document = ParseSource("Types.cbl", DocumentFormat.RDZReferenceFormat);

			var columns = document.Snapshot.TextSourceInfo.ColumnsLayout;
			var writer = new System.IO.StringWriter();
			// write parsing errors
			WriteErrors(writer, document.Errors[0], "CodeElements", columns);
			WriteErrors(writer, document.Errors[1], "ProgramClass", columns);
			// write generated code
			var codegen = new Generator(writer, document.Source);
			codegen.Generate(document.Snapshot.Program.SyntaxTree.Root, columns);
			// flush
			writer.Close();

			// compare with expected result
			string expected = File.ReadAllText(Path.Combine(ROOT, OUTPUT, "Types.fixme"));
			Assert.AreEqual(writer.ToString(), expected);
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
		internal static Parser ParseSource(string path, DocumentFormat format) {
			path = Path.Combine(ROOT, INPUT, path);
			var parser = new Parser("Codegen.Test");
			parser.Init(path, format);
			parser.Parse(path);
			return parser;
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
