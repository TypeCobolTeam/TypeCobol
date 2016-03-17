using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Config;
using TypeCobol.Codegen.Skeletons;

using TypeCobol.Compiler;// DocumentFormat
using TypeCobol.Compiler.Parser; // ProgramClassDocument
using TypeCobol.Server; // Parser

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
			Assert.AreEqual(skeletons.Count,1);
			Assert.AreEqual(skeletons[0].Patterns.Count,1);
			var pattern = skeletons[0].Patterns[0];
			var document = ParseSource("Types.cbl");
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
		internal static ProgramClassDocument ParseSource(string path, DocumentFormat format=null) {
			path = Path.Combine(ROOT, INPUT, path);
			if (format == null) format = DocumentFormat.RDZReferenceFormat;
			var parser = new Parser("TypeCobol.Server");
			parser.Init(path, format);
			parser.Parse(path);
			return parser.Snapshot;
		}
	}
}
