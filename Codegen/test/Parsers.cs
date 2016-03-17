using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Config;
using TypeCobol.Codegen.Skeletons;

namespace TypeCobol.Codegen {

	[TestClass]
	public class Parsers {

		private static string ROOT = "resource";

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

		private List<Skeleton> doParse(string resource) {
			var path = Path.Combine(ROOT, resource);
			var parser = Config.Config.CreateParser(Path.GetExtension(path));
			return parser.Parse(path);
		}

		[TestMethod]
		public void ParseEmpty() {
			Assert.AreEqual(doParse("Empty.xml").Count,0);
		}
	}
}
