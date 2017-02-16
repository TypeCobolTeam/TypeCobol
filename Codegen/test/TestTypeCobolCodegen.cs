﻿using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Collections.Generic;
using System.IO;
using TypeCobol.Codegen.Config;
using TypeCobol.Codegen.Skeletons;
using TypeCobol.Compiler; // DocumentFormat
using TypeCobol.Tools; // CodeElementDiagnostics


namespace TypeCobol.Codegen {

	[TestClass]
	public class TestTypeCobolCodegen {

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
				CodegenTestUtils.ParseConfig("NOT_FOUND");
				throw new System.Exception("Expected FileNotFoundException");
			} catch(FileNotFoundException) { } // Test OK
		}

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseEmpty() {
			Assert.AreEqual(CodegenTestUtils.ParseConfig("Empty.xml").Count,0);
		}

		[TestMethod]
		[TestCategory("Config")]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseTypes() {
			string file = Path.Combine("TypeCobol", "Types");
            var skeletons = CodegenTestUtils.ParseConfig("Types.xml");
			Assert.AreEqual(skeletons.Count,3);
			Assert.AreEqual(skeletons[0].Patterns.Count, 1);
			Assert.AreEqual(skeletons[1].Patterns.Count, 1);
			Assert.AreEqual(skeletons[2].Patterns.Count, 1);

			CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestCategory("Parsing")]
		[TestProperty("Time","fast")]
		public void ParseBooleans() {
			string file = Path.Combine("TypeCobol","TypeBOOL");
			var skeletons = CodegenTestUtils.ParseConfig(file+".xml");
			Assert.AreEqual(skeletons.Count,2);
			Assert.AreEqual(skeletons[0].Patterns.Count, 1);
			Assert.AreEqual(skeletons[1].Patterns.Count, 1);

			CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseUnsafe() {
			string file = Path.Combine("TypeCobol","unsafe");
			var skeletons = CodegenTestUtils.ParseConfig("Types.xml");// CodegenTestUtils.ParseConfig(file+".xml");
			CodegenTestUtils.ParseGenerateCompare(file+".rdz.cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseQualifiedNames() {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","QualifiedNames")+".rdz.cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseFunctions() {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","Function")+".rdz.cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseFunctionsDeclaration() {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");

            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "FunDeclare") + ".rdz.cbl", skeletons);
        }

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseLibrary() {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","Library")+".rdz.cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseProcedureCall() {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","ProcedureCall")+".rdz.cbl", skeletons);
		}

		[TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void Codegen() {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","Codegen")+".rdz.cbl", skeletons);
		}

		[TestMethod][Ignore]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
		public void ParseSuccessiveCalls() {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");
			CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol","SuccessiveCalls")+".rdz.cbl", skeletons);
		}

        [TestMethod]
		[TestCategory("Codegen")]
		[TestProperty("Time","fast")]
        public void ParseTypeDefLevel88()
        {
			var skeletons = CodegenTestUtils.ParseConfig(Path.Combine("TypeCobol","skeletons")+".xml");
            CodegenTestUtils.ParseGenerateCompare(Path.Combine("TypeCobol", "TypeDefLevel88") + ".rdz.cbl", skeletons);
		}        		
	}
}
