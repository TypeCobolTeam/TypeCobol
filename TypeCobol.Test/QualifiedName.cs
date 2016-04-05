using Microsoft.VisualStudio.TestTools.UnitTesting;
using Moq;
using System.Collections.Generic;
using TypeCobol.Compiler.Scanner;

namespace TypeCobol.Compiler.CodeElements.Expressions {

	[TestClass]
	public class QualifiedNameTest {

		[TestMethod]
		public void QualifiedName1() {
			var id = qname("x");
			Assert.AreEqual(id.Symbol.Name, "x");
			Assert.AreEqual(id.DataNames.Count, 0);
			Assert.IsNull(id.FileName);
			Assert.AreEqual(id.ToString(), "x");
			Assert.AreEqual(id.Count, 1);
			Assert.AreEqual(id[0], "x");
			try{ var x = id[1]; throw new AssertFailedException(); }
			catch(System.ArgumentOutOfRangeException ex) { }
			Assert.AreEqual(id.IndexOf("x"),    0);
			Assert.AreEqual(id.IndexOf("xin"), -1);
			Assert.IsTrue(id.Contains("x"));
			Assert.IsFalse(id.Contains("xin"));
		}
		[TestMethod]
		public void QualifiedName2() {
			var id = qname("group.name", false);
			Assert.AreEqual(id.Symbol.Name, "name");
			Assert.AreEqual(id.DataNames.Count, 1);
			Assert.AreEqual(id.DataNames[0].Name, "group");
			Assert.IsNull(id.FileName);
			Assert.AreEqual(id.ToString(), "group.name");
			Assert.AreEqual(id.Count, 2);
			Assert.AreEqual(id[0], "group");
			Assert.AreEqual(id[1], "name");
			try{ var x = id[2]; throw new AssertFailedException(); }
			catch(System.ArgumentOutOfRangeException ex) { }
			Assert.AreEqual(id.IndexOf("group"), 0);
			Assert.AreEqual(id.IndexOf("name"),  1);
			Assert.AreEqual(id.IndexOf("x"),    -1);
			Assert.AreEqual(id.IndexOf("group.name"), -1);
			Assert.IsTrue(id.Contains("group"));
			Assert.IsTrue(id.Contains("name"));
			Assert.IsFalse(id.Contains("x"));
			Assert.IsFalse(id.Contains("group.name"));
		}
		[TestMethod]
		public void QualifiedName2WithFilename() {
			var id = qname("file.name", true);
			Assert.AreEqual(id.Symbol.Name, "name");
			Assert.AreEqual(id.DataNames.Count, 0);
			Assert.AreEqual(id.FileName.Name, "file");
			Assert.AreEqual(id.ToString(), "file.name");
			Assert.AreEqual(id.Count, 2);
			Assert.AreEqual(id[0], "file");
			Assert.AreEqual(id[1], "name");
			try{ var x = id[2]; throw new AssertFailedException(); }
			catch(System.ArgumentOutOfRangeException ex) { }
			Assert.AreEqual(id.IndexOf("file"), 0);
			Assert.AreEqual(id.IndexOf("name"), 1);
			Assert.AreEqual(id.IndexOf("x"),   -1);
			Assert.AreEqual(id.IndexOf("file.name"), -1);
			Assert.IsTrue(id.Contains("file"));
			Assert.IsTrue(id.Contains("name"));
			Assert.IsFalse(id.Contains("x"));
			Assert.IsFalse(id.Contains("file.name"));
		}
		[TestMethod]
		public void QualifiedName5() {
			var id = qname("group.aaa.bb.c.name", false);
			Assert.AreEqual(id.Symbol.Name, "name");
			Assert.AreEqual(id.DataNames.Count, 4);
			Assert.AreEqual(id.DataNames[0].Name, "group");
			Assert.AreEqual(id.DataNames[1].Name, "aaa");
			Assert.AreEqual(id.DataNames[2].Name, "bb");
			Assert.AreEqual(id.DataNames[3].Name, "c");
			Assert.IsNull(id.FileName);
			Assert.AreEqual(id.ToString(), "group.aaa.bb.c.name");
			Assert.AreEqual(id.Count, 5);
			Assert.AreEqual(id[0], "group");
			Assert.AreEqual(id[1], "aaa");
			Assert.AreEqual(id[2], "bb");
			Assert.AreEqual(id[3], "c");
			Assert.AreEqual(id[4], "name");
			try{ var x = id[5]; throw new AssertFailedException(); }
			catch(System.ArgumentOutOfRangeException ex) { }
			Assert.AreEqual(id.IndexOf("group"), 0);
			Assert.AreEqual(id.IndexOf("aaa"),   1);
			Assert.AreEqual(id.IndexOf("bb"),    2);
			Assert.AreEqual(id.IndexOf("c"),     3);
			Assert.AreEqual(id.IndexOf("name"),  4);
			Assert.AreEqual(id.IndexOf("x"),    -1);
			Assert.AreEqual(id.IndexOf("group.aaa.bb.c.name"), -1);
			Assert.IsTrue(id.Contains("group"));
			Assert.IsTrue(id.Contains("aaa"));
			Assert.IsTrue(id.Contains("bb"));
			Assert.IsTrue(id.Contains("c"));
			Assert.IsTrue(id.Contains("name"));
			Assert.IsFalse(id.Contains("x"));
			Assert.IsFalse(id.Contains("group.aaa.bb.c.name"));
		}
		[TestMethod]
		public void QualifiedName5WithFilename() {
			var id = qname("file.aaa.bb.c.name", true);
			Assert.AreEqual(id.Symbol.Name, "name");
			Assert.AreEqual(id.DataNames.Count, 3);
			Assert.AreEqual(id.DataNames[0].Name, "aaa");
			Assert.AreEqual(id.DataNames[1].Name, "bb");
			Assert.AreEqual(id.DataNames[2].Name, "c");
			Assert.AreEqual(id.FileName.Name, "file");
			Assert.AreEqual(id.ToString(), "file.aaa.bb.c.name");
			Assert.AreEqual(id.Count, 5);
			Assert.AreEqual(id[0], "file");
			Assert.AreEqual(id[1], "aaa");
			Assert.AreEqual(id[2], "bb");
			Assert.AreEqual(id[3], "c");
			Assert.AreEqual(id[4], "name");
			try{ var x = id[5]; throw new AssertFailedException(); }
			catch(System.ArgumentOutOfRangeException ex) { }
			Assert.AreEqual(id.IndexOf("file"), 0);
			Assert.AreEqual(id.IndexOf("aaa"),  1);
			Assert.AreEqual(id.IndexOf("bb"),   2);
			Assert.AreEqual(id.IndexOf("c"),    3);
			Assert.AreEqual(id.IndexOf("name"), 4);
			Assert.AreEqual(id.IndexOf("x"),   -1);
			Assert.AreEqual(id.IndexOf("file.aaa.bb.c.name"), -1);
			Assert.IsTrue(id.Contains("file"));
			Assert.IsTrue(id.Contains("aaa"));
			Assert.IsTrue(id.Contains("bb"));
			Assert.IsTrue(id.Contains("c"));
			Assert.IsTrue(id.Contains("name"));
			Assert.IsFalse(id.Contains("x"));
			Assert.IsFalse(id.Contains("file.aaa.bb.c.name"));
		}



		private QualifiedName qname(string uri, bool isFirstFilename = false) {
			var parts = uri.Split('.');
			int p = 0;
			FileName filename = null;
			if (isFirstFilename) filename = new FileName(token(parts[p++]));
			var datanames = new List<DataName>();
			while(p < parts.Length-1) datanames.Add(new DataName(token(parts[p++])));
			var symbol = new DataName(token(parts[parts.Length-1]));
			return new QualifiedName(symbol, datanames, filename);
		}
		private Token token(string word) {
			var token = new Mock<Token>();
			token.Setup(foo => foo.Text).Returns(word);
//			token.Setup(foo => foo.TokenType).Returns(TokenType.UserDefinedWord);
			token.Setup(foo => foo.TokenFamily).Returns(TokenFamily.Symbol);
			return token.Object;
		}
	}
}
