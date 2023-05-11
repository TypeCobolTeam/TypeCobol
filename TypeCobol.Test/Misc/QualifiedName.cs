using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Compiler.CodeElements.Expressions;

namespace TypeCobol.Test.Misc {

    [TestClass]
    public class QualifiedNames {

        [TestMethod]
        [TestCategory("SymbolTable")]
        [TestProperty("Time","fast")]
        public void QualifiedLength1DataName() {
            var id = qname("x");
            Assert.AreEqual(id.ToString(), "x");
            Assert.AreEqual(id.Count, 1);
            Assert.AreEqual(id[0], "x");
            try{ var x = id[1]; throw new AssertFailedException(); }
            catch(System.ArgumentOutOfRangeException) { }
            Assert.AreEqual(id.IndexOf("x"),    0);
            Assert.AreEqual(id.IndexOf("xin"), -1);
            Assert.IsTrue(id.Contains("x"));
            Assert.IsFalse(id.Contains("xin"));
        }
        [TestMethod]
        [TestCategory("SymbolTable")]
        [TestProperty("Time","fast")]
        public void QualifiedLength2DataName() {
            var id = qname("group.name", false);
            Assert.AreEqual(id.ToString(), "group.name");
            Assert.AreEqual(id.Count, 2);
            Assert.AreEqual(id[0], "group");
            Assert.AreEqual(id[1], "name");
            try{ var x = id[2]; throw new AssertFailedException(); }
            catch(System.ArgumentOutOfRangeException) { }
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
        [TestCategory("SymbolTable")]
        [TestProperty("Time","fast")]
        public void QualifiedLength2FileName() {
            var id = qname("file.name", true);
            Assert.AreEqual(id.ToString(), "file.name");
            Assert.AreEqual(id.Count, 2);
            Assert.AreEqual(id[0], "file");
            Assert.AreEqual(id[1], "name");
            try{ var x = id[2]; throw new AssertFailedException(); }
            catch(System.ArgumentOutOfRangeException) { }
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
        [TestCategory("SymbolTable")]
        [TestProperty("Time","fast")]
        public void QualifiedLength5DataName() {
            var id = qname("group.aaa.bb.c.name", false);
            Assert.AreEqual(id.ToString(), "group.aaa.bb.c.name");
            Assert.AreEqual(id.Count, 5);
            Assert.AreEqual(id[0], "group");
            Assert.AreEqual(id[1], "aaa");
            Assert.AreEqual(id[2], "bb");
            Assert.AreEqual(id[3], "c");
            Assert.AreEqual(id[4], "name");
            try{ var x = id[5]; throw new AssertFailedException(); }
            catch(System.ArgumentOutOfRangeException) { }
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
        [TestCategory("SymbolTable")]
        [TestProperty("Time","fast")]
        public void QualifiedLength5FileName() {
            var id = qname("file.aaa.bb.c.name", true);
            Assert.AreEqual(id.ToString(), "file.aaa.bb.c.name");
            Assert.AreEqual(id.Count, 5);
            Assert.AreEqual(id[0], "file");
            Assert.AreEqual(id[1], "aaa");
            Assert.AreEqual(id[2], "bb");
            Assert.AreEqual(id[3], "c");
            Assert.AreEqual(id[4], "name");
            try{ var x = id[5]; throw new AssertFailedException(); }
            catch(System.ArgumentOutOfRangeException) { }
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



        private static QualifiedName qname(string uri, bool isFirstFilename = false) {
            return new URI(uri);
        }
    }
}
