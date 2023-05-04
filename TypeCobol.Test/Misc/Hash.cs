using Microsoft.VisualStudio.TestTools.UnitTesting;
using TypeCobol.Tools;

namespace TypeCobol.Test.Misc {
    [TestClass]
public class Hashes {
	[TestMethod]
	public void TestStringHashes() {
		Assert.AreEqual(Hash.CreateSHA256("Hello, world"),        "4ae7c3b6ac0beff671efa8cf57386151c06e58ca53a78d83f36107316cec125f");
		Assert.AreEqual(Hash.CreateSHA256("titi"),                "cce66316b4c1c59df94a35afb80cecd82d1a8d91b554022557e115f5c275f515");
		Assert.AreEqual(Hash.CreateSHA256("toto"),                "31f7a65e315586ac198bd798b6629ce4903d0899476d5741a9f32e2e521b6a66");
		Assert.AreEqual(Hash.CreateSHA256("totot"),               "ca0b4c052e85660d1f3a274c88fb9210db132f7a3424fdf209d1074ae7669d20");
		string message = "NO NOO̼O​O\n"+"   NΘ \n"+"stop the an​*̶͑̾̾​̅ͫ͏̙̤g͇̫͛͆̾ͫ̑͆l͖͉̗̩̳̟̍ͫͥͨe̠̅s";
		Assert.AreEqual(Hash.CreateSHA256(message),               "82b7b70cb1b423246cf480501d261dde41277829fd46ee0d5c08b3e2150e6824");
		Assert.AreEqual(Hash.CreateSHA256("TH̘Ë͖́̉ ͠P̯͍̭O̚​N̐Y̡ H̸̡̪̯ͨ͊̽̅̾̎Ȩ̬̩̾͛ͪ̈́̀́͘ ̶̧̨̱̹̭̯ͧ̾ͬC̷̙̲̝͖ͭ̏ͥͮ͟Oͮ͏̮̪̝͍M̲̖͊̒ͪͩͬ̚̚͜Ȇ̴̟̟͙̞ͩ͌͝S̨̥̫͎̭ͯ̿̔̀ͅ"), "4e60680be7e0c3e2629cae1828ab07089e3a95273753e6c5b38a3f40fcdb5070");
	}
	[TestMethod]
	public void TestStringHashesCOBOL() {
		Assert.AreEqual(Hash.CreateCOBOLNameHash("Hello, world", 14),       "ca7e4eaa6e8ae9");
		Assert.AreEqual(Hash.CreateCOBOLNameHash("titi"),                   "cce66316");
		Assert.AreEqual(Hash.CreateCOBOLNameHash("toto", 8),                "f7a65e31");
		Assert.AreEqual(Hash.CreateCOBOLNameHash("totot", 8),               "ca0b4c05");
		Assert.AreEqual(Hash.CreateCOBOLNameHash("whatever", 0),            "");
		string message = "NO NOO̼O​O\n"+"   NΘ \n"+"stop the an​*̶͑̾̾​̅ͫ͏̙̤g͇̫͛͆̾ͫ̑͆l͖͉̗̩̳̟̍ͫͥͨe̠̅s";
		Assert.AreEqual(Hash.CreateCOBOLNameHash(message, 8),               "d2ea3387");
		Assert.AreEqual(Hash.CreateCOBOLNameHash("TH̘Ë͖́̉ ͠P̯͍̭O̚​N̐Y̡ H̸̡̪̯ͨ͊̽̅̾̎Ȩ̬̩̾͛ͪ̈́̀́͘ ̶̧̨̱̹̭̯ͧ̾ͬC̷̙̲̝͖ͭ̏ͥͮ͟Oͮ͏̮̪̝͍M̲̖͊̒ͪͩͬ̚̚͜Ȇ̴̟̟͙̞ͩ͌͝S̨̥̫͎̭ͯ̿̔̀ͅ", 1), "e");
	}
}

[TestClass]
public class HumanReadableUID {
	[TestMethod]
	public void TestUIDStoreConstruction() {
		UIDStore lut;
		lut = new UIDStore();
		Assert.AreEqual(lut.MaxSize, UIDStore.DEFAULT_MAX_SIZE);
		Assert.AreEqual(lut.MaxItems, 99);
		Assert.AreEqual(lut.TruncatedSize, 27);
		lut = new UIDStore(20, 9);
		Assert.AreEqual(lut.MaxSize, 20);
		Assert.AreEqual(lut.MaxItems, 9);
		Assert.AreEqual(lut.TruncatedSize, 18);
		lut = new UIDStore(20,10);
		Assert.AreEqual(lut.MaxSize, 20);
		Assert.AreEqual(lut.MaxItems, 10);
		Assert.AreEqual(lut.TruncatedSize, 17);
		lut = new UIDStore(100,999);
		Assert.AreEqual(lut.MaxSize, 100);
		Assert.AreEqual(lut.MaxItems, 999);
		Assert.AreEqual(lut.TruncatedSize, 96);
	}
	[TestMethod]
	public void TestUIDStore() {
		var lut = new UIDStore();

		Assert.IsNull(lut.FromGenerated("titi-01"));
		Assert.IsNull(lut.FromGenerated("toto-01"));
		Assert.IsNull(lut.FromGenerated("0123456789012345678901234567-01"));

		Assert.AreEqual(lut.FromOriginal("titi"), "titi-01");
		Assert.AreEqual(lut.FromOriginal("toto"), "toto-01");
		Assert.AreEqual(lut.FromOriginal("titi"), "titi-02");
		Assert.AreEqual(lut.FromOriginal("123456789012345678901234567890"), "123456789012345678901234567-01");
		Assert.AreEqual(lut.FromOriginal("123456789012345678901234567890"), "123456789012345678901234567-02");

		Assert.AreEqual(lut.FromGenerated("titi-01"), "titi");
		Assert.AreEqual(lut.FromGenerated("titi-02"), "titi");
		Assert.IsNull(lut.FromGenerated("titi-03"));
		Assert.AreEqual(lut.FromGenerated("toto-01"), "toto");
		Assert.IsNull(lut.FromGenerated("toto-02"));
		Assert.AreEqual(lut.FromGenerated("123456789012345678901234567-01"), "123456789012345678901234567890");
		Assert.AreEqual(lut.FromGenerated("123456789012345678901234567-02"), "123456789012345678901234567890");
		Assert.IsNull(lut.FromGenerated("123456789012345678901234567-03"));
	}
}

}
